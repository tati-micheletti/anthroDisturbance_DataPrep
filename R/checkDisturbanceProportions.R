checkDisturbanceProportions <- function(DT, 
                                        destinationPath, 
                                        studyArea,
                                        rasterToMatch){
  # 1. Subset what is NOT potential
  rowsToRemove <- grepl(pattern = "potential", x = DT[["dataClass"]])
  studyAreaTotalArea <- terra::expanse(studyArea, transform = FALSE, unit = "km")
  rasterToMatchR <- raster(rasterToMatch)
  # NOTES:
  # NorthwestTerritories_15m_Disturb_Perturb_XXXX.shp: available at NWT ArcGIS hub, but 
  # there is no direct download. That's why it is hosted on GDrive
  # https://hub.arcgis.com/datasets/545f922fff514b298a64d6791296289d/api?layer=114
  #
  # XXXXXDisturbances_NT1_BCR6_clipped.shp: Layer prepared by me? Even if so, (highly) likely clipped from 
  # James' layer! (Seen the metadata from the linearDisturbances_NT1_BCR6_clipped.shp.xml, it matches the 
  # "BEADlines name)
  
  # 2. Subset unique datasets + add 2015 layer from James 30m
  cleanDT <- unique(DT[!rowsToRemove, c("fileName", "dataType", "URL")])
  cleanDT <- rbind(cleanDT, 
                       data.table(fileName = c("BEADlines2015_NWT_corrected_to_NT1_2016.shp",
                                               "BEADpolys2015_NWT_corrected_to_NT1_2016.shp"), 
                                  dataType = c("shapefile",
                                               "shapefile"), 
                                  URL = c("https://drive.google.com/file/d/1sxAa0wwwt7iwiHD7zB0DDnjfqyIQjKI2",
                                          "https://drive.google.com/file/d/1sxAa0wwwt7iwiHD7zB0DDnjfqyIQjKI2")))
  cleanDT[,"resolution" := c(NA, 15, 30, NA, NA, NA, 15, NA, 30, NA)]
  # 3. Download and postprocess to study area, and aggregate
  allDTs <- lapply(1:NROW(cleanDT), function(INDEX){
    layName <- paste0(justName(cleanDT$fileName[INDEX]), "_500m")
    flNm <- file.path(destinationPath, layName)
    fullFlnmSHP <- file.path(flNm, paste0(basename(flNm), ".shp"))
    fullFlnmTIF <- file.path(flNm, paste0(basename(flNm), ".tif"))
    if (!file.exists(fullFlnmSHP)){
    lay <- terra::aggregate(prepInputs(url = cleanDT[["URL"]][INDEX], 
                      targetFile = cleanDT[["fileName"]][INDEX], 
                      alsoExtract = "similar",
                      studyArea = studyArea,
                      rasterToMatch = rasterToMatch,
                      destinationPath = destinationPath, 
                      fun = "terra::vect", 
                      overwrite = FALSE), dissolve = TRUE)
    
    # 5. Buffer and calculate area disturbed

    whichClass <- class(lay)
    if (whichClass %in% c("SpatRaster", "RasterLayer")){
      if (sum(lay[], na.rm = TRUE) == 0){ # Works for raster 
        message(paste0("Layer ", layName, " seems empty. Returning NULL."))
        return(NULL) 
      }
      if (is(lay, "RasterLayer"))
        lay <- rast(lay)
      message(paste0("Polygonizing raster ", layName))
      lay <- as.polygons(lay, values=TRUE, na.rm=TRUE, digits=5)
      # Need to remove the study area polygon (value 0)
      lay <- terra::subset(x = lay, subset = lay[[names(lay)]] == 1)
    }
    if (NROW(lay) == 0){ # Works for vectors 
      message(paste0("Layer ", layName, " seems empty. Returning NULL."))
      return(NULL) 
    }
    message(paste0("Buffering vector ", layName))
    
    # terra::aggregate can be parallelized.
    # And maybe vectors for buffering can be split.
    # Anyway, my most time consuming one takes about 2hs, so 
    # ideas are here just in case this changes
    # library(parallel)
    # cls <- parallel::makeCluster(2)
    # ## Export the vector as sf
    # parallel::clusterExport(cls, c("rfm", "rfun", "randomForest"))
    # rp3 <- predict(logo, rfm, fun=rfun)
    # parallel::stopCluster(cls)
    
    tic(paste0("Time elapsed for buffering ", layName, ":"))
    bLay <- terra::buffer(lay, width = 500)
    toc()
    terra::writeVector(bLay, filename = flNm,
                       filetype = "ESRI Shapefile")
    } else {
    bLay <- vect(fullFlnmSHP)
  }
    bLay$totalAreaKm2 <- terra::expanse(bLay, transform = FALSE, unit = "km")
    message(crayon::green(paste0("Total area disturbed for ", layName, " as vector: ", round(bLay$totalAreaKm2, digits = 3), "km2")))
    message(paste0("This represents ", round(100*(bLay$totalAreaKm2/studyAreaTotalArea), 4), "% of total area."))
    if (!file.exists(fullFlnmTIF)){
    bLaySF <- sf::st_as_sf(bLay)
    bLayRas <- fasterize::fasterize(sf = bLaySF, raster = rasterToMatchR)
    names(bLayRas) <- layName
    terra::writeRaster(bLayRas, filename = fullFlnmTIF,
                       filetype = "GTiff")
    } else {
      bLayRas <- rast(fullFlnmTIF)
    }
    distTb <- table(bLayRas[])
    totDistKm <- distTb["1"] * 0.0625
    message(crayon::green(paste0("Total area disturbed for ", layName,
                                 " as raster: ", totDistKm)))
    return(list(bufferedRaster = bLayRas,
                bufferedVector = bLay))
    
  })
  names(allDTs) <- justName(cleanDT[["fileName"]])
  
  # 6. Put them slowly together (use 2 canonicals?), and calculate total area disturbed 
  # NOTE: NRN_NT_13_0_ROADSEG.shp, 
  #       MiningLeases.shp, 
  #       wind_turbine_database_en.gdb.zip and # This one is zero in the studyarea!
  #       CutblocksHistory_UpdatedJan2022 need to be individually checked!
  # Convert all pixels that are disturbed to 1, and non-disturbed to 0
  bufferedAnthropogenicDisturbance500m <- rasterToMatch
  bufferedAnthropogenicDisturbance500m[!is.na(bufferedAnthropogenicDisturbance500m[])] <- 0
  names(bufferedAnthropogenicDisturbance500m) <- paste0("totalDisturbance_", "IC")
  
  # 1. Extract all bufferedRasters
  # 2. Run them below/
  # 3. If disturbances make sense, consider using somehow this workflow to generate disturbances!  
  allBuffered <- lapply(allDTs,"[[", "bufferedRaster")
  allBuffered <- allBuffered[!unlist(lapply(allBuffered, is.null))]
  
  for (i in 1:length(allBuffered)){
    bufferedAnthropogenicDisturbance500m[which(allBuffered[[i]][] == 1)] <- 1
  }
  distTable <- table(bufferedAnthropogenicDisturbance500m[])
  totDistKm <- distTable["1"]*0.0625
  percDist <- round(100*(distTable["1"]/(distTable["0"]+distTable["1"])), 2)
  message(paste0("Buffered disturbances for all disturbances. Total area disturbed: ", 
                 totDistKm, "km2 -- ", percDist, "% of the total area (", 
                 0.0625*(distTable["0"]+distTable["1"]),"km2)"))
}

justName <- function(x) sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x))
