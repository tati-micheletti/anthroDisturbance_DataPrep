createDisturbanceList <- function(DT, 
                        destinationPath, 
                        studyArea, 
                        rasterToMatch,
                        skipFixErrors){
  # lapply over dataName, then over dataClass, and then over the classToSearch 
  # 1. Download data, place it in module's data folder
  # 2. Make the layer, reproj to RTM, crop to SA
  # 3. Out the classToSearch lapply and merge/union the layers to a common dataClass
  # 4. Return all layers without other union
  
  disturbanceList <- lapply(unique(DT[["dataName"]]), function(DN){
    dataClassDT <- lapply(unique(DT[["dataClass"]]), function(DC){
      classSearchDT <- lapply(unique(DT[["classToSearch"]]), function(CTS){
        subDT1 <- DT[dataName == DN & 
                      dataClass == DC &
                      classToSearch == CTS]
        if (NROW(subDT1) == 0) return(NA)
        message(crayon::yellow(paste0("Preparing ", DN, 
                                      ": ", DC)))
        
oneLay <- lapply(1:NROW(subDT1), function(index){
  subDT <- subDT1[index,]
  if (any(is.na(subDT[["URL"]]), 
          is.null(subDT[["URL"]]), 
          subDT[["URL"]] == "")){
    message(crayon::red(paste0(DN, ": ", DC, " doesn't have a URL. This should be fine for ",
                   "potential layers of types 'Enlarging' and 'Connecting'. For ",
                   "type 'Generating' or other layers, please make sure this is also valid.",
                   "Returning NA")))
    return(NA)
  }
  if (subDT[["dataType"]] == "mif"){ # FOR mif files
    # 1. create a separate folder for intermediate files
    intDir <- checkPath(file.path(destinationPath, paste(DN, 
                                                         DC, 
                                                         CTS,
                                                         sep = "_")), 
                        create = TRUE)
    # 2. Check for final file. If doesn't exist, create:
    flName <- paste(DN, 
                    DC, 
                    CTS,
                    sep = "_")
    fullFlName <- file.path(intDir, paste0(flName, ".shp"))
    if (!file.exists(fullFlName)){
      ID <- as_id(subDT[["URL"]])
      isGDrive <- if (is.na(ID)) FALSE else TRUE
      if (isGDrive){
        drive_download(file = as_id(ID), path = file.path(intDir, subDT[["fileName"]]), 
                       overwrite = TRUE)
        unzip(zipfile = list.files(intDir, full.names = TRUE, pattern = ".zip"), exdir = intDir)
      } else {
        download.file(url = subDT[["URL"]], destfile = file.path(intDir, subDT[["fileName"]]))
        unzip(zipfile = list.files(intDir, full.names = TRUE, pattern = ".zip"), exdir = intDir)
      }
      # BELOW DOESN'T WORK WITH THE NEWEST VERSION OF REPRODUCIBLE!
      # downLays <- tryCatch({
      #   preProcess(url = subDT[["URL"]],
      #              archieve = subDT[["fileName"]],
      #              alsoExtract = NULL,
      #              destinationPath = checkPath(intDir,
      #                                          create = TRUE))
      # }, error = function(e){
      #   flsExist <- list.files(path = intDir, pattern = ".mif", full.names = TRUE)
      #   if (length(flsExist) != 0) message("Layers present in directory: ", 
      #                                      paste(basename(flsExist), 
      #                                            collapse = "; ")
      #   ) else
      #     stop(paste0("Something went wrong. Please check that the URL is still",
      #                 " active and you have been granted access. Another option is ",
      #                 "that the fileName in the disturbance table does not match the ",
      #                 "zip file (it shouldn't match any internal files!)"))
      # })
      allTiles <- list.files(path = intDir, pattern = ".mif", full.names = TRUE)
      allLays <- lapply(allTiles, function(Lay){
        lay <- rmndt::readMIF(Lay)
        lay <- terra::vect(lay)
        layR <- terra::project(x = lay, studyArea)
        croppedLay <- crop(layR, studyArea)
        # We need to convert to raster because there are tiles in the shapefiles
        # and union takes too long
        rasLay <- terra::rasterize(croppedLay, rasterToMatch, 
                                   field = names(layR)[grepl(names(layR), 
                                                             pattern = CTS)])
        return(rasLay)
      })
      layRas <- do.call(what = terra::mosaic, args = allLays)
      lay <- terra::as.polygons(layRas)
      # Save the file
      terra::writeVector(lay, filename = fullFlName, 
                         filetype = "ESRI Shapefile")
    } else {
      lay <- vect(fullFlName)
    }
  } else { # FOR shapefile and rasters 
    FUN <- if (subDT[["dataType"]] == "gdb") "terra::vect" else
      if (subDT[["dataType"]] == "shapefile") "raster::shapefile" else 
        if (subDT[["dataType"]] == "raster") "raster::raster" else
        stop("dataType needs to be either 'shapefile', 'raster', 'mif' or 'gdb'")
    TF <- if (!is.na(subDT[["fileName"]])) subDT[["fileName"]] else NULL
    AE <- if (subDT[["dataType"]] == "shapefile") "similar" else NULL
    intDir <- checkPath(file.path(destinationPath, paste(DN, 
                                                         DC, 
                                                         CTS,
                                                         sep = "_")), 
                        create = TRUE)
    lay <- prepInputs(url = subDT[["URL"]], 
                      targetFile = TF, 
                      alsoExtract = AE,
                      destinationPath = intDir, 
                      fun = FUN, 
                      overwrite = FALSE)
    if (!TF %in% skipFixErrors){
      lay <- fixErrors(lay) # Some polygons have errors, so we have to fix them
      # While prepInputs can't deal completely with terra
    }
    if (class(lay) == "RasterLayer"){
      lay <- terra::rast(lay) 
    }
    if (class(lay) %in% c("SpatialLinesDataFrame", "SpatialPolygonsDataFrame")){
      lay <- terra::vect(lay) 
    }

    lay <- terra::project(lay, studyArea)
    lay <- terra::crop(lay, studyArea)
}
  if (class(lay) %in% c("RasterLayer", "SpatRaster")){
    # If raster, make into a shapefile!
    lay <- as.polygons(lay)
  }
  if (subDT[["fieldToSearch"]] != ""){
    # If the layer is not empty and fieldToSearch exists, we need to make sure 
    # to get the correct features
    lay <- subset(lay, lay[[subDT[["fieldToSearch"]]]] == CTS) 
  }
  return(lay)
})

combining <- tryCatch({
    do.call(rbind, oneLay)
    message(crayon::green("All ", length(oneLay), 
                          " layers for ", DC,
                          " can be successfully combined!"))
    TRUE
  }, error = function(e){
    return(FALSE)
  })
  if (combining){
    lay <- do.call(rbind, oneLay)
  } else {
    lay <- oneLay[[1]]
  }
if (length(lay) == 0){
  message(crayon::red(paste0("There are no ", DC, 
                             ifelse(CTS == "", 
                                    "", 
                                    paste0(" (", CTS, ")")),
                             " in the studyArea")))
  lay <- NA
}
return(lay)
      })
      # Remove all NA layers
      lay <- classSearchDT[!sapply(classSearchDT, is.na)]
      if (length(lay) > 0){
        # We might have more than 1 list here, if we have different datasets 
        # for potential, for example
        names(lay) <- rep(DC, length(lay))
      } else {
        lay <- NA
      }
      return(lay)
    })
    message(crayon::green(paste0("All classes for ", DN," were processed!")))
    # Remove all NA layers
    lay <- dataClassDT[!is.na(dataClassDT)]
    lay <- unlist(lay)
    return(lay)
  })
  message(crayon::green("All datasets were processed!"))
  names(disturbanceList) <- unique(DT[["dataName"]])
  return(disturbanceList)
}