createDisturbanceList <- function(DT, 
                                  destinationPath, 
                                  studyArea,
                                  studyAreaName,
                                  rasterToMatch,
                                  checkDisturbanceProportions){
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
          # 1. create a separate folder for intermediate files
          CTS <- if (grepl(pattern = "/", CTS)) gsub(pattern = "/", replacement = "_", x = CTS) else 
            CTS
            
            flName <- paste(studyAreaName, 
                          DN,
                          DC,
                          CTS,
                          index,
                          sep = "_")
          intDir <- checkPath(file.path(destinationPath, flName),
                              create = TRUE)

          saDigest <- digest::digest(studyArea)
          fullFlName <- file.path(intDir, paste0(flName, "_", saDigest, ".shp"))
          
          if (!file.exists(fullFlName)){
            message(paste0(flName, " doesn't exist, creating..."))
            
            if (subDT[["dataType"]] == "mif"){ # FOR mif files
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

              allTiles <- list.files(path = intDir, pattern = ".mif", full.names = TRUE)
              allLays <- lapply(allTiles, function(Lay){
                lay <- terra::vect(Lay)
                layR <- terra::project(x = lay, studyArea)
                croppedLay <- terra::crop(layR, studyArea)
                maskedLay <- terra::mask(croppedLay, studyArea)

                # We need to convert to raster because there are tiles in the shapefiles
                # and union takes too long
                if (NROW(maskedLay) == 0) return(NULL) # For small study areas, one of the tiles might
                # not intersect
                # terra::rast() gives the template's geometry (and converts a RasterLayer)
                rasLay <- terra::rasterize(maskedLay, terra::rast(rasterToMatch),
                                           field = names(layR)[grepl(names(layR),
                                                                     pattern = CTS)])
                return(rasLay)
              })
              allLays <- allLays[!sapply(allLays, is.null)]
              layRas <- do.call(what = terra::mosaic, args = allLays)
              lay <- terra::as.polygons(layRas)
            } else {
              FUN <- if (subDT[["dataType"]] %in% c("gdb", "shapefile", "mif")) "terra::vect" else
                if (subDT[["dataType"]] == "raster") "terra::rast" else
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
                                studyArea = studyArea,
                                rasterToMatch = rasterToMatch,
                                destinationPath = intDir, 
                                fun = FUN, 
                                overwrite = getOption("reproducible.overwrite", FALSE))
              # On some SpaDES runs prepInputs can incorrectly drop all features even when
              # the source file overlaps the studyArea (e.g., forestry cutblocks). If that
              # happens, try a lightweight manual crop/mask from the raw file before giving up.
              if (inherits(lay, "SpatVector") && terra::nrow(lay) == 0) {
                src_raw <- sub("^file://", "", utils::URLdecode(subDT[["URL"]]))
                if (nzchar(src_raw) && file.exists(src_raw)) {
                  recovered <- tryCatch({
                    raw <- terra::vect(src_raw)
                    tgt <- if (inherits(rasterToMatch, "SpatRaster")) rasterToMatch else NULL
                    if (!is.null(tgt)) raw <- terra::project(raw, tgt)
                    if (!is.null(studyArea)) {
                      raw <- terra::crop(raw, studyArea)
                      raw <- terra::mask(raw, studyArea)
                    }
                    raw
                  }, error = function(...) NULL)
                  if (inherits(recovered, "SpatVector") && terra::nrow(recovered) > 0) {
                    lay <- recovered
                    message(crayon::yellow(paste0(
                      "Recovered ", terra::nrow(lay),
                      " features after prepInputs returned empty for ",
                      paste(DN, DC, CTS, sep = "/"), "."
                    )))
                  }
                }
              }
              # Ensure shapefile sidecars exist when reproducible hardlinks only .shp
              if (!is.null(TF) && endsWith(tolower(TF), ".shp")) {
                shp_out <- list.files(intDir, pattern = paste0("^", gsub("([\\^\\$\\.\\|\\(\\)\\[\\]\\+\\?\\{\\}\\\\])", "\\\\\\1", basename(TF)), "$"),
                                      full.names = TRUE)
                sidecars <- c(".dbf", ".shx", ".prj", ".cpg")
                if (!length(shp_out)) shp_out <- file.path(intDir, basename(TF))
                stem_out <- tools::file_path_sans_ext(shp_out[1])
                src_stem <- sub("^file://", "", utils::URLdecode(subDT[["URL"]]))
                src_stem <- tools::file_path_sans_ext(src_stem)
                for (ext in sidecars) {
                  dst <- paste0(stem_out, ext)
                  if (!file.exists(dst)) {
                    src <- paste0(src_stem, ext)
                    if (file.exists(src)) {
                      file.copy(src, dst, overwrite = FALSE)
                    }
                  }
                }
              }
              
              if (length(lay) == 0){
                  message(crayon::red(paste0("There are no ", DC, 
                                             ifelse(CTS == "", 
                                                    "", 
                                                    paste0(" (", CTS, ")")),
                                             " in the studyArea")))
                  return(NA)
              }
              if (is(lay, "SpatVector")){
                isValidLay <- all(terra::is.valid(lay))
                if (!isValidLay)
                  lay <- terra::makeValid(lay) # Some polygons may have errors, so we have to fix them
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
            }
            # Save the layer if doesn't exist
            if (inherits(lay, "SpatVector") && terra::nrow(lay) == 0L) {
              message("Skipping write: empty SpatVector (", paste(DN, DC, CTS, sep = "/"), ")")
            } else {
              terra::writeVector(lay,
                                 filename = fullFlName,
                                 filetype = "ESRI Shapefile")
            }
          } else {
            # Load the layer if exists
            message(paste0(flName, " exists, returning..."))
            lay <- terra::vect(fullFlName)
          }
          return(lay)
})

        # A layer with no URL, or none in the studyArea, is NA: it holds nothing to combine, and
        # must not displace the layers that do
        oneLay <- oneLay[!vapply(oneLay, function(x) is.atomic(x) && all(is.na(x)), logical(1))]
        if (length(oneLay) == 0L) return(NA)
        # Layers that cannot be combined (e.g., lines and polygons in one class) stop the run:
        # keeping only one of them would silently drop the others' features
        lay <- tryCatch(do.call(rbind, oneLay), error = function(e) {
          types <- vapply(oneLay, function(x) {
            if (inherits(x, "SpatVector")) terra::geomtype(x) else class(x)[1]
          }, character(1))
          stop(DN, "/", DC, "/", CTS, ": its ", length(oneLay), " layers cannot be combined (",
               paste(types, collapse = ", "), "); a class must hold one geometry type. ",
               conditionMessage(e), call. = FALSE)
        })
        message(crayon::green("All ", length(oneLay), " layers for ",
                              DC, " can be successfully combined!"))
        return(lay)
      })
      # Remove all NA layers
      laysToRemove <- unlist(lapply(1:length(classSearchDT), 
                                    function(LAY) all(is.na(classSearchDT[[LAY]]))))
      lay <- classSearchDT[!laysToRemove]
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
  if (checkDisturbanceProportions){
    checkDisturbanceProportions(DT = DT, 
                                destinationPath = checkPath(file.path(destinationPath,
                                                                      paste0("proportions_",
                                                                             studyAreaName)),
                                                            create = TRUE),
                                studyArea = studyArea,
                                rasterToMatch = rasterToMatch)
  }
  return(disturbanceList)
}
