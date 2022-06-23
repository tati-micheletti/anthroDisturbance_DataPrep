wrapTerraList <- function(terraList, generalPath){
  listNames <- lapply(1:length(names(terraList)), function(index1){
    obj <- lapply(1:length(names(terraList[[index1]])), function(index2){
      message(paste0("Saving ", names(terraList[[index1]][[index2]])))
      obj2 <- terra::wrap(terraList[[index1]][[index2]])
      fileName <- file.path(generalPath, paste0(stringi::stri_rand_strings(1, 10), 
                                                ".qs"))
      qs::qsave(obj2, fileName)
      return(fileName)
    })
    names(obj) <- names(terraList[[index1]])
    return(obj)
  })
  names(listNames) <- names(terraList)
  return(listNames)
}

unwrapTerraList <- function(terraList){
  listNames <- lapply(1:length(names(terraList)), function(index1){
    obj <- lapply(1:length(names(terraList[[index1]])), function(index2){
      message(paste0("Recovering ", names(terraList[[index1]][[index2]])))
      obj2 <- qs::qread(terraList[[index1]][[index2]])
      return(terra::vect(obj2))
    })
    names(obj) <- names(terraList[[index1]])
    return(obj)
  })
  names(listNames) <- names(terraList)
  return(listNames)
}
