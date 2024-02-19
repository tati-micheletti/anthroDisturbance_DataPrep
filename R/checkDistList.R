checkDistList <- function(distList, 
                          studyArea,
                          rasterToMatch,
                          listFileName,
                          generalPath){
  # Make sure a list passed matches study Area and RTM

  if (is(distList, "list")){
    L1 <- lapply(seq_along(distList), function(level1){
      R1 <- distList[[level1]]
      R1name <- names(distList)[level1]
      if (is(R1, "list")){
        L2 <- lapply(seq_along(R1), function(level2){
          R2 <- R1[[level2]]
          R2name <- names(R1)[level2]
          if (is(R2, "list")){
            stop("disturbance list seems to have more than 2 levels. Please Debug.")
          }
          message(paste0("Postprocessing ", R1name, ": ", R2name))
          distRas <- reproducible::postProcess(x = R2, 
                                               studyArea = studyArea,
                                               rasterToMatch = rasterToMatch,
                                               userTags = paste0(level1,"_",level2,"_", 
                                                                 .robustDigest(R2)))
          return(distRas)
      })
        names(L2) <- names(R1)
        return(L2)
      } else stop("disturbance list seems to have less than 2 levels. Please Debug.")
    })
    names(L1) <- names(distList)
  } else {
    stop("disturbance list seems to have less than 2 levels. Please Debug.")
  }
  tList <- wrapTerraList(terraList = L1,
                         generalPath = generalPath)
  qs::qsave(tList, listFileName)
  return(L1)
}