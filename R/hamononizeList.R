hamononizeList <- function(disturbanceList, whatNotToCombine){
  if (is.null(disturbanceList)) stop("disturbanceList can't be NULL")
  # Now that the datset is ready, we need to harmonize the ones with the same names
  harmonizedList <- lapply(names(disturbanceList), function(sectors){
    message(paste0("Harmonizing ", sectors))
    DT <- disturbanceList[[sectors]]
    if (suppressWarnings(all(all(unique(names(DT)) == names(DT)),
                         length(unique(names(DT))) == length(names(DT))))){
      message(crayon::green(paste0("Returning ", sectors, " unchanged.")))
      return(DT)
    } else {
      toGoOver <- names(DT)[!grepl(x = names(DT),
                                   pattern = paste(whatNotToCombine, collapse = "|"))]
      # Subset without the whatNotToCombine and test again. If only one of each, 
      # return the whole thing
      toAddLater <- DT[!names(DT) %in% toGoOver]
      toGoOverNow <- DT[names(DT) %in% toGoOver]
      # Find out which names are repeated
      dups <- unique(names(toGoOverNow)[duplicated(names(toGoOverNow))])
      if (length(dups) > 1){
        toAddLater <- c(toAddLater, toGoOverNow[!names(toGoOverNow) %in% dups])
      }
      if (length(dups) == 0){
        DT <- c(toAddLater, toGoOverNow)
        message(crayon::green(paste0("Returning ", sectors, " unchanged.")))
        return(DT)
      } else {
        DT2 <- lapply(dups, function(grp){
          allLays <- DT[names(DT) %in% grp]
          names(allLays) <- NULL
          oneLay <- do.call(rbind, allLays)
          return(oneLay)
        })
        names(DT2) <- dups
        message(crayon::yellow(paste0("Returning ", sectors, " merged.")))
        return(c(toAddLater, DT2))
      }
    }
  })
  names(harmonizedList) <- names(disturbanceList)
  return(harmonizedList)
}
