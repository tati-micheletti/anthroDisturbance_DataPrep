# tests/testthat/test-reqdPkgs.R
# --- Helpers ---------------------------------------------------------------

# Package names declared in a module's reqdPkgs, without GitHub owner, branch or version
moduleReqdPkgs <- function(moduleFile) {
  findReqd <- function(e) {
    if (is.call(e)) {
      if (!is.null(names(e)) && "reqdPkgs" %in% names(e)) return(e[["reqdPkgs"]])
      for (a in as.list(e)[-1L]) {
        r <- findReqd(a)
        if (!is.null(r)) return(r)
      }
    }
    NULL
  }
  for (e in parse(moduleFile, keep.source = FALSE)) {
    reqd <- findReqd(e)
    if (!is.null(reqd)) break
  }
  pkgs <- unlist(eval(reqd, baseenv()))
  sub("@.*$", "", sub("^.*/", "", sub("[ (].*$", "", pkgs)))
}

# Packages the code in `files` calls as pkg::fun() or pkg:::fun() (comments and strings excluded),
# without base R's own. Walks the parsed code, so it does not need parse data (getParseData()
# returns NULL when the keep.parse.data option is FALSE, as it can be under covr).
qualifiedPkgs <- function(files) {
  basePkgs <- c("base", "utils", "stats", "methods", "tools", "parallel", "grDevices", "graphics")
  found <- character(0)
  walk <- function(e) {
    if (is.pairlist(e)) e <- as.list(e) # function arguments and their defaults
    if (is.call(e) && (identical(e[[1L]], as.name("::")) || identical(e[[1L]], as.name(":::")))) {
      found <<- c(found, as.character(e[[2L]]))
    }
    if (is.call(e) || is.list(e)) {
      for (i in seq_along(e)) {
        if (!identical(e[[i]], quote(expr = ))) walk(e[[i]]) # skip empty arguments, as in x[, 1]
      }
    }
  }
  for (f in files) for (e in parse(f, keep.source = FALSE)) walk(e)
  setdiff(unique(found), basePkgs)
}

# --- Tests -----------------------------------------------------------------

testthat::test_that("reqdPkgs declares googledrive, whose functions createDisturbanceList() calls", {
  testthat::skip_if_not_installed("googledrive")
  testthat::skip_if_not_installed("codetools")
  calls <- codetools::findGlobals(createDisturbanceList, merge = FALSE)$functions
  driveCalls <- intersect(calls, getNamespaceExports("googledrive"))
  testthat::expect_gt(length(driveCalls), 0L)
  reqd <- moduleReqdPkgs(testthat::test_path("..", "..", "anthroDisturbance_DataPrep.R"))
  testthat::expect_true("googledrive" %in% reqd,
                        info = paste("calls:", paste(driveCalls, collapse = ", ")))
})

testthat::test_that("reqdPkgs declares every package the module code calls with pkg::", {
  moduleDir <- testthat::test_path("..", "..")
  files <- c(file.path(moduleDir, "anthroDisturbance_DataPrep.R"),
             list.files(file.path(moduleDir, "R"), "[.]R$", full.names = TRUE))
  used <- qualifiedPkgs(files)
  testthat::expect_true("terra" %in% used) # the scan found the module's terra:: calls
  reqd <- moduleReqdPkgs(file.path(moduleDir, "anthroDisturbance_DataPrep.R"))
  testthat::expect_equal(setdiff(used, reqd), character(0))
})
