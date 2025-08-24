# tests/testthat/test-checkDistList.R
# --- Helpers ---------------------------------------------------------------

make_square <- function(xmin, ymin, xmax, ymax, crs = "EPSG:3857") {
  v <- terra::vect(
    matrix(c(xmin, ymin,
             xmax, ymin,
             xmax, ymax,
             xmin, ymax,
             xmin, ymin), ncol = 2, byrow = TRUE),
    type = "polygons", crs = crs
  )
  v
}

make_line <- function(x1, y1, x2, y2, crs = "EPSG:3857") {
  v <- sf::st_sfc(sf::st_linestring(rbind(c(x1, y1), c(x2, y2))), crs = 3857)
  terra::vect(v)
}

build_demo_inputs <- function() {
  rtm <- terra::rast(ncol = 20, nrow = 20, xmin = 0, xmax = 10, ymin = 0, ymax = 10, vals = 1)
  terra::crs(rtm) <- "EPSG:3857"
  sa  <- terra::as.polygons(terra::ext(rtm)); terra::crs(sa) <- terra::crs(rtm)
  
  # One polygon partly outside the SA, and one simple line
  poly <- make_square(-2, -2, 5, 5)
  line <- make_line(2, 2, 12, 2)
  
  distList <- list(
    forestry = list(cutblocks = poly),
    energy   = list(roads     = line)
  )
  
  list(rtm = rtm, sa = sa, distList = distList)
}

local_ns_mock <- function(pkg, name, fn, envir = parent.frame()) {
  ns <- asNamespace(pkg)
  if (!exists(name, envir = ns, inherits = FALSE)) {
    stop(sprintf("Can't find %s::%s in the namespace", pkg, name))
  }
  was_locked <- bindingIsLocked(name, ns)
  if (was_locked) unlockBinding(name, ns)
  orig <- get(name, envir = ns, inherits = FALSE)
  assign(name, fn, envir = ns)
  if (was_locked) lockBinding(name, ns)
  
  withr::defer({
    if (bindingIsLocked(name, ns)) unlockBinding(name, ns)
    assign(name, orig, envir = ns)
    if (was_locked) lockBinding(name, ns)
  }, envir = envir)
}

local_global_mock <- function(name, fn, envir = parent.frame()) {
  env <- .GlobalEnv
  had  <- exists(name, envir = env, inherits = FALSE)
  orig <- if (had) get(name, envir = env, inherits = FALSE) else NULL
  assign(name, fn, envir = env)
  
  withr::defer({
    if (had) {
      assign(name, orig, envir = env)
    } else {
      rm(list = name, envir = env, inherits = FALSE)
    }
  }, envir = envir)
}


# Provide a predictable digest used inside checkDistList() call
# (the function references `.robustDigest(...)`)
if (!exists(".robustDigest", mode = "function")) {
  .robustDigest <- function(x) "FAKEHASH"
}


# --- Tests -----------------------------------------------------------------

testthat::test_that("errors when input is not a 2-level nested list", {
  demo <- build_demo_inputs()
  
  # Not a list at all
  testthat::expect_error(
    checkDistList(demo$sa, demo$sa, demo$rtm, tempfile(fileext = ".qs"), tempdir()),
    "less than 2 levels", fixed = TRUE
  )
  
  # A 1-level list (no inner list)
  bad <- list(forestry = make_square(0, 0, 1, 1))
  testthat::expect_error(
    checkDistList(bad, demo$sa, demo$rtm, tempfile(fileext = ".qs"), tempdir()),
    "less than 2 levels", fixed = TRUE
  )
  
  # A 3-level list (inner list inside inner list)
  too_deep <- list(forestry = list(cutblocks = list(A = make_square(0,0,1,1))))
  testthat::expect_error(
    checkDistList(too_deep, demo$sa, demo$rtm, tempfile(fileext = ".qs"), tempdir()),
    "more than 2 levels", fixed = TRUE
  )
})

testthat::test_that("happy path: structure preserved; postProcess, wrapTerraList, qsave invoked", {
  testthat::skip_on_cran()
  demo <- build_demo_inputs()
  
  # capture bins
  call_env <- new.env(parent = emptyenv())
  call_env$post_calls <- list()
  call_env$wrap_args  <- NULL
  call_env$qsave_args <- NULL
  
  # mock reproducible::postProcess (namespaced)
  local_ns_mock("reproducible", "postProcess", function(x, studyArea, rasterToMatch, userTags) {
    call_env$post_calls[[length(call_env$post_calls) + 1]] <<- list(
      class_x   = class(x)[1],
      is_vect   = inherits(x, "SpatVector"),
      userTags  = userTags,
      crs_match = terra::same.crs(studyArea, rasterToMatch)
    )
    x
  })
  
  # mock wrapTerraList (likely defined in your project/global env)
  mockery::stub(checkDistList, "wrapTerraList", function(terraList, generalPath) {
    call_env$wrap_args <<- list(terraList = terraList, generalPath = generalPath)
    structure(list(marker = "wrapped-sentinel"), class = "wrapped")
  })
  
  # mock qs::qsave (namespaced)
  local_ns_mock("qs", "qsave", function(x, file, ...) {
    call_env$qsave_args <<- list(x = x, file = file)
    invisible(NULL)
  })
  
  # stub .robustDigest if it doesn't exist (project dep)
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  out_file <- tempfile(fileext = ".qs")
  out <- checkDistList(
    distList     = demo$distList,
    studyArea    = demo$sa,
    rasterToMatch= demo$rtm,
    listFileName = out_file,
    generalPath  = tempdir()
  )
  
  # 1) Returned object is a 2-level list with preserved names
  testthat::expect_type(out, "list")
  testthat::expect_setequal(names(out), c("forestry", "energy"))
  testthat::expect_setequal(names(out$forestry), "cutblocks")
  testthat::expect_setequal(names(out$energy),   "roads")
  
  # 2) postProcess was called once per inner-layer (2 total)
  testthat::expect_equal(length(call_env$post_calls), 2L)
  # sanity checks for captured args
  testthat::expect_true(all(vapply(call_env$post_calls, `[[`, logical(1), "is_vect")))
  testthat::expect_true(all(vapply(call_env$post_calls, `[[`, logical(1), "crs_match")))
  
  # userTags contains the level ids and our FAKEHASH (we don't check exact prefix, just the digest part)
  tags <- vapply(call_env$post_calls, `[[`, character(1), "userTags")
  # Each tag should start with either "1_1_" (forestry/cutblocks) or "2_1_" (energy/roads)
  testthat::expect_setequal(substr(tags, 1, 4), c("1_1_", "2_1_"))
  # And there must be a non-empty digest after the "<L1>_<L2>_" prefix
  suffixes <- sub("^[^_]+_[^_]+_", "", tags)
  testthat::expect_true(all(nchar(suffixes) > 0))
  
  
  # 3) wrapTerraList received the same nested structure that function returns
  testthat::expect_false(is.null(call_env$wrap_args))
  testthat::expect_true(is.list(call_env$wrap_args$terraList))
  # The function returns L1 (post-processed list). That should be identical to what wrapTerraList saw.
  testthat::expect_equal(call_env$wrap_args$terraList, out)
  
  # 4) qs::qsave was called with whatever wrapTerraList returned and correct path
  testthat::expect_false(is.null(call_env$qsave_args))
  testthat::expect_s3_class(call_env$qsave_args$x, "wrapped")
  testthat::expect_identical(call_env$qsave_args$file, out_file)
})

testthat::test_that("optional integration: real postProcess crops to studyArea (quick sanity)", {
  testthat::skip_on_cran()

  demo <- build_demo_inputs()
  
  # Use real reproducible::postProcess; only stub wrapTerraList + qs::qsave
  mockery::stub(checkDistList, "wrapTerraList", function(terraList, generalPath) terraList)
  local_ns_mock("qs", "qsave", function(x, file, ...) invisible(NULL))
  
  # Ensure the digest symbol exists (checkDistList calls `.robustDigest`)
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  out <- checkDistList(
    distList      = demo$distList,
    studyArea     = demo$sa,
    rasterToMatch = demo$rtm,
    listFileName  = tempfile(fileext = ".qs"),
    generalPath   = tempdir()
  )
  
  # The polygon originally extended outside SA (-2..5). After postProcess it should be inside 0..10.
  poly_out <- out$forestry$cutblocks
  e <- terra::ext(poly_out)
  testthat::expect_gte(terra::xmin(e), 0)
  testthat::expect_gte(terra::ymin(e), 0)
  testthat::expect_lte(terra::xmax(e), 10)
  testthat::expect_lte(terra::ymax(e), 10)
  
  # Output objects should be in the same CRS as the rasterToMatch
  testthat::expect_true(terra::same.crs(poly_out, demo$rtm))
  testthat::expect_true(terra::same.crs(out$energy$roads, demo$rtm))
})

testthat::test_that("SpatRaster entries are cropped to studyArea and aligned to rasterToMatch", {
  testthat::skip_on_cran()

  # Build base inputs
  demo <- build_demo_inputs()
  
  # A raster that extends outside the SA/resolution of RTM
  r2 <- terra::rast(ncol = 5, nrow = 5, xmin = -5, xmax = 15, ymin = -5, ymax = 15, vals = 1)
  terra::crs(r2) <- terra::crs(demo$rtm)
  
  distList <- list(
    remote = list(rastermask = r2)  # raster branch only
  )
  
  # Use real postProcess; stub wrap & qsave
  mockery::stub(checkDistList, "wrapTerraList", function(terraList, generalPath) terraList)
  local_ns_mock("qs", "qsave", function(x, file, ...) invisible(NULL))
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  out <- checkDistList(
    distList      = distList,
    studyArea     = demo$sa,
    rasterToMatch = demo$rtm,
    listFileName  = tempfile(fileext = ".qs"),
    generalPath   = tempdir()
  )
  
  testthat::expect_true(inherits(out$remote$rastermask, "SpatRaster"))
  # Inside SA extent (0..10 both axes)
  e <- terra::ext(out$remote$rastermask)
  testthat::expect_gte(terra::xmin(e), 0); testthat::expect_gte(terra::ymin(e), 0)
  testthat::expect_lte(terra::xmax(e), 10); testthat::expect_lte(terra::ymax(e), 10)
  # Same CRS & resolution as RTM
  testthat::expect_true(terra::same.crs(out$remote$rastermask, demo$rtm))
  testthat::expect_equal(terra::res(out$remote$rastermask), terra::res(demo$rtm))
})

testthat::test_that("Non-overlapping vector becomes empty without errors", {
  testthat::skip_on_cran()

  demo <- build_demo_inputs()
  # Polygon completely outside SA
  poly_outside <- terra::vect(matrix(c(20,20, 21,20, 21,21, 20,21, 20,20), ncol=2, byrow=TRUE),
                              type="polygons", crs=terra::crs(demo$rtm))
  distList <- list(forestry = list(cutblocks = poly_outside))
  
  mockery::stub(checkDistList, "wrapTerraList", function(terraList, generalPath) terraList)
  local_ns_mock("qs", "qsave", function(x, file, ...) invisible(NULL))
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  out <- checkDistList(
    distList      = distList,
    studyArea     = demo$sa,
    rasterToMatch = demo$rtm,
    listFileName  = tempfile(fileext = ".qs"),
    generalPath   = tempdir()
  )
  
  testthat::expect_true(inherits(out$forestry$cutblocks, "SpatVector"))
  testthat::expect_equal(nrow(out$forestry$cutblocks), 0L)
})

testthat::test_that("Informative messages are emitted per layer", {
  testthat::skip_on_cran()
  demo <- build_demo_inputs()
  
  # Quiet postProcess; we only want the message from checkDistList itself
  local_ns_mock("reproducible", "postProcess", function(x, studyArea, rasterToMatch, userTags) x)
  mockery::stub(checkDistList, "wrapTerraList", function(terraList, generalPath) terraList)
  local_ns_mock("qs", "qsave", function(x, file, ...) invisible(NULL))
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  expect_msg <- "Postprocessing (forestry|energy): (cutblocks|roads)"
  testthat::expect_message(
    checkDistList(
      distList      = demo$distList,
      studyArea     = demo$sa,
      rasterToMatch = demo$rtm,
      listFileName  = tempfile(fileext = ".qs"),
      generalPath   = tempdir()
    ),
    regexp = expect_msg
  )
})

testthat::test_that("wrapTerraList error propagates and qsave is not called", {
  testthat::skip_on_cran()
  demo <- build_demo_inputs()
  
  called_qsave <- FALSE
  local_ns_mock("reproducible", "postProcess", function(x, ...) x)
  mockery::stub(checkDistList, "wrapTerraList",
                function(terraList, generalPath) stop("boom wrap")
  )  
  local_ns_mock("qs", "qsave", function(x, file, ...) { called_qsave <<- TRUE; invisible(NULL) })
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  testthat::expect_error(
    checkDistList(
      distList      = demo$distList,
      studyArea     = demo$sa,
      rasterToMatch = demo$rtm,
      listFileName  = tempfile(fileext = ".qs"),
      generalPath   = tempdir()
    ),
    "boom wrap"
  )
  testthat::expect_false(called_qsave)
})

testthat::test_that("Empty outer list is handled gracefully", {
  testthat::skip_on_cran()
  
  demo <- build_demo_inputs()
  empty <- list()
  
  seen_wrap <- FALSE
  seen_qsave <- FALSE
  mockery::stub(checkDistList, "wrapTerraList", function(terraList, generalPath) {
    seen_wrap <<- TRUE
    testthat::expect_identical(terraList, list())
    terraList
  })
  local_ns_mock("qs", "qsave", function(x, file, ...) { seen_qsave <<- TRUE; invisible(NULL) })
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  out <- checkDistList(
    distList      = empty,
    studyArea     = demo$sa,
    rasterToMatch = demo$rtm,
    listFileName  = tempfile(fileext = ".qs"),
    generalPath   = tempdir()
  )
  
  testthat::expect_true(is.list(out))
  testthat::expect_length(out, 0L)
  testthat::expect_true(seen_wrap)
  testthat::expect_true(seen_qsave)
})

# --- More tests for checkDistList ------------------------------------------------

# 1) SpatRaster support: cropped to SA, aligned to RTM
testthat::test_that("SpatRaster entries are cropped to studyArea and aligned to rasterToMatch", {
  testthat::skip_on_cran()
  
  demo <- build_demo_inputs()
  
  # A raster extending beyond the SA/RTM extent
  r2 <- terra::rast(ncol = 5, nrow = 5, xmin = -5, xmax = 15, ymin = -5, ymax = 15, vals = 1)
  terra::crs(r2) <- terra::crs(demo$rtm)
  
  distList <- list(
    remote = list(rastermask = r2)
  )
  
  # Use real postProcess; stub wrap & qsave
  mockery::stub(checkDistList, "wrapTerraList", function(terraList, generalPath) terraList)
  local_ns_mock("qs", "qsave", function(x, file, ...) invisible(NULL))
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  out <- checkDistList(
    distList      = distList,
    studyArea     = demo$sa,
    rasterToMatch = demo$rtm,
    listFileName  = tempfile(fileext = ".qs"),
    generalPath   = tempdir()
  )
  
  testthat::expect_true(inherits(out$remote$rastermask, "SpatRaster"))
  
  e <- terra::ext(out$remote$rastermask)
  testthat::expect_gte(terra::xmin(e), 0)
  testthat::expect_gte(terra::ymin(e), 0)
  testthat::expect_lte(terra::xmax(e), 10)
  testthat::expect_lte(terra::ymax(e), 10)
  
  testthat::expect_true(terra::same.crs(out$remote$rastermask, demo$rtm))
  testthat::expect_equal(terra::res(out$remote$rastermask), terra::res(demo$rtm))
})

# 2) Non-overlapping vector → empty output, no crash
testthat::test_that("Non-overlapping vector becomes empty without errors", {
  testthat::skip_on_cran()
  
  demo <- build_demo_inputs()
  
  poly_outside <- terra::vect(
    matrix(c(20,20, 21,20, 21,21, 20,21, 20,20), ncol=2, byrow=TRUE),
    type="polygons", crs=terra::crs(demo$rtm)
  )
  distList <- list(forestry = list(cutblocks = poly_outside))
  
  mockery::stub(checkDistList, "wrapTerraList", function(terraList, generalPath) terraList)
  local_ns_mock("qs", "qsave", function(x, file, ...) invisible(NULL))
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  out <- checkDistList(
    distList      = distList,
    studyArea     = demo$sa,
    rasterToMatch = demo$rtm,
    listFileName  = tempfile(fileext = ".qs"),
    generalPath   = tempdir()
  )
  
  testthat::expect_true(inherits(out$forestry$cutblocks, "SpatVector"))
  testthat::expect_equal(nrow(out$forestry$cutblocks), 0L)
})

# 3) Informative messages per layer
testthat::test_that("Informative messages are emitted per layer", {
  testthat::skip_on_cran()
  
  demo <- build_demo_inputs()
  
  # Quiet postProcess; we only assert the messages our function emits
  local_ns_mock("reproducible", "postProcess", function(x, studyArea, rasterToMatch, userTags) x)
  mockery::stub(checkDistList, "wrapTerraList", function(terraList, generalPath) terraList)
  local_ns_mock("qs", "qsave", function(x, file, ...) invisible(NULL))
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  # We expect at least one of the layer messages
  expect_msg <- "Postprocessing (forestry|energy): (cutblocks|roads)"
  testthat::expect_message(
    checkDistList(
      distList      = demo$distList,
      studyArea     = demo$sa,
      rasterToMatch = demo$rtm,
      listFileName  = tempfile(fileext = ".qs"),
      generalPath   = tempdir()
    ),
    regexp = expect_msg
  )
})

# 4) wrapTerraList error propagates; qsave is not called
testthat::test_that("wrapTerraList error propagates and qsave is not called", {
  testthat::skip_on_cran()
  
  demo <- build_demo_inputs()
  
  called_qsave <- FALSE
  local_ns_mock("reproducible", "postProcess", function(x, ...) x)
  mockery::stub(checkDistList, "wrapTerraList",
                function(terraList, generalPath) stop("boom wrap")
  )  
  local_ns_mock("qs", "qsave", function(x, file, ...) { called_qsave <<- TRUE; invisible(NULL) })
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  testthat::expect_error(
    checkDistList(
      distList      = demo$distList,
      studyArea     = demo$sa,
      rasterToMatch = demo$rtm,
      listFileName  = tempfile(fileext = ".qs"),
      generalPath   = tempdir()
    ),
    "boom wrap"
  )
  testthat::expect_false(called_qsave)
})

# 5) Empty outer list handled gracefully (still wraps & saves)
testthat::test_that("Empty outer list is handled gracefully", {
  testthat::skip_on_cran()
  
  demo <- build_demo_inputs()
  empty <- list()
  
  seen_wrap  <- FALSE
  seen_qsave <- FALSE
  mockery::stub(checkDistList, "wrapTerraList", function(terraList, generalPath) {
    seen_wrap <<- TRUE
    testthat::expect_identical(terraList, list())
    terraList
  })
  local_ns_mock("qs", "qsave", function(x, file, ...) { seen_qsave <<- TRUE; invisible(NULL) })
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  
  out <- checkDistList(
    distList      = empty,
    studyArea     = demo$sa,
    rasterToMatch = demo$rtm,
    listFileName  = tempfile(fileext = ".qs"),
    generalPath   = tempdir()
  )
  
  testthat::expect_true(is.list(out))
  testthat::expect_length(out, 0L)
  testthat::expect_true(seen_wrap)
  testthat::expect_true(seen_qsave)
})
