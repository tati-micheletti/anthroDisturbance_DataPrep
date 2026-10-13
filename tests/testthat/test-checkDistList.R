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
  f <- tempfile(fileext = ".rds")
  testthat::expect_error(checkDistList(demo$sa, demo$sa, demo$rtm, f, tempdir()),
                         "less than 2 levels", fixed = TRUE)
  bad <- list(forestry = make_square(0, 0, 1, 1))
  testthat::expect_error(checkDistList(bad, demo$sa, demo$rtm, f, tempdir()),
                         "less than 2 levels", fixed = TRUE)
  too_deep <- list(forestry = list(cutblocks = list(A = make_square(0, 0, 1, 1))))
  testthat::expect_error(checkDistList(too_deep, demo$sa, demo$rtm, f, tempdir()),
                         "more than 2 levels", fixed = TRUE)
})

testthat::test_that("structure preserved; postProcess called once per layer; list saved as .rds", {
  testthat::skip_on_cran()
  demo <- build_demo_inputs()
  calls <- new.env(parent = emptyenv()); calls$n <- 0L; calls$tags <- character()
  local_ns_mock("reproducible", "postProcess", function(x, studyArea, rasterToMatch, userTags) {
    calls$n <- calls$n + 1L
    calls$tags <- c(calls$tags, userTags)
    x
  })
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  out_file <- tempfile(fileext = ".rds")
  out <- checkDistList(demo$distList, demo$sa, demo$rtm, out_file, tempdir())

  testthat::expect_setequal(names(out), c("forestry", "energy"))
  testthat::expect_setequal(names(out$forestry), "cutblocks")
  testthat::expect_setequal(names(out$energy), "roads")
  testthat::expect_equal(calls$n, 2L)
  testthat::expect_setequal(substr(calls$tags, 1, 4), c("1_1_", "2_1_"))

  # The saved file is a wrapped version of the same nested list and can be recovered
  testthat::expect_true(file.exists(out_file))
  saved <- readRDS(out_file)
  testthat::expect_setequal(names(saved), names(out))
  rec <- terra::unwrap(saved$forestry$cutblocks)
  testthat::expect_s4_class(rec, "SpatVector")
})

testthat::test_that("vectors are cropped to studyArea and keep the rasterToMatch CRS", {
  testthat::skip_on_cran()
  demo <- build_demo_inputs()
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  out <- checkDistList(demo$distList, demo$sa, demo$rtm, tempfile(fileext = ".rds"), tempdir())
  e <- terra::ext(out$forestry$cutblocks)  # originally -2..5, study area is 0..10
  testthat::expect_gte(terra::xmin(e), 0)
  testthat::expect_gte(terra::ymin(e), 0)
  testthat::expect_lte(terra::xmax(e), 10)
  testthat::expect_lte(terra::ymax(e), 10)
  testthat::expect_true(terra::same.crs(out$forestry$cutblocks, demo$rtm))
  testthat::expect_true(terra::same.crs(out$energy$roads, demo$rtm))
})

testthat::test_that("empty outer list is handled gracefully", {
  testthat::skip_on_cran()
  demo <- build_demo_inputs()
  out_file <- tempfile(fileext = ".rds")
  out <- checkDistList(list(), demo$sa, demo$rtm, out_file, tempdir())
  testthat::expect_true(is.list(out))
  testthat::expect_length(out, 0L)
})

testthat::test_that("informative messages are emitted per layer", {
  testthat::skip_on_cran()
  demo <- build_demo_inputs()
  if (!exists(".robustDigest", mode = "function")) {
    local_global_mock(".robustDigest", function(x) "FAKEHASH")
  }
  msgs <- testthat::capture_messages(
    checkDistList(demo$distList, demo$sa, demo$rtm, tempfile(fileext = ".rds"), tempdir())
  )
  testthat::expect_true(any(grepl("Postprocessing forestry: cutblocks", msgs, fixed = TRUE)))
  testthat::expect_true(any(grepl("Postprocessing energy: roads", msgs, fixed = TRUE)))
})
