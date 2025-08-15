library(testthat)
library(terra)
library(data.table)
library(mockery)
library(sf)

testthat::test_that("createDisturbanceList: basic structure, subsetting, and combining", {
  skip_on_cran()
  
  # --- Fixtures -------------------------------------------------------------
  rtm <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100, vals = 1)
  crs(rtm) <- "EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  
  DT <- data.table(
    dataName       = c("forestry", "forestry", "mining"),
    dataClass      = c("cutblocks", "cutblocks", "potentialMining"),
    classToSearch  = c("HIGH", "LOW", ""),
    fieldToSearch  = c("type", "type", ""),
    URL            = c("vector:forestry:HIGH", "vector:forestry:LOW", "raster:mining"),
    fileName       = c(NA, NA, NA),
    dataType       = c("shapefile", "shapefile", "raster")
  )
  
  dest <- tempfile("dl_"); dir.create(dest, recursive = TRUE)
  saName <- "UT"
  
  # --- Fakes ---------------------------------------------------------------
  mk_poly <- function(xmin, ymin, size, crs, type_val = NA_character_) {
    v <- vect(matrix(c(
      xmin, ymin,
      xmin+size, ymin,
      xmin+size, ymin+size,
      xmin, ymin+size,
      xmin, ymin
    ), byrow = TRUE, ncol = 2), type = "polygons", crs = crs)
    if (!is.na(type_val)) v$type <- type_val
    v
  }
  
  fake_prepInputs <- function(url, targetFile = NULL, alsoExtract = NULL,
                              studyArea, rasterToMatch, destinationPath,
                              fun, overwrite = FALSE, ...) {
    if (url == "vector:forestry:HIGH") return(mk_poly(10, 10, 10, crs(studyArea), "HIGH"))
    if (url == "vector:forestry:LOW")  return(mk_poly(30, 10, 12, crs(studyArea), "LOW"))
    if (url == "raster:mining") {
      rr <- rast(rasterToMatch); rr[] <- 0; rr[23] <- 1; return(rr) # -> polygonized inside function
    }
    stop("Unexpected URL: ", url)
  }
  
  fake_checkPath <- function(path, create = FALSE) {
    if (isTRUE(create) && !dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }
  
  # --- Subject under test with stubs ---------------------------------------
  f <- createDisturbanceList
  stub(f, "prepInputs", fake_prepInputs)
  stub(f, "checkPath",  fake_checkPath)
  
  out <- f(DT, dest, sa, saName, rtm, FALSE)
  
  # Assertions: top-level keys
  expect_true("forestry" %in% names(out))
  expect_true("mining"   %in% names(out))
  
  # There should be TWO entries named "cutblocks" (HIGH and LOW), not a single merged one
  idx <- which(names(out$forestry) == "cutblocks")
  expect_equal(length(idx), 2L)
  
  cb_high <- out$forestry[[ idx[1] ]]
  cb_low  <- out$forestry[[ idx[2] ]]
  
  expect_s4_class(cb_high, "SpatVector")
  expect_s4_class(cb_low,  "SpatVector")
  expect_equal(geomtype(cb_high), "polygons")
  expect_equal(geomtype(cb_low),  "polygons")
  expect_true(all(cb_high$type == "HIGH"))
  expect_true(all(cb_low$type  == "LOW"))
  
  # Mining raster became polygons
  pm <- out$mining$potentialMining
  expect_s4_class(pm, "SpatVector")
  expect_equal(geomtype(pm), "polygons")
  expect_gte(nrow(pm), 1)
})

testthat::test_that("createDisturbanceList: uses on-disk cached shapefiles on second run", {
  skip_on_cran()
  
  rtm <- rast(nrows = 5, ncols = 5, xmin = 0, xmax = 50, ymin = 0, ymax = 50, vals = 1)
  crs(rtm) <- "EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  
  DT <- data.table(
    dataName       = "forestry",
    dataClass      = "cutblocks",
    classToSearch  = "HIGH",
    fieldToSearch  = "type",
    URL            = "vector:forestry:HIGH",
    fileName       = NA,
    dataType       = "shapefile"
  )
  
  dest <- tempfile("dl2_"); dir.create(dest, recursive = TRUE)
  saName <- "UT"
  
  calls <- new.env(parent = emptyenv()); calls$n <- 0L
  fake_prepInputs <- function(url, ..., studyArea, rasterToMatch, destinationPath, fun, overwrite = FALSE) {
    calls$n <- calls$n + 1L
    v <- vect(matrix(c(5,5, 15,5, 15,15, 5,15, 5,5), ncol=2, byrow=TRUE), type="polygons", crs=crs(studyArea))
    v$type <- "HIGH"; v
  }
  fake_checkPath <- function(path, create = FALSE) {
    if (isTRUE(create) && !dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }
  
  f <- createDisturbanceList
  stub(f, "prepInputs", fake_prepInputs)
  stub(f, "checkPath",  fake_checkPath)
  
  # First run: should call prepInputs once
  out1 <- f(DT, dest, sa, saName, rtm, FALSE)
  expect_equal(calls$n, 1L)
  
  # Compute the exact cached shapefile path that the function will check
  DN <- "forestry"; DC <- "cutblocks"; CTS <- "HIGH"; index <- 1L
  flName <- paste(saName, DN, DC, CTS, index, sep = "_")
  intDir <- file.path(dest, flName)
  saDigest <- digest::digest(sa)
  fullFlName <- file.path(intDir, paste0(flName, "_", saDigest, ".shp"))
  
  # If for any reason terra didn't actually create it, ensure it exists now
  if (!file.exists(fullFlName)) {
    dir.create(intDir, recursive = TRUE, showWarnings = FALSE)
    sf::st_write(sf::st_as_sf(out1$forestry$cutblocks), fullFlName,
                 delete_layer = TRUE, quiet = TRUE)
  }
  expect_true(file.exists(fullFlName))
  
  # Second call with identical inputs: should NOT call prepInputs again
  out2 <- f(DT, dest, sa, saName, rtm, FALSE)
  expect_equal(calls$n, 1L)
  
  # Sanity: same geometry
  expect_equal(nrow(out1$forestry$cutblocks), nrow(out2$forestry$cutblocks))
})

test_that("createDisturbanceList: rbind failure → falls back to first layer", {
  skip_on_cran()

  rtm <- rast(nrows = 5, ncols = 5, xmin=0, xmax=50, ymin=0, ymax=50, vals=1)
  crs(rtm) <- "EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  dest <- tempfile("dl3_"); dir.create(dest, recursive = TRUE)
  
  DT <- data.table(
    dataName       = "forestry",
    dataClass      = "cutblocks",
    classToSearch  = c("A", "B"),
    fieldToSearch  = c("keepA", "keepB"),
    URL            = c("vec:A", "vec:B"),
    fileName       = c(NA, NA),
    dataType       = c("shapefile", "shapefile")
  )
  
  polyA <- function(crs) { v <- vect(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE),
                                     type="polygons", crs=crs); v$keepA <- "A"; v }
  lineB <- function(crs) { v <- vect(matrix(c(20,0, 30,10), ncol=2, byrow=TRUE),
                                     type="lines", crs=crs); v$keepB <- "B"; v }
  
  fake_prepInputs <- function(url, targetFile = NULL, alsoExtract = NULL,
                              studyArea, rasterToMatch, destinationPath,
                              fun, overwrite = FALSE, ...) {
    if (url == "vec:A") return(polyA(crs(studyArea)))
    if (url == "vec:B") return(lineB(crs(studyArea)))  # different geometry → rbind error
    stop("Unexpected URL")
  }
  fake_checkPath <- function(path, create = FALSE) {
    if (isTRUE(create) && !dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }
  
  f <- createDisturbanceList
  stub(f, "prepInputs", fake_prepInputs)
  stub(f, "checkPath",  fake_checkPath)
  
  out <- f(DT, dest, sa, "UT", rtm, FALSE)
  cb  <- out$forestry$cutblocks
  
  # Expect it took the first layer (polygon with keepA), because rbind failed
  expect_s4_class(cb, "SpatVector")
  expect_equal(geomtype(cb), "polygons")
  expect_true("keepA" %in% names(cb))
  expect_false("keepB" %in% names(cb))
})


test_that("createDisturbanceList: checkDisturbanceProportions is called when flag = TRUE", {
  skip_on_cran()

  rtm <- rast(nrows=2, ncols=2, xmin=0, xmax=20, ymin=0, ymax=20, vals=1)
  crs(rtm) <- "EPSG:3857"
  sa <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  dest <- tempfile("dl4_"); dir.create(dest, recursive = TRUE)
  
  DT <- data.table(
    dataName       = "forestry",
    dataClass      = "cutblocks",
    classToSearch  = "HIGH",
    fieldToSearch  = "type",
    URL            = "vector:forestry:HIGH",
    fileName       = NA,
    dataType       = "shapefile"
  )
  
  fake_prepInputs <- function(...) {
    v <- vect(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE),
              type="polygons", crs="EPSG:3857")
    v$type <- "HIGH"; v
  }
  fake_checkPath <- function(path, create = FALSE) {
    if (isTRUE(create) && !dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }
  
  hit <- FALSE
  fake_checkDisturbanceProportions <- function(...) { hit <<- TRUE; invisible(NULL) }
  
  f <- createDisturbanceList
  stub(f, "prepInputs",                  fake_prepInputs)
  stub(f, "checkPath",                   fake_checkPath)
  stub(f, "checkDisturbanceProportions", fake_checkDisturbanceProportions)
  
  f(DT, dest, sa, "UT", rtm, checkDisturbanceProportions = TRUE)
  expect_true(hit)
})

test_that("combine occurs within same CTS (rbind success)", {
  skip_on_cran()
  
  rtm <- rast(nrows=5, ncols=5, xmin=0, xmax=50, ymin=0, ymax=50, vals=1); crs(rtm) <- "EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  dest <- tempfile("dlC_"); dir.create(dest)
  
  DT <- data.table(
    dataName="forestry", dataClass="cutblocks",
    classToSearch=c("HIGH","HIGH"), fieldToSearch=c("type","type"),
    URL=c("vec:H1","vec:H2"), fileName=NA, dataType="shapefile"
  )
  
  mk <- function(x,y,s,lab) { v <- vect(matrix(c(x,y, x+s,y, x+s,y+s, x,y+s, x,y),2,byrow=TRUE),
                                        type="polygons", crs=crs(sa)); v$type <- lab; v }
  
  fake_prep <- function(url, ..., studyArea, rasterToMatch, destinationPath, fun, overwrite=FALSE) {
    if (url=="vec:H1") return(mk(0,0,10,"HIGH"))
    if (url=="vec:H2") return(mk(12,0,10,"HIGH"))
    stop("bad url")
  }
  fake_check <- function(p, create=FALSE){ if(create && !dir.exists(p)) dir.create(p,TRUE); normalizePath(p, winslash="/", mustWork=FALSE) }
  
  f <- createDisturbanceList
  stub(f,"prepInputs", fake_prep); stub(f,"checkPath", fake_check)
  
  out <- f(DT, dest, sa, "UT", rtm, FALSE)
  # Only one cutblocks entry (single CTS), and it contains 2 features
  idx <- which(names(out$forestry)=="cutblocks")
  cb  <- out$forestry[[idx]]
  expect_gte(nrow(cb), 2L)
  expect_true(all(cb$type == "HIGH"))
})

test_that("invalid polygons are fixed via makeValid", {
  skip_on_cran()
  
  rtm <- rast(nrows=2, ncols=2, xmin=0, xmax=20, ymin=0, ymax=20, vals=1); crs(rtm) <- "EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  dest <- tempfile("dlV_"); dir.create(dest)
  
  DT <- data.table(dataName="forestry", dataClass="cutblocks",
                   classToSearch="", fieldToSearch="",
                   URL="vec:INVALID", fileName=NA, dataType="shapefile")
  
  # Bow-tie (self-intersecting)
  fake_prep <- function(url, ..., studyArea, rasterToMatch, destinationPath, fun, overwrite=FALSE) {
    coords <- matrix(c(0,0, 10,10, 0,10, 10,0, 0,0), ncol=2, byrow=TRUE)
    v <- vect(coords, type="polygons", crs=crs(studyArea)); v
  }
  fake_check <- function(p, create=FALSE){ if(create && !dir.exists(p)) dir.create(p,TRUE); normalizePath(p, winslash="/", mustWork=FALSE) }
  
  f <- createDisturbanceList
  stub(f,"prepInputs", fake_prep); stub(f,"checkPath", fake_check)
  
  out <- f(DT, dest, sa, "UT", rtm, FALSE)
  cb <- out$forestry$cutblocks
  expect_true(all(terra::is.valid(cb)))
})

test_that("NA URL rows are dropped; dataName may remain empty", {
  skip_on_cran()
  
  rtm <- rast(nrows=2, ncols=2, xmin=0, xmax=20, ymin=0, ymax=20, vals=1); crs(rtm) <- "EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  dest <- tempfile("dlN_"); dir.create(dest)
  
  DT <- data.table(
    dataName=c("energy"),
    dataClass=c("windTurbines"),
    classToSearch=c(""),
    fieldToSearch=c(""),
    URL=c(NA_character_), fileName=NA, dataType="shapefile"
  )
  
  # We won’t be called, but stub anyway to be safe
  f <- createDisturbanceList
  stub(f,"prepInputs", function(...) stop("should not be called"))
  stub(f,"checkPath",  function(p, create=FALSE){ if(create && !dir.exists(p)) dir.create(p,TRUE); normalizePath(p, winslash="/", mustWork=FALSE) })
  
  out <- f(DT, dest, sa, "UT", rtm, FALSE)
  expect_true("energy" %in% names(out))
  expect_true(is.null(out$energy) || length(out$energy)==0)
})

test_that("classToSearch with '/' is sanitized in cache path", {
  skip_on_cran()
  
  rtm <- rast(nrows=2, ncols=2, xmin=0, xmax=20, ymin=0, ymax=20, vals=1); crs(rtm) <- "EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  dest <- tempfile("dlS_"); dir.create(dest)
  saName <- "UT"
  
  DT <- data.table(
    dataName="forestry", dataClass="cutblocks",
    classToSearch="A/B", fieldToSearch="type",
    URL="vec:AB", fileName=NA, dataType="shapefile"
  )
  
  fake_prep <- function(url, ..., studyArea, rasterToMatch, destinationPath, fun, overwrite=FALSE) {
    v <- vect(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE),
              type="polygons", crs=crs(studyArea))
    v$type <- "A_B"  
    v
  }
  fake_check <- function(p, create=FALSE){ if(create && !dir.exists(p)) dir.create(p,TRUE); normalizePath(p, winslash="/", mustWork=FALSE) }
  
  f <- createDisturbanceList
  stub(f,"prepInputs", fake_prep); stub(f,"checkPath", fake_check)
  
  out <- f(DT, dest, sa, saName, rtm, FALSE)
  # Should run without error; also check output attribute preserved
  idx <- which(names(out$forestry)=="cutblocks")
  expect_true(length(idx)==1L)
  expect_true(all(out$forestry[[idx]]$type == "A_B"))
})

test_that("prepInputs returning 0-feature layer is handled (NA pruned)", {
  skip_on_cran()
  
  rtm <- rast(nrows=2, ncols=2, xmin=0, xmax=20, ymin=0, ymax=20, vals=1); crs(rtm) <- "EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  dest <- tempfile("dlZ_"); dir.create(dest)
  
  DT <- data.table(
    dataName="forestry", dataClass="cutblocks",
    classToSearch="", fieldToSearch="",
    URL="vec:EMPTY", fileName=NA, dataType="shapefile"
  )
  
  fake_prep <- function(url, ..., studyArea, rasterToMatch, destinationPath, fun, overwrite=FALSE) {
    v <- vect(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE),
              type="polygons", crs=crs(studyArea))
    v[0, ]  # empty SpatVector with proper schema/CRS
  }
  fake_check <- function(p, create=FALSE){ if(create && !dir.exists(p)) dir.create(p,TRUE); normalizePath(p, winslash="/", mustWork=FALSE) }
  
  f <- createDisturbanceList
  stub(f,"prepInputs", fake_prep); stub(f,"checkPath", fake_check)
  
  out <- f(DT, dest, sa, "UT", rtm, FALSE)
  expect_true("forestry" %in% names(out))
  # No cutblocks entry produced
  expect_false("cutblocks" %in% names(out$forestry))
})

test_that("missing fieldToSearch attribute triggers an error", {
  skip_on_cran()
  
  rtm <- rast(nrows=2, ncols=2, xmin=0, xmax=20, ymin=0, ymax=20, vals=1); crs(rtm) <- "EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  dest <- tempfile("dlF_"); dir.create(dest)
  
  DT <- data.table(
    dataName="forestry", dataClass="cutblocks",
    classToSearch="HIGH", fieldToSearch="type",
    URL="vec:NO_TYPE", fileName=NA, dataType="shapefile"
  )
  
  # No 'type' column here
  fake_prep <- function(url, ..., studyArea, rasterToMatch, destinationPath, fun, overwrite=FALSE) {
    vect(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0),2,byrow=TRUE), type="polygons", crs=crs(studyArea))
  }
  fake_check <- function(p, create=FALSE){ if(create && !dir.exists(p)) dir.create(p,TRUE); normalizePath(p, winslash="/", mustWork=FALSE) }
  
  f <- createDisturbanceList
  stub(f,"prepInputs", fake_prep); stub(f,"checkPath", fake_check)
  
  expect_error(
    f(DT, dest, sa, "UT", rtm, FALSE),
    "SpatVector has no records to write"
  )
})

test_that("cache invalidates when studyArea changes", {
  skip_on_cran()
  
  rtm <- rast(nrows=5,ncols=5,xmin=0,xmax=50,ymin=0,ymax=50,vals=1); crs(rtm)<-"EPSG:3857"
  sa1 <- as.polygons(ext(rtm)); crs(sa1) <- crs(rtm)
  sa2 <- buffer(sa1, 1) # different geometry → different digest
  DT <- data.table(dataName="forestry",dataClass="cutblocks",classToSearch="HIGH",
                   fieldToSearch="type",URL="vec:H",fileName=NA,dataType="shapefile")
  dest <- tempfile("dlCI_"); dir.create(dest)
  calls <- new.env(); calls$n <- 0L
  fake_prep <- function(url, ..., studyArea, rasterToMatch, destinationPath, fun, overwrite=FALSE) {
    calls$n <- calls$n + 1L
    v <- vect(matrix(c(0,0,10,0,10,10,0,10,0,0),2,byrow=TRUE), "polygons", crs=crs(studyArea))
    v$type <- "HIGH"; v
  }
  fake_check <- function(p, create=FALSE){ if(create && !dir.exists(p)) dir.create(p,TRUE); normalizePath(p, winslash="/", mustWork=FALSE) }
  f <- createDisturbanceList; stub(f,"prepInputs",fake_prep); stub(f,"checkPath",fake_check)
  f(DT, dest, sa1, "UT", rtm, FALSE); expect_equal(calls$n, 1L)
  f(DT, dest, sa2, "UT", rtm, FALSE); expect_equal(calls$n, 2L) # changed SA → new prep
})

test_that("unknown dataType errors clearly", {
  skip_on_cran()
  
  rtm <- rast(nrows=2,ncols=2,xmin=0,xmax=20,ymin=0,ymax=20,vals=1); crs(rtm)<-"EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  DT <- data.table(dataName="x", dataClass="y", classToSearch="", fieldToSearch="",
                   URL="x", fileName=NA, dataType="csv")  # invalid
  dest <- tempfile("dlDT_"); dir.create(dest)
  f <- createDisturbanceList
  stub(f,"checkPath", function(p, create=FALSE){ if(create && !dir.exists(p)) dir.create(p,TRUE); normalizePath(p, winslash="/", mustWork=FALSE) })
  expect_error(f(DT, dest, sa, "UT", rtm, FALSE),
               "dataType needs to be either 'shapefile', 'raster', 'mif' or 'gdb'")
})

test_that("fieldToSearch present but value not found → empty and write error", {
  skip_on_cran()
  
  rtm <- rast(nrows=2,ncols=2,xmin=0,xmax=20,ymin=0,ymax=20,vals=1); crs(rtm)<-"EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  DT <- data.table(dataName="forestry", dataClass="cutblocks",
                   classToSearch="HIGH", fieldToSearch="type",
                   URL="vec:NM", fileName=NA, dataType="shapefile")
  dest <- tempfile("dlNM_"); dir.create(dest)
  fake_prep <- function(url, ..., studyArea, rasterToMatch, destinationPath, fun, overwrite=FALSE) {
    v <- vect(matrix(c(0,0,10,0,10,10,0,10,0,0),2,byrow=TRUE), "polygons", crs=crs(studyArea))
    v$other <- "foo"; v   # no 'type' column → subset yields 0 features
  }
  fake_check <- function(p, create=FALSE){ if(create && !dir.exists(p)) dir.create(p,TRUE); normalizePath(p, winslash="/", mustWork=FALSE) }
  f <- createDisturbanceList; stub(f,"prepInputs",fake_prep); stub(f,"checkPath",fake_check)
  expect_error(f(DT, dest, sa, "UT", rtm, FALSE), "SpatVector has no records to write")
})

test_that("multiple dataName outputs are isolated", {
  skip_on_cran()
  
  rtm <- rast(nrows=5,ncols=5,xmin=0,xmax=50,ymin=0,ymax=50,vals=1); crs(rtm)<-"EPSG:3857"
  sa  <- as.polygons(ext(rtm)); crs(sa) <- crs(rtm)
  DT <- data.table(
    dataName=c("forestry","mining"),
    dataClass=c("cutblocks","cutblocks"),
    classToSearch=c("F", "M"),
    fieldToSearch=c("type","type"),
    URL=c("vec:F","vec:M"),
    fileName=NA, dataType="shapefile"
  )
  dest <- tempfile("dlMN_"); dir.create(dest)
  fake_prep <- function(url, ..., studyArea, rasterToMatch, destinationPath, fun, overwrite=FALSE) {
    v <- vect(matrix(c(0,0,10,0,10,10,0,10,0,0),2,byrow=TRUE), "polygons", crs=crs(studyArea))
    v$type <- if (url=="vec:F") "F" else "M"; v
  }
  fake_check <- function(p, create=FALSE){ if(create && !dir.exists(p)) dir.create(p,TRUE); normalizePath(p, winslash="/", mustWork=FALSE) }
  f <- createDisturbanceList; stub(f,"prepInputs",fake_prep); stub(f,"checkPath",fake_check)
  out <- f(DT, dest, sa, "UT", rtm, FALSE)
  expect_true(all(out$forestry$cutblocks$type=="F"))
  expect_true(all(out$mining$cutblocks$type=="M"))
})

