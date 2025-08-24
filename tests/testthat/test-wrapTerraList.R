# Helper: create a simple SpatVector for testing
elem <- matrix(c(0,0, 1,1, 2,2), ncol=2, byrow=TRUE)
create_dummy_vect <- function() terra::vect(elem, type="points")

# Helper: create a simple SpatRaster for testing
create_dummy_rast <- function() {
  r <- terra::rast(nrows=2, ncols=2)
  terra::values(r) <- matrix(1:4, nrow=2)
  terra::crs(r) <- "EPSG:4326"
  return(r)
}

# ---- Basic tests ----
test_that("wrapTerraList wraps and saves SpatVectors", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  
  tv <- list(g1 = list(a = create_dummy_vect()),
             g2 = list(b = create_dummy_vect()))
  res <- suppressMessages(wrapTerraList(tv, generalPath = tmp, zipFiles = FALSE))
  
  expect_named(res, names(tv))
  for (grp in names(tv)) {
    expect_named(res[[grp]], names(tv[[grp]]))
    for (feat in names(tv[[grp]])) {
      pth <- res[[grp]][[feat]]
      expect_true(file.exists(pth), info = pth)
      wrapped <- qread(pth)
      rec <- terra::vect(wrapped)
      expect_equal(terra::crds(rec), terra::crds(tv[[grp]][[feat]]))
    }
  }
})


test_that("unwrapTerraList reconstructs SpatVectors from paths", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  
  tv <- list(x = list(p = create_dummy_vect()))
  wrapped <- wrapTerraList(tv, generalPath = tmp, zipFiles = FALSE)
  rec <- unwrapTerraList(wrapped, generalPath = tmp)
  
  expect_s4_class(rec$x$p, "SpatVector")
  expect_equal(terra::crds(rec$x$p), terra::crds(tv$x$p))
})

# ---- Raster support ----
test_that("wrapTerraList wraps SpatRaster objects", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  
  tv <- list(r1 = list(r = create_dummy_rast()))
  res <- wrapTerraList(tv, generalPath = tmp, zipFiles = FALSE)
  
  pth <- res$r1$r
  expect_true(file.exists(pth))
  wrapped <- qread(pth)
  rec_r <- terra::rast(wrapped)
  expect_equal(terra::values(rec_r), terra::values(create_dummy_rast()))
  expect_equal(terra::crs(rec_r, proj=TRUE), terra::crs(create_dummy_rast(), proj=TRUE))
})

# ---- Manifest structure ----
test_that("theList.qs manifest has correct structure", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  
  tv <- list(a = list(b = create_dummy_vect()),
             c = list(d = create_dummy_vect()))
  suppressWarnings(wrapTerraList(tv, generalPath = tmp, zipFiles = TRUE))
  
  manifest <- qread(file.path(tmp, "theList.qs"))
  expect_named(manifest, names(tv))
  for (grp in names(tv)) {
    expect_named(manifest[[grp]], names(tv[[grp]]))
    for (pth in manifest[[grp]]) {
      expect_true(startsWith(pth, tmp))
      expect_true(file.exists(pth))
    }
  }
})

# ---- Zip and Drive tests ----
test_that("wrapTerraList creates zip and manifest when zipFiles=TRUE", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  
  tv <- list(x = list(v = create_dummy_vect()))
  suppressWarnings(wrapTerraList(tv, generalPath = tmp, zipFiles = TRUE))
  expect_true(file.exists(file.path(tmp, "disturbanceList.zip")))
  expect_true(file.exists(file.path(tmp, "theList.qs")))
})

# stub drive_upload for drive tests
test_that("wrapTerraList uploads zip only when zipFiles=TRUE and uploadZip provided", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  
  tv <- list(a = list(b = create_dummy_vect()))
  mock_up <- mockery::mock()
  mockery::stub(wrapTerraList, 'drive_upload', mock_up)
  
  # zipFiles=FALSE => no upload
  wrapTerraList(tv, generalPath = tmp, zipFiles = FALSE, uploadZip = "drive123")
  expect_equal(length(mockery::mock_args(mock_up)), 0)
  
  # zipFiles=TRUE => upload once
  suppressWarnings(wrapTerraList(tv, generalPath = tmp, zipFiles = TRUE, uploadZip = "drive123"))
  expect_equal(length(mockery::mock_args(mock_up)), 1)
})

# unwrap drive download
test_that("unwrapTerraList downloads and unwraps when given a Drive link", {
  orig <- tempfile(); dir.create(orig)
  on.exit(unlink(orig, recursive=TRUE), add=TRUE)
  tv <- list(a = list(b = create_dummy_vect()))
  suppressWarnings(wrapTerraList(tv, generalPath = orig, zipFiles = TRUE))
  zipf <- file.path(orig, "disturbanceList.zip")
  
  dest <- tempfile(); dir.create(dest)
  on.exit(unlink(dest, recursive=TRUE), add=TRUE)
  
  mockery::stub(unwrapTerraList, 'drive_download', function(file, path) {
    expect_equal(file, as_id("link123"))
    file.copy(zipf, file.path(dest, "disturbanceList.zip"))
    NULL
  })
  
  rec <- unwrapTerraList("link123", generalPath = dest)
  expect_s4_class(rec$a$b, "SpatVector")
  expect_equal(terra::crds(rec$a$b), terra::crds(tv$a$b))
})

# ---- Path-fixing logic ----
test_that("unwrapTerraList correctly updates paths when moving files", {
  orig <- tempfile(); dir.create(orig)
  on.exit(unlink(orig, recursive=TRUE), add=TRUE)
  tv <- list(a = list(b = create_dummy_vect()))
  wrapped <- wrapTerraList(tv, generalPath = orig, zipFiles = FALSE)
  
  dest <- tempfile(); dir.create(dest)
  on.exit(unlink(dest, recursive=TRUE), add=TRUE)
  file.copy(unlist(wrapped), dest)
  moved <- lapply(wrapped, function(sub) lapply(sub, function(p) file.path(dest, basename(p))))
  
  rec <- unwrapTerraList(moved, generalPath = dest)
  expect_equal(terra::crds(rec$a$b), terra::crds(tv$a$b))
})

# ---- Attribute fidelity ----
test_that("round-trip preserves CRS and attributes", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  
  dv <- create_dummy_vect()
  terra::crs(dv) <- "EPSG:3857"
  dv$mark <- factor(c("x","y","z"))
  tv <- list(z = list(d = dv))
  wrapped <- wrapTerraList(tv, generalPath = tmp, zipFiles = FALSE)
  rec <- unwrapTerraList(wrapped, generalPath = tmp)
  
  expect_equal(terra::crs(rec$z$d, proj=TRUE), terra::crs(dv, proj=TRUE))
  expect_equal(as.character(rec$z$d$mark), as.character(dv$mark))
})

# ---- Filename collision detection ----
test_that("wrapTerraList generates unique file names", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  
  tv <- list(g = list(a = create_dummy_vect(), b = create_dummy_vect()))
  
  # Stub rand_strings to produce distinct names on each call, avoiding infinite loop
  i <- 0
  mock_fn <- function(...) {
    i <<- i + 1
    rep(paste0("dup", i), 1)
  }
  mockery::stub(wrapTerraList, 'stringi::stri_rand_strings', mock_fn)
  
  res <- wrapTerraList(tv, generalPath = tmp, zipFiles = FALSE)
  paths <- unlist(res, use.names = FALSE)
  expect_equal(length(unique(paths)), length(paths), info = "File name collision detected")
})
