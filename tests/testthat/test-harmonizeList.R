# tests/testthat/test-harmonizeList.R
library(testthat)
library(terra)

# Helper: tiny square polygon at (xmin, ymin) with given size
mk_sq <- function(xmin, ymin, size = 1, crs = "EPSG:3857") {
  coords <- matrix(
    c(xmin, ymin,
      xmin + size, ymin,
      xmin + size, ymin + size,
      xmin, ymin + size,
      xmin, ymin),
    ncol = 2, byrow = TRUE
  )
  terra::vect(coords, type = "polygons", crs = crs)
}

test_that("harmonizeList: errors on NULL", {
  skip_on_cran()
  expect_error(harmonizeList(NULL, whatNotToCombine = "potential"),
               "can't be NULL")
})

test_that("harmonizeList: returns sector unchanged when there are no duplicate dataClass names", {
  skip_on_cran()
  
  # Sector 'mining' has unique names: 'claims', 'permits'
  mining_unique <- setNames(
    list(mk_sq(0, 0), mk_sq(2, 0)),
    c("claims", "permits")
  )
  
  # Sector 'forestry' has unique names: 'cutblocks', 'roads'
  forestry_unique <- setNames(
    list(mk_sq(0, 2), mk_sq(2, 2)),
    c("cutblocks", "roads")
  )
  
  disturbances <- list(mining = mining_unique,
                       forestry = forestry_unique)
  
  out <- harmonizeList(disturbances, whatNotToCombine = "potential")
  
  # Sector names preserved
  expect_setequal(names(out), names(disturbances))
  
  # No merging: same set of inner names per sector
  expect_setequal(names(out$mining),   c("claims", "permits"))
  expect_setequal(names(out$forestry), c("cutblocks", "roads"))
  
  # Geometry counts unchanged
  expect_equal(nrow(out$mining$claims),   nrow(disturbances$mining$claims))
  expect_equal(nrow(out$mining$permits),  nrow(disturbances$mining$permits))
  expect_equal(nrow(out$forestry$cutblocks), nrow(disturbances$forestry$cutblocks))
  expect_equal(nrow(out$forestry$roads),     nrow(disturbances$forestry$roads))
})

test_that("harmonizeList: merges duplicate classes except those matching whatNotToCombine", {
  skip_on_cran()
  
  # Sector 'mining' has 2 duplicate 'claims' + single 'permits'
  mining_dup <- setNames(
    list(mk_sq(0, 0), mk_sq(2, 0), mk_sq(4, 0)),
    c("claims", "claims", "permits")
  )
  
  # Sector 'forestry' has 2 duplicate 'roads' and 2 duplicate 'potentialCutblocks'
  # 'potentialCutblocks' SHOULD NOT be merged (excluded by whatNotToCombine = 'potential')
  forestry_mix <- setNames(
    list(mk_sq(0, 2), mk_sq(2, 2), mk_sq(4, 2), mk_sq(6, 2)),
    c("roads", "roads", "potentialCutblocks", "potentialCutblocks")
  )
  
  disturbances <- list(mining = mining_dup,
                       forestry = forestry_mix)
  
  out <- harmonizeList(disturbances, whatNotToCombine = "potential")
  
  # --- mining expectations ---
  # duplicates 'claims' merged to a single entry; 'permits' kept
  expect_setequal(names(out$mining), c("claims", "permits"))
  # merged 'claims' should have 2 features = sum of inputs
  expect_equal(nrow(out$mining$claims), nrow(mining_dup[[1]]) + nrow(mining_dup[[2]]))
  expect_equal(nrow(out$mining$permits), nrow(mining_dup[[3]]))
  
  # --- forestry expectations ---
  # 'roads' duplicates merged -> single 'roads'
  # 'potentialCutblocks' duplicates NOT merged -> both remain present
  expect_true("roads" %in% names(out$forestry))
  # Count how many potentialCutblocks entries remain
  n_pot <- sum(names(out$forestry) == "potentialCutblocks")
  expect_equal(n_pot, 2)
  
  # merged 'roads' should have 2 features = sum of inputs
  # first occurrence by name is fine to access
  roads_merged <- out$forestry$roads
  expect_equal(nrow(roads_merged), 2L)
})

test_that("harmonizeList: honors multiple whatNotToCombine patterns", {
  skip_on_cran()
  
  # Two duplicate 'roads' and two duplicate 'potentialX'
  forestry <- setNames(
    list(mk_sq(0, 0), mk_sq(1.5, 0), mk_sq(0, 2), mk_sq(1.5, 2)),
    c("roads", "roads", "potentialX", "potentialX")
  )
  disturbances <- list(forestry = forestry)
  
  # Exclude both 'potential' AND 'roads' from merging
  out <- harmonizeList(disturbances, whatNotToCombine = c("potential", "^roads$"))
  
  # Both duplicates should remain as duplicates (no merge)
  expect_equal(sum(names(out$forestry) == "roads"), 2)
  expect_equal(sum(names(out$forestry) == "potentialX"), 2)
})

# --- More edge cases for harmonizeList ---------------------------------------
library(testthat)
library(terra)

mk_sq <- mk_sq # reuse helper from previous file, or re-define if needed

test_that("harmonizeList: empty disturbances returns empty list", {
  skip_on_cran()
  out <- harmonizeList(list(), whatNotToCombine = "potential")
  expect_type(out, "list")
  expect_length(out, 0)
})

test_that("harmonizeList: sector with only excluded classes is returned with duplicates intact", {
  skip_on_cran()
  
  forestry_only_potential <- setNames(
    list(mk_sq(0, 0), mk_sq(1.5, 0), mk_sq(3, 0)),
    c("potentialCutblocks", "potentialCutblocks", "potentialCutblocks")
  )
  disturbances <- list(forestry = forestry_only_potential)
  
  out <- harmonizeList(disturbances, whatNotToCombine = "potential")
  
  # All 3 duplicates remain — nothing merged
  expect_equal(sum(names(out$forestry) == "potentialCutblocks"), 3)
  # Geometry counts preserved
  expect_equal(nrow(out$forestry[[1]]), 1)
  expect_equal(nrow(out$forestry[[2]]), 1)
  expect_equal(nrow(out$forestry[[3]]), 1)
})

test_that("harmonizeList: exclusion is case-sensitive by default (Potential vs potential)", {
  skip_on_cran()
  
  forestry_mixed_case <- setNames(
    list(mk_sq(0, 0), mk_sq(1.5, 0)),
    c("PotentialCutblocks", "PotentialCutblocks")
  )
  disturbances <- list(forestry = forestry_mixed_case)
  
  # Pattern 'potential' does NOT match 'Potential...' with default grepl (case-sensitive)
  out <- harmonizeList(disturbances, whatNotToCombine = "potential")
  
  # Since not excluded, duplicates should merge down to one layer
  expect_equal(sum(names(out$forestry) == "PotentialCutblocks"), 1)
  expect_equal(nrow(out$forestry$PotentialCutblocks), 2L)
  
  # If we wanted case-insensitive exclusion, users can pass a regex that handles it:
  out_ci <- harmonizeList(disturbances, whatNotToCombine = "(?i)potential")
  expect_equal(sum(names(out_ci$forestry) == "PotentialCutblocks"), 2)
})

test_that("harmonizeList: merging duplicates with different attribute schemas yields union with NAs", {
  skip_on_cran()
  
  a <- mk_sq(0, 0);  a$id <- 1L
  b <- mk_sq(2, 0);  b$val <- 10.5
  c <- mk_sq(4, 0);  c$id <- 3L; c$val <- 20.0
  
  # Three duplicates of 'roads' with different columns
  forestry <- setNames(list(a, b, c), c("roads", "roads", "roads"))
  disturbances <- list(forestry = forestry)
  
  out <- harmonizeList(disturbances, whatNotToCombine = "potential")
  merged <- out$forestry$roads
  
  # Expect 3 features total after merge
  expect_equal(nrow(merged), 3L)
  # Expect union of columns 'id' and 'val' both present
  expect_true(all(c("id", "val") %in% names(merged)))
  # Check NA filling works (2nd feature had no 'id', 1st had no 'val')
  # NOTE: Use terra's extraction to data.frame to avoid factor/levels surprises
  df <- as.data.frame(merged)
  expect_true(is.na(df$id[2]))
  expect_true(is.na(df$val[1]))
})

