# Please build your own test file from test-template.R, and place it in tests folder
# please specify the package you need to run the sim function in the test files.

### Setup ######################################################################
# List of packages to install and load
pkgs <- c("covr", "DT", "htmltools", "htmlwidgets", "testthat", "terra", "data.table", "qs", "sf", "withr", "reproducible", "raster", "tictoc", "mockery")

# Check which packages are not installed
to_install <- setdiff(pkgs, rownames(installed.packages()))

# Install the missing packages
if (length(to_install)) install.packages(to_install)

# Load all the packages
lapply(pkgs, library, character.only = TRUE)

Paths <<- list(
  inputPath  = tempdir(),
  outputPath = tempdir()
)
r_scripts <- list.files("R", full.names = TRUE, pattern = "\\.R$")
invisible(lapply(r_scripts, source))

### Testing ####################################################################
# to test all the test files in the tests folder:
testthat::test_dir(file.path("tests", "testthat"))

### Test Coverage ##############################################################
src <- list.files("R", pattern = "\\.R$", full.names = TRUE)
tst <- list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE)

cov <- file_coverage(source_files = src, test_files = tst)
percent_coverage(cov)
covr::report(cov, file = "coverage.html", browse = interactive())