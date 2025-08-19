# Please build your own test file from test-template.R, and place it in tests folder
# please specify the package you need to run the sim function in the test files.

### Setup ######################################################################
Paths <<- list(
  inputPath  = tempdir(),
  outputPath = tempdir()
)
r_scripts <- list.files("R", full.names = TRUE, pattern = "\\.R$")
invisible(lapply(r_scripts, source))

# to test all the test files in the tests folder:
testthat::test_dir(file.path("tests", "testthat"))
