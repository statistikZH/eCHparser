# INFORMATION ==================================================================


# This script parses all testing data and saves it in tests/testthat/testdata.


# PARSE ALL TEST FILES AND SAVE THEM ===========================================


testfiles <- system.file("extdata", package = "eCHparser") |>
  list.files()

for (i in seq_along(testfiles)) {

  # Get the full file path
  filepath <- system.file("extdata", testfiles[i], package = "eCHparser")

  # Parse the file using the parse_eCH_0252 function
  file_out <- parse_eCH_0252(filepath)

  # Load the corresponding RDS file from the testdata folder (use testthat::test_path for the test to work also with devtools::test())
  rds_filepath <- testthat::test_path("testdata", paste0(sub("\\.[^.]*$", "", testfiles[i]), ".RDS")) # the sub() removes the .xml extension

  saveRDS(file_out, rds_filepath)

}
