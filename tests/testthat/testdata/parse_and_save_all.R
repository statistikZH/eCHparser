# INFORMATION ==================================================================


# This script parses all testing data and saves it in tests/testthat/testdata.


# PARSE ALL TEST FILES AND SAVE THEM ===========================================

eCH_type <- svDialogs::dlg_input("Which eCH-type would you like to parse?")$res
testfiles <- list.files(testthat::test_path(paste0("testdata/eCH-", eCH_type, "/files_unparsed")))

for (i in seq_along(testfiles)) {

  # Get the full file path
  filepath <- testthat::test_path("testdata/files_unparsed", paste0(testfiles[i]))

  # Parse the file using the parse_eCH_0252 function
  file_out <- parse_eCH_0252(filepath)

  # Load the corresponding RDS file from the testdata folder (use testthat::test_path for the test to work also with devtools::test())
  rds_filepath <- testthat::test_path("testdata/files_parsed", paste0(sub("\\.[^.]*$", "", testfiles[i]), ".RDS")) # the sub() removes the .xml extension

  saveRDS(file_out, rds_filepath)

}
