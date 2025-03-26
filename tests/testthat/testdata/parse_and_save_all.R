# INFORMATION ==================================================================


# This script parses all testing data and saves it in tests/testthat/testdata.


# PARSE ALL TEST FILES AND SAVE THEM ===========================================

eCH_type <- svDialogs::dlg_input("Which eCH-type would you like to parse?")$res
testfiles <- list.files(testthat::test_path(paste0("testdata/files_unparsed/eCH-", eCH_type)))

for (i in seq_along(testfiles)) {

  # Get the full file path
  filepath <- testthat::test_path(paste0("testdata/files_unparsed/eCH-", eCH_type), paste0(testfiles[i]))

  # Parse the file
  if (eCH_type == "0157") {
    file_out <- parse_eCH_0157(filepath)
  } else if (eCH_type == "0252") {
    file_out <- parse_eCH_0252(filepath)
  } else {
    stop(paste0("No function found for filetype eCH-", eCH_type))
  }

  # Load the corresponding RDS file from the testdata folder (use testthat::test_path for the test to work also with devtools::test())
  rds_filepath <- testthat::test_path(paste0("testdata/files_parsed/eCH-", eCH_type), paste0(sub("\\.[^.]*$", "", testfiles[i]), ".RDS")) # the sub() removes the .xml extension

  saveRDS(file_out, rds_filepath)

}
