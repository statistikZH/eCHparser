# INFORMATION ==================================================================


# This script parses all testing data and saves it in tests/testthat/testdata.


# PARSE ALL TEST FILES AND SAVE THEM ===========================================

type <- svDialogs::dlg_input("Which files (0157, 0252 or templates) would you like to parse?")$res

if (type %in% c("0157", "0252")) {
  slug <- paste0("/eCH-", type)
} else {
  slug <- "/election_templates"
}

testfilepath_in <- paste0(testthat::test_path("testdata/files_unparsed"), slug)
testfiles <- list.files(testfilepath_in)


for (i in seq_along(testfiles)) {

  # Get the full file path
  file <- paste0(testfilepath_in, "/", testfiles[i])

  # Define election type (only relevant for template conversion)
  election_type <- ifelse(testfiles[i] == "maj_template.xlsx", "Majority", "Proportion")
  mandates <- ifelse(testfiles[i] == "maj_template.xlsx", 5, 10)

  # Parse the file
  if (type == "0157") {
    file_out <- parse_eCH_0157(file)
  } else if (type == "0252") {
    file_out <- parse_eCH_0252(file)
  } else if (type == "templates") {
    file_out <- read_election_template(
      input_path = file,
      election_type = election_type,
      date = "2030-01-01",
      election_title_short = "Testtitle short",
      election_title_long = "Testtitle long",
      mandates = mandates
    )
  } else {
    stop(paste0("No function found for filetype eCH-", eCH_type))
  }

  # Load the corresponding RDS file path from the testdata folder (use testthat::test_path for the test to work also with devtools::test())
  testfilepath_out <- testthat::test_path(paste0("testdata/files_parsed", slug), paste0(sub("\\.[^.]*$", "", testfiles[i]), ".RDS")) # the sub() removes the .xml extension

  saveRDS(file_out, testfilepath_out)

}
