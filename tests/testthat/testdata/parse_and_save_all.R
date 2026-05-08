# INFORMATION ==================================================================


# This script parses all test data and saves it in tests/testthat/testdata/expected/.
# So, it uses the functions of the package to generate the files in the expected folder based on the files in the input folder.


# CREATE RDS FILES =============================================================


type <- svDialogs::dlg_input("Which files (0157, 0252 or templates) would you like to parse?")$res |>
  tolower()

if (type %in% c("0157", "0252")) {
  slug <- paste0("/eCH-", type)
} else if (type == "templates") {
  slug <- "/election_templates"
}

testfilepath_in <- paste0(testthat::test_path("testdata/input"), slug)
testfiles <- list.files(testfilepath_in)


for (i in seq_along(testfiles)) {

  # Get the full file path
  file <- paste0(testfilepath_in, "/", testfiles[i])

  # Define election type (only relevant for template conversion)
  election_type <- ifelse(testfiles[i] == "maj_template.xlsx", "Majority", "Proportion")
  mandates <- ifelse(testfiles[i] == "maj_template.xlsx", 5, 10)

  # Process the file
  if (type == "0157") {

    # Build the two RDS files by parsing the input xml files; the xml files will be built later
    file_out <- parse_eCH_0157(file)

  } else if (type == "0252") {

    # Build the two RDS files by parsing the input xml files
    file_out <- parse_eCH_0252(file)

  } else if (type == "templates") {

    # Build the two RDS files by parsing the input xml files
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
  testfilepath_out <- testthat::test_path(paste0("testdata/expected", slug), paste0(sub("\\.[^.]*$", "", testfiles[i]), ".RDS")) # the sub() removes the .xml extension

  saveRDS(file_out, testfilepath_out)

}


# CREATE XML FILES =============================================================


if (type == "0157") {

  # Define path
  testfilepath_in <- paste0(testthat::test_path("testdata/expected"), slug)
  # Define files (only the RDS files that were created before)
  testfiles <- list.files(testfilepath_in)[grep(".RDS", list.files(testfilepath_in))]

  # Loop over all defined files
  for (j in seq_along(testfiles)) {

    # Get the full file path
    file <- paste0(testfilepath_in, "/", testfiles[j])

    # Load the file
    data <- readRDS(file)

    # Define the election type from the file name
    election_type <- ifelse(grepl("ajority", testfiles[j]), "Majority", "Proportion")

    # Build file
    file_out <- build_eCH_0157(data, election_type)

    # Load the corresponding RDS file path from the testdata folder (use testthat::test_path for the test to work also with devtools::test())
    testfilepath_out <- sub("([^_]+)(?=_election)", "stat", file, perl = TRUE)
    testfilepath_out <- sub(".RDS", ".xml", testfilepath_out)

    # Save file
    xml2::write_xml(file_out, testfilepath_out)

  }

}
