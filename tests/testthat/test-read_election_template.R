test_that("Read in file matches corresponding RDS file", {

  # Get the list of unparsed test files
  testfiles <- list.files(testthat::test_path("testdata/files_unparsed/election_templates"))

  # Initialize a new environment (necessary to actually assign values inside of tryCatch)
  errorenv <- new.env()

  # Initialize a list in a new environment to capture errors (new env is necessary to actually assign values inside of tryCatch)
  errorenv$errors <- list()

  for (i in seq_along(testfiles)) {

    # Get the full file path
    filepath <- testthat::test_path("testdata/files_unparsed/election_templates", paste0(testfiles[i]))

    # Define election type (only relevant for template conversion)
    election_type <- ifelse(testfiles[i] == "maj_template.xlsx", "Majority", "Proportion")
    mandates <- ifelse(testfiles[i] == "maj_template.xlsx", 5, 10)

    # Read and transform file
    file_out <- read_election_template(
      input_path = filepath,
      election_type = election_type,
      date = "2030-01-01",
      election_title_short = "Testtitle short",
      election_title_long = "Testtitle long",
      mandates = mandates
    )

    # Load the corresponding RDS file from the testdata folder (use testthat::test_path for the test to work also with devtools::test())
    rds_filepath <- testthat::test_path("testdata/files_parsed/election_templates", paste0(sub("\\.[^.]*$", "", testfiles[i]), ".RDS")) # the sub() removes the .xsls extension

    if (file.exists(rds_filepath)) {
      expected_out <- readRDS(rds_filepath)

      # Check if the parsed output matches the expected RDS content
      tryCatch({

        testthat::expect_equal(file_out, expected_out, info = paste("File:", testfiles[i]))

      }, error = function(e) {

        # Collect errors in the list for reporting later
        errorenv$errors[[testfiles[i]]] <- e$message

      })

    } else {

      # return a warning message if the corresponding RDS file was not found
      warning(paste("No corresponding RDS file found for", testfiles[i]))

    }

  }

  # Report all errors if there are any
  if (length(errorenv$errors) > 0) {

    stop(paste(
      "Errors found in the following files:",
      paste(names(errorenv$errors), collapse = ", "),
      "\n",
      paste(errorenv$errors, collapse = "\n")
    ))

  }

  # Remove error environment
  rm(errorenv)

})
