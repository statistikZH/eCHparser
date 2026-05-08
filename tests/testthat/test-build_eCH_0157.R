test_that("Built eCH-0157 file output is an XML and matches corresponding file", {

  # Get the list of tabular .RDS files (that were parsed from original eCH-0157 files)
  input_files <- list.files(testthat::test_path("testdata/expected/eCH-0157"))[grep(".RDS", list.files(testthat::test_path("testdata/expected/eCH-0157")))]

  # Get the list of expected output files (from the same folder)
  expected_files <- list.files(testthat::test_path("testdata/expected/eCH-0157"))[grep(".xml", list.files(testthat::test_path("testdata/expected/eCH-0157")))]

  # Define election types for test files
  election_types <- c("Majority", "Proportion")

  # Initialize a new environment (necessary to actually assign values inside of tryCatch)
  errorenv <- new.env()

  # Initialize a list in a new environment to capture errors (new env is necessary to actually assign values inside of tryCatch)
  errorenv$errors <- list()

  for (i in seq_along(input_files)) {

    # Get the full file path
    filepath <- testthat::test_path("testdata/expected/eCH-0157", paste0(input_files[i]))

    # Load and transform file
    file <- readRDS(filepath)
    file_out <- build_eCH_0157(file, election_types[i])

    # Load the corresponding xml file from the same folder
    xml_filepath <- testthat::test_path("testdata/expected/eCH-0157", paste0(expected_files[i]))

    if (file.exists(xml_filepath)) {
      expected_out <- xml2::read_xml(xml_filepath)

      # Check if the parsed output matches the expected RDS content
      tryCatch({

        testthat::expect_equal(
          xml2::as_list(file_out)[[1]][[2]],
          xml2::as_list(expected_out)[[1]][[2]],
          info = paste("File:", input_files[i])
        )

      }, error = function(e) {

        # Collect errors in the list for reporting later
        errorenv$errors[[input_files[i]]] <- e$message

      })

    } else {

      # return a warning message if the corresponding RDS file was not found
      warning(paste("No corresponding RDS file found for", input_files[i]))

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
