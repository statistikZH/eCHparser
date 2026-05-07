test_that("Built eCH-0157 file output is an XML and matches corresponding file", {

  # Get the list of parsed test files
  testfiles <- list.files(testthat::test_path("testdata/expected/eCH-0157"))

  # Define election types for test files
  election_types <- c("Majority", "Proportion")

  # Initialize a new environment (necessary to actually assign values inside of tryCatch)
  errorenv <- new.env()

  # Initialize a list in a new environment to capture errors (new env is necessary to actually assign values inside of tryCatch)
  errorenv$errors <- list()

  for (i in seq_along(testfiles)) {

    # Get the full file path
    filepath <- testthat::test_path("testdata/expected/eCH-0157", paste0(testfiles[i]))

    # Load and transform file
    file <- readRDS(filepath)
    file_out <- build_eCH_0157(file, election_types[i])

    # # Ensure the output is a data frame
    # testthat::expect_true(is.data.frame(file_out), info = paste("File:", testfiles[i]))

    # Load the corresponding RDS file from the testdata folder (use testthat::test_path for the test to work also with devtools::test())
    xml_filepath <- testthat::test_path("testdata/input/eCH-0157", paste0(sub("\\.[^.]*$", "", testfiles[i]), ".xml")) # the sub() removes the .xml extension

    if (file.exists(xml_filepath)) {
      expected_out <- xml2::read_xml(xml_filepath)

      # Check if the parsed output matches the expected RDS content
      tryCatch({

        testthat::expect_equal(
          xml2::as_list(file_out)[[1]][[2]],
          xml2::as_list(expected_out)[[1]][[2]],
          info = paste("File:", testfiles[i])
        )

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
