test_that("Parsed file output is a dataframe and matches corresponding RDS file", {

  # Get the list of unparsed test files
  testfiles <- list.files(testthat::test_path("testdata/eCH-0157/files_unparsed"))

  # Initialize a list to capture errors
  errors <- list()

  for (i in seq_along(testfiles)) {

    # Get the full file path
    filepath <- testthat::test_path("testdata/eCH-0157/files_unparsed", paste0(testfiles[i]))

    # Parse the file using the parse_eCH_0252 function
    file_out <- parse_eCH_0157(filepath)

    # Ensure the output is a data frame
    testthat::expect_true(is.data.frame(file_out), info = paste("File:", testfiles[i]))

    # Load the corresponding RDS file from the testdata folder (use testthat::test_path for the test to work also with devtools::test())
    rds_filepath <- testthat::test_path("testdata/eCH-0157/files_parsed", paste0(sub("\\.[^.]*$", "", testfiles[i]), ".RDS")) # the sub() removes the .xml extension

    if (file.exists(rds_filepath)) {
      expected_out <- readRDS(rds_filepath)

      # Check if the parsed output matches the expected RDS content
      tryCatch({

        testthat::expect_equal(file_out, expected_out, info = paste("File:", testfiles[i]))

      }, error = function(e) {

        # Collect errors in the list for reporting later
        errors[[testfiles[i]]] <- e$message

      })

    } else {

      # return a warning message if the corresponding RDS file was not found
      warning(paste("No corresponding RDS file found for", testfiles[i]))

    }

  }

  # Report all errors if there are any
  if (length(errors) > 0) {

    stop(paste("Errors found in the following files:", paste(names(errors), collapse = ", "), "\n", paste(errors, collapse = "\n")))

  }

})
