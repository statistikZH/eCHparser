test_that("Output of parsed file is dataframe", {

  testfiles <- system.file("extdata", package = "eCHparser") |>
    list.files()

  for (i in 1:length(testfiles)) {

    filepath <- system.file("extdata", testfiles[i], package = "eCHparser")

    file_out <- parse_eCH_0252(filepath)

    testthat::expect_true(is.data.frame(file_out))

  }

})
