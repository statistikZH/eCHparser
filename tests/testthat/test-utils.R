test_that("get_election_template works correctly", {
  # Test with "Proportion" election type
  template_prop <- get_election_template("Proportion")
  expect_true(is.data.frame(template_prop))

  # Test with "Majority" election type
  template_majority <- get_election_template("Majority")
  expect_true(is.data.frame(template_majority))

  # Test with "proportion" (lowercase) election type
  template_prop_lower <- get_election_template("proportion")
  expect_true(is.data.frame(template_prop_lower))

  # Test with "majority" (lowercase) election type
  template_majority_lower <- get_election_template("majority")
  expect_true(is.data.frame(template_majority_lower))
})

test_that("get_election_template handles invalid input correctly", {
  # Test with invalid election type
  expect_error(
    get_election_template("InvalidType"),
    "The parameter \"election_type\" must be either \"Majority\" or \"Proportion\""
  )
})
