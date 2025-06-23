#' Convert an xlsx file into an XML file with the format eCH-0157
#'
#' @description
#' This function transforms an xlsx file in a defined structure into a xml file in the format eCH-0157.
#' Use the function "open_eCH_0157_xlsx" to open a blank template file.
#'
#' @param file Path to your xlsx file.
#' @param template_xml_path The path to the template xml file that provides the structure for the output xml file.
#'
#' @return An XML file.
#' @export
#'
#' @examples
#'
write_eCH_0157 <- function(file, template_xml_path = system.file("templates", "eCH_0157_template.xml", package = "eCHparser")){


  # !!!!!!! DEV-HELPER - DELETE AFTER DEV  =====================================


  test <- parse_eCH_0157("tests/testthat/testdata/files_unparsed/eCH-0157/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xml")
  writexl::write_xlsx(test, "/home/file-server/01_Post/Graf/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xlsx")
  file <- "/home/file-server/01_Post/Graf/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xlsx"
  target_xml <- xml2::read_xml("tests/testthat/testdata/files_unparsed/eCH-0157/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xml")
  target_list <- xml2::as_list(target_xml)


  # PREPARE DATA ===============================================================


  # Read xlsx file
  data <- readxl::read_xlsx(file) # This could/should be changed later so that the input is a tibble

  # Split data into contest...
  contest_tbl <- data |>
    dplyr::select(contest_contestIdentification:`contestDescriptionInfo-rm_contestDescription`) |>
    unique()

  # ... election group ballots, ...
  election_group_ballot_tbl <- data |>
    dplyr::select(
      electionGroupBallot_domainOfInfluenceIdentification,
      electionGroupBallot_index
    ) |>
    unique()

  # ...elections, and...
  election_tbl <- data |>
    dplyr::select(
      electionGroupBallot_index,
      election_electionIdentification:candidate_candidateIdentification,
      -candidate_candidateIdentification,
      electionGroupBallot_domainOfInfluenceIdentification # as link to the election group ballot
    ) |>
    unique()

  # ...candidates.
  candidates_tbl <- data |>
    dplyr::select(
      candidate_candidateIdentification:ncol(data),
      election_electionIdentification # as link to the election
    )


  # WRITE LIST =================================================================


  initialDelivery <- list(
    contest = create_contest_list(contest_tbl[1, ]), # Define contest list
    electionGroupBallot = lapply(data$electionGroupBallot_index |> unique(), function(egb_index){ # apply on all election group ballots

      electionGroupBallot_tbl <- data |>
        dplyr::filter(electionGroupBallot_index == egb_index)

      result = list(
        result = list(
        domainOfInfluenceIdentification = list(electionGroupBallot_tbl$electionGroupBallot_domainOfInfluenceIdentification[1])
      )
      )

      names(result) <- paste("electionGroupBallot", egb_index)

      return(result)

    })
  )




  # lapply over all unique rows of the election group ballot tbl
  # lapply over all unique rows of the election tbl
  # inside each of those, lapply over the rows of the election tbl




  # initialDelivery <- list()
  #
  # initialDelivery <- append(initialDelivery, list("contest"))


}





#' Create contest lists
#'
#' @description
#' This helper function transforms contest information to a contest list.
#'
#' @param data A tibble containing the necessary contest information for the eCH-0157.
#'
#' @return A list.
#' @export
#'
#' @examples
#'
create_contest_list <- function(data){


  # !!!!!!! DEV-HELPER - DELETE AFTER DEV  =====================================


  # data <- contest_tbl[1, ]


  # PREPARE NESTED LISTS =======================================================


  ## Multilingual Contest Description ------------------------------------------


  # Define and transform nested data for multilingual information
  data_nested <- transform_nested(data, "language")

  # Create lists
  contestDescription <- lapply(data_nested$language |> unique(), function(language) {

    # # Filter data
    # data_nested_language <- data_nested |>
    #   dplyr::filter(language == language)

    # Assemble list
    contestDescriptionInfo <- list(
      language = list(language),
      contestDescription = list(data_nested[data_nested$language == language, ]$value)
    )

  })

  # Name the sublists
  names(contestDescription) <- rep("contestDescriptionInfo", length(contestDescription))


  # ASSEMBLE ELECTION LIST =====================================================


  # Hardcode everything since structure is fixed
  contest_list <- list(
    contesIdentification = list(data$contest_contestIdentification),
    contestDate = list(data$contest_contestDate),
    contestDescription = contestDescription
  )

}





#' Create election lists
#'
#' @description
#' This helper function transforms election information to an election list.
#'
#' @param data A tibble containing the necessary election information for the eCH-0157.
#'
#' @return A list.
#' @export
#'
#' @examples
#'
create_election_list <- function(data){


  # !!!!!!! DEV-HELPER - DELETE AFTER DEV  =====================================


  # data <- election_tbl[1, ]


  # PREPARE NESTED LISTS =======================================================


  ## Referenced Election -------------------------------------------------------


  # Define and transform nested data for referenced elections and drop all rows with no value
  data_nested <- transform_nested(data, "relation") |>
    dplyr::filter(!is.na(value))

  referencedElection <- list(
    referencedElection = list(data_nested$value),
    electionRelation = list(data_nested$relation)
  )


  ## Multilingual Information --------------------------------------------------


  # Define and transform nested data for multilingual information
  data_nested <- transform_nested(data, "language")

  # Create lists
  electionDescription <- lapply(data_nested$language |> unique(), function(language) {

    # Define electionDescriptionShort
    electionDescriptionShort <- data_nested |>
      dplyr::filter(
        language == language,
        name_list_element == "electionDescriptionShort"
      )

    # Define electionDescription
    electionDescription <- data_nested |>
      dplyr::filter(
        language == language,
        name_list_element == "electionDescription"
      )

    # Assemble list
    electionDescriptionInfo <- list(
      language = list(language),
      electionDescriptionShort = list(electionDescriptionShort$value),
      electionDescription = list(electionDescription$value)
    )

  })

  # Name the sublists
  names(electionDescription) <- rep("electionDescriptionInfo", length(electionDescription))


  # ASSEMBLE ELECTION LIST =====================================================


  # Hardcode everything since structure is fixed
  election_list <- list(
    electionIdentification = list(data$election_electionIdentification),
    typeOfElection = list(data$election_typeOfElection),
    electionPosition = list(data$election_electionPosition),
    electionDescription = electionDescription,
    numberOfMandates = list(data$election_numberOfMandates),
    referencedElection = referencedElection
  )

}





#' Create candidate lists
#'
#' @description
#' This helper function transforms candidate information to a candidate list.
#'
#' @param data A tibble containing the necessary candidate information for the eCH-0157.
#'
#' @return A list.
#' @export
#'
#' @examples
#'
create_candidate_list <- function(data){


  # !!!!!!! DEV-HELPER - DELETE AFTER DEV  =====================================


  # data <- candidates_tbl[1, ]


  # PREPARE NESTED LISTS =======================================================


  # Define and transform nested multilingual data
  data_nested <- transform_nested(data, "language")

  # Create lists
  list <- lapply(data_nested$name_parent_element |> unique(), function(list_name) {

    # Filter data
    data_nested_parent <- data_nested |>
      dplyr::filter(name_parent_element == list_name)

    list <- lapply(1:nrow(data_nested_parent), function(element_number) {

      # Select relevant row
      data_nested_child <- data_nested_parent[element_number, ]

      # Build list
      list <- list(
        language = list(data_nested_child$language),
        dynamic_name = list(data_nested_child$value)
      )

      # Rename the second list element dynamically
      names(list)[2] <- data_nested_child$name_list_element

      return(list)

    })

    # Rename list elements
    names(list) <- rep(unique(data_nested_parent$name_parent_element), length(list))

    return(list)

  })


  # Split Up -------------------------------------------------------------------


  # Define names
  nested_list_names <- sub(pattern = "Info", replacement = "", unique(data_nested$name_parent_element))

  # Assign the names to the lists. This gives us separate lists for candidateTextInfo, occupationalTitleInfo, partyAffiliationInfo
  for (i in 1:length(nested_list_names)) {
    assign(nested_list_names[i], list[[i]])
  }


  # ASSEMBLE CANDIDATE LIST ====================================================


  # Hardcode everything since structure is fixed
  candidate_list <- list(
    candidateIdentification = list(data$candidate_candidateIdentification),
    familyName = list(data$candidate_familyName),
    firstName = list(data$candidate_firstName),
    callName = list(data$candidate_callName),
    candidateText = candidateText,
    dateOfBirth = list(data$candidate_dateOfBirth),
    sex = list(data$candidate_sex),
    occupationalTitle = occupationalTitle,
    dwellingAddress = list(
      street = list(data$dwellingAddress_street),
      houseNumber = list(data$dwellingAddress_houseNumber),
      town = list(data$dwellingAddress_town),
      swissZipCode = list(data$dwellingAddress_swissZipCode),
      country = list(
        countryId = list(data$country_countryId),
        countryIdISO2 = list(data$country_countryIdISO2),
        countryNameShort = list(data$country_countryNameShort)
      )
    ),
    swiss = list(
      origin = list(data$swiss_origin)
    ),
    mrMrs = list(data$candidate_mrMrs),
    title = list(data$candidate_title),
    languageOfCorrespondence = list(data$candidate_languageOfCorrespondence),
    candidateReference = list(data$candidate_candidateReference),
    partyAffiliation = partyAffiliation
  )

}





#' Prepare specified data for further processing
#'
#' @description
#' This helper function transforms specified columns into dataframes for further processing.
#'
#' @param data A tibble containing columns, for which a characteristic of the content is defined by a tag in the column title.
#' @param type A character vector defining the type of the specification. Either "language" or "relation".
#'
#' @return A tibble.
#' @export
#'
#' @examples
#'
transform_nested <- function(data, type) {

  # Select data based on the defined specification type
  if (type == "language") {
    selected_data <- data |>
      dplyr::select(grep("-([a-z])", names(data)))
  } else if (type == "relation") {
    selected_data <- data |>
      dplyr::select(grep("-([0-9])", names(data)))
  }

  # Transform data to long and add names for the different levels in the list later on
  transformed_data <- selected_data |>
    # dplyr::select(dplyr::contains("-")) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "name",
      values_to = "value"
    ) |>
    dplyr::mutate(
      name_list_element = sub(".*_", "", name),
      name_parent_element = sub("-.*", "", name)
    )

  # Add the specification
  if (type == "language") {
    transformed_data <- transformed_data |>
      dplyr::mutate(language = sub(".*-([a-zA-Z]{2}).*", "\\1", name))
  } else if (type == "relation") {
    transformed_data <- transformed_data |>
      dplyr::mutate(relation = sub(".*-([0-9]{1}).*", "\\1", name))
  }

  return(transformed_data)

}
