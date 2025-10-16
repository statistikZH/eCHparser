#' Convert an xlsx file into an XML file with the format eCH-0157
#'
#' @description
#' This function transforms an xlsx file in a defined structure into a xml file
#' in the format eCH-0157. Use the function "open_eCH_0157_xlsx" to open a
#' blank template file.
#'
#' @param file Path to your xlsx file.
#'
#' @return An XML file.
#' @export
#'
#' @examples
#'
write_eCH_0157 <- function(file){


  # PREPARE DATA ===============================================================


  # Read file template including delivery header
  template_path <- system.file("templates", "eCH-0157_majority_header_template.RDS", package = "eCHparser")
  delivery <- readRDS(template_path)

  # Update deliveryHeader
  delivery[[1]][["messageId"]] <- list(substr(paste0("STAT_", sub(" ", "T", as.character(Sys.time()))), 1, 36))
  delivery[[1]][["sendingApplication"]][["productVersion"]] <- list(substr(packageVersion("eCHparser"), 1, 10))
  delivery[[1]][["messageDate"]] <- list(sub(" ", "T", as.character(Sys.time())))

  # Read xlsx file
  data <- readxl::read_xlsx(file) # This could/should be changed later so that the input is a tibble

  # Load namespaces file
  ns_path <- system.file("templates", "eCH-0157_majority_namespaces.RDS", package = "eCHparser")
  ns_list <- readRDS(ns_path)


  # BUILD INITIAL DELIVERY LIST ================================================


  initialDelivery <- list(
    contest = create_contest_list(data[1, ]) # Define contest list
  )


  ## Build Election Group Ballot List ------------------------------------------


  electionGroupBallot <- lapply(unique(data$electionGroupBallot_index), function(egb_index){

    # Get the right data
    egb_data <- data |>
      dplyr::filter(electionGroupBallot_index == egb_index)

    # Define the single DOI-ID of the election group ballot (always a single one)
    domainOfInfluenceIdentification <- egb_data$electionGroupBallot_domainOfInfluenceIdentification[1]

    # Create the list
    electionGroupBallot <- list(
      domainOfInfluenceIdentification = list(domainOfInfluenceIdentification)
    )


    ## Build Election Information List -----------------------------------------


    # Apply over unique election identification in this specific election group ballot
    electionInformation <- lapply(unique(egb_data$election_electionIdentification), function(elec_id){

      # Get the right data
      elec_data <- egb_data |>
        dplyr::filter(election_electionIdentification == elec_id)

      # Define the election list (take the first element since the election information must be identical in every row)
      electionInformation <- list(election = create_election_list(elec_data[1, ]))


      ## Build Candidate Lists -------------------------------------------------


      # Apply over unique candidate identification in this specific election
      candidate <- lapply(unique(elec_data$candidate_candidateIdentification), function(cand_id){

        # Get the right data
        cand_data <- elec_data |>
          dplyr::filter(candidate_candidateIdentification == cand_id)

        # Define the candidate list (take the first element since nrow() must be 1 here)
        candidate <- create_candidate_list(cand_data[1, ])

      })


      # RENAME AND ASSEMBLE ====================================================


      # Set the names of the candidate lists
      names(candidate) <- rep("candidate", length(candidate))

      electionInformation <- c(electionInformation, candidate)

    })

    # Set the names of the election lists
    names(electionInformation) <- rep("electionInformation", length(electionInformation))

    electionGroupBallot <- c(electionGroupBallot, electionInformation)

  })

  # Set the names of the election group ballot list
  names(electionGroupBallot) <- rep("electionGroupBallot", length(electionGroupBallot))

  # Put together entire list
  initialDelivery <- c(initialDelivery, electionGroupBallot)

  # Add initialDelivery to the delivery
  delivery[[length(delivery) + 1]] <- initialDelivery
  names(delivery)[length(delivery)] <- "initialDelivery"

  # Clean the list (drop all empty and NULL elements as well as empty lists)
  delivery <- clean_list(delivery)

  # Add all namespaces
  delivery <- assign_namespaces_by_path(delivery, ns_list)
  attr(delivery, "xmlns:xsi") <- "http://www.w3.org/2001/XMLSchema-instance"
  attr(delivery, "xmlns:xsd") <- "http://www.w3.org/2001/XMLSchema"
  attr(delivery, "xmlns") <- "http://www.ech.ch/xmlns/eCH-0157/4"

  # Build root node of length() == 1
  delivery_list <- list(delivery)
  names(delivery_list) <- "delivery"

  # Turn to xml
  delivery_xml <- xml2::as_xml_document(delivery_list)



  # DEV: WRITE XML ===============

  xml2::write_xml(delivery_xml, "/home/file-server/01_Post/Graf/eCH_writer_output_0157.xml") # for dev purposes

  return(delivery_xml)

}





#' Create contest lists
#'
#' @description
#' This helper function transforms contest information to a contest list.
#'
#' @param cont_data A tibble containing the necessary contest information for
#' the eCH-0157.
#'
#' @return A list.
#' @export
#'
#' @examples
#'
create_contest_list <- function(cont_data){


  # PREPARE NESTED LISTS =======================================================


  ## Multilingual Contest Description ------------------------------------------


  # Define and transform nested data for multilingual information
  data_nested <- transform_nested(cont_data, "language")

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
    contestIdentification = list(cont_data$contest_contestIdentification),
    contestDate = list(cont_data$contest_contestDate),
    contestDescription = contestDescription
  )

  # Remove all NAs from the list
  contest_list <- contest_list[!sapply(contest_list, function(x) all(is.na(x)))]

}





#' Create election lists
#'
#' @description
#' This helper function transforms election information to an election list.
#'
#' @param elec_data A tibble containing the necessary election information for
#' the eCH-0157.
#'
#' @return A list.
#' @export
#'
#' @examples
#'
create_election_list <- function(elec_data){


  # PREPARE NESTED LISTS =======================================================


  ## Referenced Election -------------------------------------------------------


  # Define and transform nested data for referenced elections and drop all rows with no value and duplicate rows if there are multiple referenced elections
  data_nested <- transform_nested(elec_data, "relation") |>
    dplyr::filter(!is.na(value)) |>
    tidyr::separate_rows(value, sep = ", ")

  # Work through multiple referenced elections
  referencedElection <- lapply(1:nrow(data_nested), function(rownumber){
    referencedElection <- list(
      referencedElection = list(data_nested$value[rownumber]),
      electionRelation = list(data_nested$relation[rownumber])
      )
    })

  names(referencedElection) <- rep("referencedElection", length(referencedElection))


  ## Multilingual Information --------------------------------------------------


  # Define and transform nested data for multilingual information
  data_nested <- transform_nested(elec_data, "language")

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
    electionIdentification = list(elec_data$election_electionIdentification),
    typeOfElection = list(elec_data$election_typeOfElection),
    electionPosition = list(elec_data$election_electionPosition),
    electionDescription = electionDescription,
    numberOfMandates = list(elec_data$election_numberOfMandates)
  )

  # Add the referenced election(s) at the end
  election_list <- c(election_list, referencedElection)


  # Remove all NAs from the list
  election_list <- election_list[!sapply(election_list, function(x) all(is.na(x)))]

}





#' Create candidate lists
#'
#' @description
#' This helper function transforms candidate information to a candidate list.
#'
#' @param cand_data A tibble containing the necessary candidate information for
#' the eCH-0157.
#'
#' @return A list.
#' @export
#'
#' @examples
#'
create_candidate_list <- function(cand_data){


  # PREPARE NESTED LISTS =======================================================


  # Define and transform nested multilingual data and drop all empty value rows
  data_nested <- transform_nested(cand_data, "language") |>
    dplyr::filter(!is.na(value))

  # Check if there is data left and if so, transform it into separate lists
  if(nrow(data_nested > 0)) {

    # Create lists
    data_nested_list <- lapply(data_nested$name_parent_element |> unique(), function(list_name) {

      # Filter data
      data_nested_parent <- data_nested |>
        dplyr::filter(name_parent_element == list_name)

      data_nested_list <- lapply(1:nrow(data_nested_parent), function(element_number) {

        # Select relevant row
        data_nested_child <- data_nested_parent[element_number, ]

        # Build list
        data_nested_list <- list(
          language = list(data_nested_child$language),
          dynamic_name = list(data_nested_child$value)
        )

        # Rename the second list element dynamically
        names(data_nested_list)[2] <- data_nested_child$name_list_element

        return(data_nested_list)

      })

      # Rename list elements
      names(data_nested_list) <- rep(unique(data_nested_parent$name_parent_element), length(data_nested_list))

      return(data_nested_list)

    })


    # Split Up -------------------------------------------------------------------


    # Define names
    nested_list_names <- sub(pattern = "Info", replacement = "", unique(data_nested$name_parent_element))

    # Assign the names to the lists. This gives us separate lists for candidateTextInfo, occupationalTitleInfo, partyAffiliationInfo
    for (i in 1:length(nested_list_names)) {
      assign(nested_list_names[i], data_nested_list[[i]])
    }

  }


  # ASSEMBLE CANDIDATE LIST ====================================================

# browser()
  # Hardcode everything since structure is fixed (fill in NAs if there was no nested language info)
  candidate_list <- list(
    candidateIdentification = list(cand_data$candidate_candidateIdentification),
    familyName = list(cand_data$candidate_familyName),
    firstName = list(cand_data$candidate_firstName),
    callName = list(cand_data$candidate_callName),
    candidateText = if(exists("candidateText")) candidateText else NA,
    dateOfBirth = list(cand_data$candidate_dateOfBirth),
    sex = list(cand_data$candidate_sex),
    occupationalTitle = if(exists("occupationalTitle")) occupationalTitle else NA,
    dwellingAddress = list(
      street = list(cand_data$dwellingAddress_street),
      houseNumber = list(cand_data$dwellingAddress_houseNumber),
      town = list(cand_data$dwellingAddress_town),
      swissZipCode = list(cand_data$dwellingAddress_swissZipCode),
      country = list(
        countryId = list(cand_data$country_countryId),
        countryIdISO2 = list(cand_data$country_countryIdISO2),
        countryNameShort = list(cand_data$country_countryNameShort)
      )
    ),
    swiss = list(
      origin = list(cand_data$swiss_origin)
    ),
    mrMrs = list(cand_data$candidate_mrMrs),
    title = list(cand_data$candidate_title),
    languageOfCorrespondence = list(cand_data$candidate_languageOfCorrespondence),
    candidateReference = list(cand_data$candidate_candidateReference),
    partyAffiliation = if (exists("partyAffiliation")) partyAffiliation else NA
  )

  # Remove all NAs from the list
  candidate_list <- candidate_list[!sapply(candidate_list, function(x) all(is.na(x)))]

}





#' Create list lists
#'
#' @description
#' This helper function transforms list information to a list list.
#'
#' @param cand_data A tibble containing the necessary list information for the
#' eCH-0157.
#'
#' @return A list.
#' @export
#'
#' @examples
#'
create_list_list <- function(list_data){
  list_data
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
transform_nested <- function(nested_data, type) {

  # Select data based on the defined specification type
  if (type == "language") {
    selected_data <- nested_data |>
      dplyr::select(grep("-([a-z])", names(nested_data)))
  } else if (type == "relation") {
    selected_data <- nested_data |>
      dplyr::select(grep("-([0-9])", names(nested_data)))
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
