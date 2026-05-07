#' Convert an dataframe into an XML file with the format eCH-0157
#'
#' @description
#' This function transforms an xlsx file in a defined structure into a xml file
#' in the format eCH-0157. Use the function "open_eCH_0157_xlsx" to open a
#' blank template file.
#'
#' @param data A dataframe in the required format.
#' @inheritParams get_election_template
#'
#' @return An XML file.
#' @export
#'
#' @examples
#' \dontrun{
#' my_xml_file <- build_eCH_0157(my_df)
#' }
#'
build_eCH_0157 <- function(data, election_type){


  # PREPARE DATA ===============================================================


  election_type <- tolower(election_type)

  if (!election_type %in% c("proportion", "majority")) {
    stop("The election_type has to be either \"Proportion\" or \"Majority\"!")
  }

  # Read file template including delivery header
  header_template_path <- system.file("templates", "eCH-0157_header_template.RDS", package = "eCHparser")
  delivery <- readRDS(header_template_path)

  # Update deliveryHeader
  delivery[[1]][["messageId"]] <- list(substr(paste0("STAT_", sub(" ", "T", as.character(Sys.time()))), 1, 36))
  delivery[[1]][["sendingApplication"]][["productVersion"]] <- list(substr(packageVersion("eCHparser"), 1, 10))
  delivery[[1]][["messageDate"]] <- list(sub(" ", "T", as.character(Sys.time())))

  # Load namespaces file
  ns_path <- system.file("templates", paste0("eCH-0157_", election_type, "_namespaces.RDS"), package = "eCHparser")
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

      # Name sublists
      names(candidate) <- rep("candidate", length(candidate))


      ## Build List Information List -------------------------------------------


      # Only build this if lists are present in data
      if ("list_listIdentification" %in% names(elec_data)) {
        # Apply over unique list identification in this specific election
        list <- lapply(unique(elec_data$list_listIdentification), function(list_id){

          # Get the right data
          list_data <- elec_data |>
            dplyr::filter(list_listIdentification == list_id)

          # Define the list info list (take the first element since nrow() must be 1 here)
          list <- create_list_list(list_data)

        })

        # Name sublists
        names(list) <- rep("list", length(list))

      }


      # ASSEMBLE LIST ==========================================================


      # Add to electionInformation
      electionInformation <- c(electionInformation, candidate)

      # If we have list data, add to electionInformation
      if ("list_listIdentification" %in% names(elec_data)) {
        electionInformation <- c(electionInformation, list)
      }

      # return(electionInformation)
      electionInformation

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
#'
create_contest_list <- function(cont_data){


  # PREPARE NESTED LISTS =======================================================


  ## Multilingual Contest Description ------------------------------------------


  # Define and transform nested data for multilingual information
  data_nested <- transform_nested(cont_data, "language") |>
    # Filter to include only contest descriptions
    dplyr::filter(name_list_element == "contestDescription")

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
#'
create_election_list <- function(elec_data){


  # PREPARE NESTED LISTS =======================================================


  ## Referenced Election -------------------------------------------------------


  # Check names for nested data of type relation
  if (any(grepl("-([0-9])", names(elec_data)))) {

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

  }


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

  # Add the referenced election(s) at the end if object exists
  if (exists("referencedElection")) {
    election_list <- c(election_list, referencedElection)
  }

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
#'
create_candidate_list <- function(cand_data){


  # PREPARE NESTED LISTS =======================================================


  # Define and transform nested multilingual data and drop all empty value rows
  data_nested <- transform_nested(cand_data, "language") |>
    dplyr::filter(!is.na(value))

  # Check if there is data left and if so, transform it into separate lists
  if(nrow(data_nested) > 0) {

    result_list <- listify_language_table(data_nested)

    # Unpack list
    for (i in 1:length(result_list)) {
      sublist <- result_list[[i]]
      assign(names(result_list)[i], sublist)
    }

  }


  # ASSEMBLE CANDIDATE LIST ====================================================


  # Hardcode everything since structure is fixed (fill in NAs if there was no nested language info)
  candidate_list <- list(
    candidateIdentification = list(cand_data$candidate_candidateIdentification),
    familyName = list(cand_data$candidate_familyName),
    firstName = list(cand_data$candidate_firstName),
    callName = list(cand_data$candidate_callName),
    candidateText = if(exists("candidateText")) candidateText else NA, # not needed but does not hurt atm
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
    incumbentYesNo = list(cand_data$candidate_incumbentYesNo),
    candidateReference = list(cand_data$candidate_candidateReference),
    partyAffiliation = if (exists("partyAffiliation")) partyAffiliation else NA
  )

  # Remove all NAs from the list
  candidate_list <- candidate_list[!sapply(candidate_list, function(x) all(is.na(x)))]

}





#' Create list information lists
#'
#' @description
#' This helper function transforms list information to a list information list.
#'
#' @param list_data A tibble containing the necessary list information for
#' the eCH-0157.
#'
#' @return A list.
#'
create_list_list <- function(list_data){


  # PREPARE LIST LEVEL INFORMATION =============================================


  # Take the first row of the list level nested information and parse it
  list_info_nested_data <- list_data[1, grep("listDescriptionInfo-", names(list_data))]
  data_nested <- transform_nested(list_info_nested_data, "language")

  # Check if there is data left and if so, transform it into separate lists
  if(nrow(data_nested) > 0) {

    result_list <- listify_language_table(data_nested)

    # Unpack list
    for (i in 1:length(result_list)) {
      sublist <- result_list[[i]]
      assign(names(result_list)[i], sublist)
    }

  }


  # PREPARE CANDIDATE INFORMATION ==============================================


  candidate_position_list <- lapply(1:nrow(list_data), function(candidate) {

    # Select data
    list_data <- list_data[candidate, ]

    # Take the first row of the list level nested information and parse it
    candidate_position_nested_data <- list_data[1, grep("candidateTextInfo-", names(list_data))]
    data_nested <- transform_nested(candidate_position_nested_data, "language")

    # Check if there is data left and if so, transform it into separate lists
    if(nrow(data_nested) > 0) {

      result_list <- listify_language_table(data_nested)

      # Unpack list
      for (i in 1:length(result_list)) {
        sublist <- result_list[[i]]
        assign(names(result_list)[i], sublist)
      }

    }

    # Write list (Hardcode everything since structure is fixed)
    candidatePosition <- list(
      positionOnList = list(list_data$candidatePosition_positionOnList),
      candidateReferenceOnPosition = list(list_data$candidate_candidateReference),
      candidateIdentification = list(list_data$candidate_candidateIdentification),
      candidateTextOnPosition = if(exists("candidateText")) candidateText else NA
    )

  })

  # Name elements
  names(candidate_position_list) <- rep("candidatePosition", length(candidate_position_list))


  # ASSEMBLE LIST LIST =========================================================


  # Hardcode everything since structure is fixed (fill in NAs if there was no nested language info)
  list_list <- list(
    listIdentification = list(list_data$list_listIdentification),
    # # if we use the pasted listIdentification, in some cases, the string length
    # # gets bigger than the allowed 50 characters. I fixed it here and not at the
    # # original paste(), because i saw, that you make a some matchings in the process.
    # # Seemed the easiest location for the quick-fix
    # listIdentification = list(gsub("-.*", "",list_data$list_listIdentification)),
    listIndentureNumber = list(list_data$list_listIndentureNumber),
    listDescription = if(exists("listDescription")) listDescription else NA,
    isEmptyList = list("false"),
    listOrderOfPrecedence = list(as.numeric(list_data$list_listIndentureNumber)[1]),
    totalPositionsOnList = list(as.numeric(list_data$list_totalPositionsOnList)[1])
  )

  # Add candidate list information
  list_list <- c(list_list, candidate_position_list)

  return(list_list)

}





#' Prepare specified data for further processing
#'
#' @description
#' This helper function transforms specified columns into dataframes for
#' further processing.
#'
#' @param nested_data A tibble containing columns, for which a characteristic
#' of the content is defined by a tag in the column title.
#' @param type A character vector defining the type of the specification.
#' Either "language" or "relation".
#'
#' @return A tibble.
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





#' Turn tables from nested language info into lists
#'
#' @description
#' This helper function transforms tables created by transform_nested()
#' containing language data into lists.
#'
#' @param language_data_table A tibble containing information created by the
#' function transform_nested() for language data.
#'
#' @return A list.
#'
listify_language_table <- function(language_data_table) {

  # Create a list for each parent element
  data_nested_list <- lapply(language_data_table$name_parent_element |> unique(), function(list_name) {

    # Filter data
    data_nested_parent <- language_data_table |>
      dplyr::filter(name_parent_element == list_name)

    # Build list for each language
    data_nested_list <- lapply(unique(data_nested_parent$language), function(language) {

      # Filter for selected language
      data_nested_child <- data_nested_parent |>
        dplyr::filter(language == language)

      # Build initial list for the language
      data_nested_list <- list(
        language = list(data_nested_child$language[1])
      )

      # For each row of the same parent element in the same language, add a list element
      for (row in 1:nrow(data_nested_child)) {

        data_nested_list_element <- list(
          dynamic_name = list(data_nested_child$value[row])

        )

        data_nested_list <- c(
          data_nested_list,
          data_nested_list_element
        )

        # Rename the newly added element
        names(data_nested_list)[length(data_nested_list)] <- data_nested_child$name_list_element[row]
      }

      return(data_nested_list)

    })


    # Rename list elements
    names(data_nested_list) <- rep(unique(data_nested_parent$name_parent_element), length(data_nested_list))

    return(data_nested_list)

  })


  # Split Up -------------------------------------------------------------------


  # Define names
  nested_list_names <- sub(pattern = "Info", replacement = "", unique(language_data_table$name_parent_element))

  # Create empty list to be returned at the end of the function
  result_list <- list()

  # Assign the names to the lists. This gives us separate lists
  for (i in 1:length(nested_list_names)) {
    name <- nested_list_names[i]
    assign(name, data_nested_list[[i]])

    # Store in the list
    result_list[[name]] <- get(name)
  }

  return(result_list)

}
