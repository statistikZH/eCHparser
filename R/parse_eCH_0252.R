#' Convert an eCH-0252 XML file into a dataframe
#'
#' @description
#' This function turns an eCH-0252 XML file for a contest including multiple
#' votes into a dataframe.
#'
#' @param input_path Path to your XML file.
#' @param doi Domains of influence of the votes that you want included.
#' If set to "all" (the default), all votes are included.
#' [eCH-0155](https://www.ech.ch/de/ech/ech-0155/4.1) defines the following
#' domains of influence:
#' * CH = Bund
#' * CT = Kanton
#' * BZ = Bezirk / Amt / Verwaltungskreis
#' * MU = Gemeinde
#' * SC = Schulgemeinde
#' * KI = Kirchgemeinde
#' * OG = Ortsbürgergemeinden
#' * KO = Korporationen
#' * SK = Stadtkreis
#' * AN = andere
#'
#' @return A dataframe.
#'
#' @export
#'
#' @examples
#' votedata <- parse_eCH_0252(
#'   system.file(
#'     "extdata/eCH-0252_abraxas_vote_ZH_counting_2024-11-24.xml",
#'     package = "eCHparser"
#'   ),
#'   doi = c("CH", "CT", "MU")
#' )
#'
parse_eCH_0252 <- function(input_path, doi = "all"){

  # Load file
  xml_data <- xml2::read_xml(input_path)

  # Strip namespaces
  xml_data_stripped <- strip_namespaces(xml_data)

  # Load base delivery part of the file
  node_voteBaseDelivery <- xml2::xml_find_first(xml_data_stripped, ".//voteBaseDelivery")


  # POLLING DAY INFORMATION ====================================================


  # Define canton id
  cantonId <- xml2::xml_find_first(node_voteBaseDelivery, paste0(".//cantonId")) |>
    xml2::xml_integer()

  # Define polling day
  pollingDay <- xml2::xml_find_first(node_voteBaseDelivery, paste0(".//pollingDay")) |>
    xml2::xml_text()


  # VOTE INFORMATION ===========================================================


  # Define domain of influence types found in data
  domainofOnfluenceType <- xml2::xml_find_all(node_voteBaseDelivery, ".//domainOfInfluence") |>
    xml2::xml_find_all(paste0(".//domainOfInfluenceType")) |>
    xml2::xml_text()

  # Define index of votes with the relevant domain of influence types (must be +2 since the first two nodes are not of interest)
  if ("all" %in% doi) {
    relevant <- seq_along(domainofOnfluenceType) + 2
  } else {
  relevant <- which(grepl(paste0(doi, collapse = "|"), domainofOnfluenceType)) + 2
  }

  # Stop if there are no relevant votes
  if (length(relevant) == 0) {
    stop(paste0("There are no votes matching your defined domains of influence (", paste0(doi, collapse = ", "), ")."))
  }


  ## Parse Relevant Votes ------------------------------------------------------


  # Transform relevant data to list
  out_list <- lapply(relevant, function(relevant_index) {

    read_voteInfo(
      xml_node = node_voteBaseDelivery,
      index = relevant_index
    )

  })

  # Transform relevant data to df
  out_df <- dplyr::bind_rows(out_list)


  ## Finalise Data -------------------------------------------------------------


  # Add contest information
  out_df <- out_df |>
    dplyr::mutate(
      cantonId = cantonId,
      pollingDay = pollingDay
    )

  # Drop unique_id column
  if ("unique_id" %in% names(out_df)) {
    out_df <- out_df |>
      dplyr::select(-unique_id)
  }

  return(out_df)

}





#' Convert an eCH-0252 voteInfo node into a dataframe
#'
#' @param xml_node Node voteBaseDelivery of the XML file.
#' @param index Index of the voteInfo node of interest.
#'
#' @return A dataframe.
#'
read_voteInfo <- function(xml_node, index){

  # Get voteInfo element
  voteInfo_xml <- xml2::xml_child(xml_node, index)


  # SPECIFY SPECIAL NODES' NAMES ===============================================


  # Nodes containing language nodes
  specify_node(voteInfo_xml, "language")

  # otherIdentification nodes
  specify_node(voteInfo_xml, "idName")

  # namedElement nodes
  specify_node(voteInfo_xml, "elementName")

  # Subtotal voter nodes
  specify_voter_node(voteInfo_xml)


  # TURN TO DF =================================================================


  # Get structure of the indexed node as a list
  voteInfo <- voteInfo_xml |>
    xml2::as_list()

  # Number the names to create unique names
  names(voteInfo) <- paste0(1:length(voteInfo),"_", names(voteInfo))

  # Unlist the list
  voteInfo_unlist <- unlist(voteInfo)

  # List to df and add unique id
  voteInfo_df_long <- to_df(voteInfo_unlist, names(voteInfo_unlist)) |>
    dplyr::mutate(unique_id = gsub("^(\\d+)_.*", "\\1", var))


  ## Vote Information ----------------------------------------------------------


  # Define vote information
  vote_info <- voteInfo_df_long |>
    dplyr::filter(grepl("vote\\.", var)) |>
    to_wide() |>
    dplyr::select(-unique_id) |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    )


  ## Vote Results --------------------------------------------------------------


  # Define vote results
  vote_result <- voteInfo_df_long |>
    dplyr::filter(!grepl("vote\\.", var)) |>
    to_wide() |>
    as.data.frame()

  # Replace all NULL with NA
  vote_result[vote_result == "NULL"] <- NA

  # Unnest
  vote_result_full <- vote_result |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    ) |>
    dplyr::mutate(vote_voteIdentification = vote_info$vote_voteIdentification)

  # Join result and information data
  vote_data_complete <- vote_result_full |>
    dplyr::left_join(vote_info, by = "vote_voteIdentification")

  return(vote_data_complete)

}





#' Convert an eCH-0252 election information XML file into a dataframe
#'
#' @description
#' This function turns an eCH-0252 XML file for a contest including multiple
#' elections into a dataframe.
#'
#' @inheritParams parse_eCH_0252
#'
#' @return A dataframe.
#'
#' @export
#'
#' @examples
#'
parse_eCH_0252_elections_info <- function(input_path, doi = "all"){

  # Load file
  xml_data <- xml2::read_xml(input_path)

  # Strip namespaces
  xml_data_stripped <- strip_namespaces(xml_data)

  # Load election information delivery part of the file
  node_electionInformationDelivery <- xml2::xml_find_first(xml_data_stripped, ".//electionInformationDelivery")


  # POLLING DAY INFORMATION ====================================================


  # Define canton id
  cantonId <- xml2::xml_find_first(node_electionInformationDelivery, paste0(".//cantonId")) |>
    xml2::xml_integer()

  # Define polling day
  pollingDay <- xml2::xml_find_first(node_electionInformationDelivery, paste0(".//pollingDay")) |>
    xml2::xml_text()


  # ELECTION INFORMATION =======================================================


  # Define domain of influence types found in data
  domainofOnfluenceType <- xml2::xml_find_all(node_electionInformationDelivery, ".//domainOfInfluence") |>
    xml2::xml_find_all(paste0(".//domainOfInfluenceType")) |>
    xml2::xml_text()

  # Define the index of the first electionGroupInfo node
  index_addition <- match("electionGroupInfo", xml2::xml_name(xml2::xml_children(node_electionInformationDelivery))) - 1

  # Define index of votes with the relevant domain of influence types (out of all electionGroupInfo nodes)
  if ("all" %in% doi) {
    relevant <- seq_along(domainofOnfluenceType) + index_addition
  } else {
    relevant <- which(grepl(paste0(doi, collapse = "|"), domainofOnfluenceType)) + index_addition
  }

  # Stop if there are no relevant votes
  if (length(relevant) == 0) {
    stop(paste0("There are no votes matching your defined domains of influence (", paste0(doi, collapse = ", "), ")."))
  }


  ## Define Election Association -----------------------------------------------


  # Define election association nodes
  electionAssociation_nodes <- xml2::xml_find_all(node_electionInformationDelivery, ".//electionAssociation")

  # Parse through all electionAssociation nodes
  electionAssociation_list <- lapply(seq_along(electionAssociation_nodes), function(electionAssociation_index){

    # Isolate relevant electionAssociation
    node_electionAssociation <- electionAssociation_nodes[electionAssociation_index]

    # Specify language node
    specify_node(node_electionAssociation, "language")

    # Turn into list
    electionAssociation_unlist <- node_electionAssociation |>
      xml2::as_list() |>
      unlist()

  })

  # Bind rows to df
  electionAssociations <- dplyr::bind_rows(electionAssociation_list)


  ## Parse Relevant Elections --------------------------------------------------


  # Transform relevant data to list
  out_list <- lapply(relevant, function(relevant_index) {

    read_electionGroupInfo(
      xml_node = node_electionInformationDelivery,
      index = relevant_index
    )

  })

  # Transform relevant data to df
  out_df <- dplyr::bind_rows(out_list)


  ## Finalise Data -------------------------------------------------------------


  # Add contest information
  out_df <- out_df |>
    dplyr::mutate(
      cantonId = cantonId,
      pollingDay = pollingDay
    )

  # Drop unique_id column
  if ("unique_id" %in% names(out_df)) {
    out_df <- out_df |>
      dplyr::select(-unique_id)
  }

  return(out_df)

}





#' Convert an eCH-0252 electionGroupInfo node into a dataframe
#'
#' @param xml_node Node electionInformationDelivery of the XML file.
#' @param index Index of the electionGroupInfo node of interest.
#'
#' @return A dataframe.
#'
read_electionGroupInfo <- function(xml_node, index){

  # Get electionGroupInfo element
  electionGroupInfo_xml <- xml2::xml_child(xml_node, index)


  # SPECIFY SPECIAL NODES' NAMES ===============================================


  # Nodes containing language nodes
  specify_node(electionGroupInfo_xml, "language")


  # TURN TO DF =================================================================


  # Get structure of the indexed node as a list
  electionGroupInfo <- electionGroupInfo_xml |>
    xml2::as_list()

  # Drop the countingCircle elements
  electionGroupInfo <- electionGroupInfo[["electionGroup"]]

  # Separate the electionInformation (candidates etc.)
  electionInfo <- electionGroupInfo[["electionInformation"]]

  # Drop the electionInformation elements
  electionGroupInfo[["electionInformation"]] <- NULL

  # Unlist the list
  electionGroupInfo_unlist <- unlist(electionGroupInfo)

  # List to df and add unique id
  electionGroupInfo_df_long <- to_df(electionGroupInfo_unlist, names(electionGroupInfo_unlist))


  ## Election Information ------------------------------------------------------


  # Define vote information
  electionGroup_info <- electionGroupInfo_df_long |>
    to_wide() |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    )

  # Add second level to names if not there already
  single_level_positions <- grep("_", names(electionGroup_info), invert = TRUE)
  names(electionGroup_info)[single_level_positions] <- paste0("electionGroup_", names(electionGroup_info)[single_level_positions])


  ## Candidate and List Information --------------------------------------------


  # Number the names to create unique names
  names(electionInfo) <- paste0(1:length(electionInfo),"_", names(electionInfo))

  # Unlist the list
  electionInfo_unlist <- unlist(electionInfo)

  # List to df and add unique id
  electionInfo_df_long <- to_df(electionInfo_unlist, names(electionInfo_unlist)) |>
    dplyr::mutate(unique_id = gsub("^(\\d+)_.*", "\\1", var))

  # Define candidate information
  candidate_info <- electionInfo_df_long |>
    dplyr::filter(grepl("candidate\\.", var)) |>
    to_wide() |>
    dplyr::select(-unique_id) |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    )

  # Define list information
  list_info <- electionInfo_df_long |>
    dplyr::filter(grepl("list\\.", var)) |>
    to_wide() |>
    dplyr::select(-unique_id) |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    )

  # Check if we are dealing with a proportional election
  if (length(list_info) > 0) {

    # Define variables of both tables
    join_var <- intersect(names(candidate_info), names(list_info))

    # Join info (start with list to keep the WoP)
    candidate_list_info <- list_info |>
      dplyr::left_join(candidate_info, by = c(join_var, "candidatePosition_candidateIdentification" = "candidate_candidateIdentification"))

  } else {

    candidate_list_info <- candidate_info

  }




  ## Join All Data -------------------------------------------------------------


  electionGroup_data_complete <- dplyr::bind_cols(candidate_list_info, electionGroup_info)

  return(electionGroup_data_complete)

}





#' Convert an eCH-0252 election results XML file into a dataframe
#'
#' @description
#' This function turns an eCH-0252 XML file for a contest including multiple
#' elections into a dataframe.
#'
#' @inheritParams parse_eCH_0252
#'
#' @return A dataframe.
#'
#' @export
#'
#' @examples
#'
parse_eCH_0252_elections_result <- function(input_path, doi = "all"){

  # Load file
  xml_data <- xml2::read_xml(input_path)

  # Strip namespaces
  xml_data_stripped <- strip_namespaces(xml_data)

  # Load election information delivery part of the file
  node_electionResultDelivery <- xml2::xml_find_first(xml_data_stripped, ".//electionResultDelivery")


  # POLLING DAY INFORMATION ====================================================


  # Define canton id
  cantonId <- xml2::xml_find_first(node_electionResultDelivery, paste0(".//cantonId")) |>
    xml2::xml_integer()

  # Define polling day
  pollingDay <- xml2::xml_find_first(node_electionResultDelivery, paste0(".//pollingDay")) |>
    xml2::xml_text()


  # ELECTION INFORMATION =======================================================


  # Define election group result nodes
  results <- which(grepl("electionGroupResult", xml2::xml_name(xml2::xml_children(node_electionResultDelivery))))

  # Stop if there are no result nodes
  if (length(results) == 0) {
    stop(paste0("There are no votes matching your defined domains of influence (", paste0(doi, collapse = ", "), ")."))
  }


  ## Parse Relevant Elections --------------------------------------------------


  # Transform results data to list
  out_list <- lapply(results, function(result_index) {

    read_electionGroupResult(
      xml_node = node_electionResultDelivery,
      index = result_index
    )

  })


  # STAND HIER =================================================================










  ## Parse Relevant Elections --------------------------------------------------


  # Transform relevant data to list
  out_list <- lapply(relevant, function(relevant_index) {

    read_electionGroupInfo(
      xml_node = node_electionInformationDelivery,
      index = relevant_index
    )

  })

  # Transform relevant data to df
  out_df <- dplyr::bind_rows(out_list)


  ## Finalise Data -------------------------------------------------------------


  # Add contest information
  out_df <- out_df |>
    dplyr::mutate(
      cantonId = cantonId,
      pollingDay = pollingDay
    )

  # Drop unique_id column
  if ("unique_id" %in% names(out_df)) {
    out_df <- out_df |>
      dplyr::select(-unique_id)
  }

  return(out_df)

}





#' Convert an eCH-0252 electionGroupInfo node into a dataframe
#'
#' @param xml_node Node electionInformationDelivery of the XML file.
#' @param index Index of the electionGroupInfo node of interest.
#'
#' @return A dataframe.
#'
read_electionGroupResult <- function(xml_node, index){

  # Get electionGroupResult element
  electionGroupResult_xml <- xml2::xml_child(xml_node, index)


  # SPECIFY SPECIAL NODES' NAMES ===============================================


  # Nodes containing language nodes
  specify_node(electionGroupResult_xml, "language")

  # namedElement nodes
  specify_node(electionGroupResult_xml, "elementName")

  # Subtotal voter nodes
  specify_voter_node(electionGroupResult_xml)


  # TURN TO DF =================================================================


  ## Election Group Results ----------------------------------------------------


  # Get structure of the indexed node as a list
  electionGroupResult <- electionGroupResult_xml |>
    xml2::as_list()

  # Get election group ID
  electionGroupID <- electionGroupResult[["electionGroupIdentification"]] |>
    unlist()

  # Get election result indices
  electionResult_indices <- which(grepl("electionResult", names(electionGroupResult)))

  # Parse through election results
  out_list <- lapply(electionResult_indices, function(electionResult_index) {

    # Define electionGroupResult element
    electionResult <- electionGroupResult[[electionResult_index]]

    # Get election ID
    electionID <- electionResult[["electionIdentification"]] |>
      unlist()

    # Get counting circle result indices
    countingCircleResult_indices <- which(grepl("ountingCircleResult", names(electionResult)))

    # Parse through election results
    out_list <- lapply(countingCircleResult_indices, function(countingCircleResult_index) {

      # Define countingCircleResult element
      countingCircleResult <- electionResult[[countingCircleResult_index]]

      # Split into counting circle information and result data
      if ("resultData" %in% names(countingCircleResult)) {
        electionResult <- countingCircleResult[["resultData"]][["electionResult"]]
        countingCircleResult[["resultData"]][["electionResult"]] <- NULL
      }

      # Unlist the list
      countingCircleResult_unlist <- unlist(countingCircleResult)

      # List to df and add unique id
      countingCircleResult_df_long <- to_df(countingCircleResult_unlist, names(countingCircleResult_unlist))

      # Transform to table
      countingCircle_result <- countingCircleResult_df_long |>
        to_wide() |>
        tidyr::unnest_longer(
          tidyselect::everything(),
          keep_empty = TRUE
        )


      ## Election Results ------------------------------------------------------


      if ("resultData" %in% names(countingCircleResult)) {

        # Define election ID, then drop it
        electionID <- electionResult[["electionIdentification"]]
        electionResult[["electionIdentification"]] <- NULL

        out_list <- lapply(seq_along(electionResult[[1]]), function(result_index) {
browser()



          # STAND HIER ======================================================================================================================
          # Müssen wohl noch einmal ufteilen in candidateResult und die restlichen




          # Define candidateResult element
          candidateResult <- electionResult[[1]][[result_index]]

          # Unlist the list
          candidateResult_unlist <- unlist(candidateResult)

          # List to df and add unique id
          candidateResult_df_long <- to_df(candidateResult_unlist, names(candidateResult_unlist))

          # Transform to table
          candidate_result <- candidateResult_df_long |>
            to_wide() |>
            tidyr::unnest_longer(
              tidyselect::everything(),
              keep_empty = TRUE
            )

          # return(candidate_result)

        })

        # Transform relevant data to df
        out_df <- dplyr::bind_rows(out_list)

        # Add contest information
        out_df <- out_df |>
          dplyr::mutate(
            electionID = electionID
          )

        return(out_df)

      }

    })



  })















  # Drop the election group ID elements
  electionGroupResult[["electionGroupIdentification"]] <- NULL

  # Unlist the list
  electionGroupResult_unlist <- unlist(electionGroupResult)

  # List to df and add unique id
  electionGroupResult_df_long <- to_df(electionGroupResult_unlist, names(electionGroupResult_unlist))


  ## Election Information ------------------------------------------------------


  # Define election results
  electionGroup_result <- electionGroupResult_df_long |>
    to_wide() |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    )






  # Add second level to names if not there already
  single_level_positions <- grep("_", names(electionGroup_info), invert = TRUE)
  names(electionGroup_info)[single_level_positions] <- paste0("electionGroup_", names(electionGroup_info)[single_level_positions])


  ## Candidate and List Information --------------------------------------------


  # Number the names to create unique names
  names(electionInfo) <- paste0(1:length(electionInfo),"_", names(electionInfo))

  # Unlist the list
  electionInfo_unlist <- unlist(electionInfo)

  # List to df and add unique id
  electionInfo_df_long <- to_df(electionInfo_unlist, names(electionInfo_unlist)) |>
    dplyr::mutate(unique_id = gsub("^(\\d+)_.*", "\\1", var))

  # Define candidate information
  candidate_info <- electionInfo_df_long |>
    dplyr::filter(grepl("candidate\\.", var)) |>
    to_wide() |>
    dplyr::select(-unique_id) |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    )

  # Define list information
  list_info <- electionInfo_df_long |>
    dplyr::filter(grepl("list\\.", var)) |>
    to_wide() |>
    dplyr::select(-unique_id) |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    )

  # Check if we are dealing with a proportional election
  if (length(list_info) > 0) {

    # Define variables of both tables
    join_var <- intersect(names(candidate_info), names(list_info))

    # Join info (start with list to keep the WoP)
    candidate_list_info <- list_info |>
      dplyr::left_join(candidate_info, by = c(join_var, "candidatePosition_candidateIdentification" = "candidate_candidateIdentification"))

  } else {

    candidate_list_info <- candidate_info

  }




  ## Join All Data -------------------------------------------------------------


  electionGroup_data_complete <- dplyr::bind_cols(candidate_list_info, electionGroup_info)

  return(electionGroup_data_complete)

}


