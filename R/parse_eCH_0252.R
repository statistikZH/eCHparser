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
  # xml_data <- xml2::read_xml(input_path)

  # Load file and strip namespaces
  xml_data_stripped <- strip_namespaces(input_path)

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

  # Load file and strip namespaces
  xml_data_stripped <- strip_namespaces(input_path)

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

  # Replace all NULL with NA
  out_df[out_df == "NULL"] <- NA

  # Shorten names and remove leftover _3_ that are left from read_electionGroupInfo()
  names <- data.frame(name_new = names(out_df)) |>
    dplyr::mutate(
      name_new  = gsub("_\\d+_$", "", name_new),
      name_new  = gsub("_\\d+_", "_", name_new)
    )

  names(out_df) <- names$name_new

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

  # Get electionGroup element (ignore countingCircle elements of the electionGroupInfo node)
  electionGroup_xml <- xml2::xml_child(xml_node, index) |>
    xml2::xml_child()


  # SPECIFY SPECIAL NODES' NAMES ===============================================


  # Nodes containing language nodes
  specify_node(electionGroup_xml, "language")

  # namedElement nodes
  specify_node(electionGroup_xml, "elementName")


  # TURN TO DF =================================================================


  # Get structure of the indexed node as a list
  electionGroup <- electionGroup_xml |>
    xml2::as_list()

  # Number the names to create unique names
  names(electionGroup) <- paste0(1:length(electionGroup),"_", names(electionGroup))

  # Number also the list elements of the sublists in here to create identifiers for the candidates and lists so we will be able to complete missing elements further down.
  electionGroup <- lapply(electionGroup, function(sublist) {
    names(sublist) <- paste0(seq_along(sublist), "_", names(sublist))
    return(sublist)
  })

  # Unlist the list
  electionGroup_unlist <- unlist(electionGroup)

  # List to df and add unique id
  electionGroup_df_long <- to_df(electionGroup_unlist, names(electionGroup_unlist)) |>
    dplyr::mutate(
      electionGroup_element_id = gsub("^(\\d+)_.*", "\\1", var),
      cand_list_id = gsub(".*\\.(\\d+)_.*", "\\1", var)
    )


  ## Election Group Information ------------------------------------------------


  # Define election group information
  electionGroup_info <- electionGroup_df_long |>
    dplyr::filter(!grepl("electionInformation\\.", var)) |>
    dplyr::select(-electionGroup_element_id, -cand_list_id) |>
    to_wide() |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    )


  ## Election Information and Results ------------------------------------------


  # Define election information
  election_info <- electionGroup_df_long |>
    dplyr::filter(grepl("electionInformation\\.", var)) |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    )


  ### Split Table --------------------------------------------------------------


  # Split table into candidate/list information and proper election information
  # This is necessary to complete potentially missing data on the candidate level
  # so that to_wide() works.
  election_info_info <- election_info |>
    dplyr::filter(grepl("^election", var_short))

  election_cand_info <- election_info |>
    dplyr::filter(!grepl("^election", var_short) & !grepl("list", var) & grepl("candidate", var))

  election_list_info <- election_info |>
    dplyr::filter(grepl("list", var))

  # Define all variables present in the candidate data
  election_cand_list_vars <- election_cand_info$var_short |>
    unique()

  # Complete the cand vars for every cand
  election_cand_info <- election_cand_info |>
    tidyr::complete(tidyr::nesting(cand_list_id, electionGroup_element_id), var_short = election_cand_list_vars)


  ### Widen Data ---------------------------------------------------------------


  # Election information
  election_info_info <- election_info_info |>
    dplyr::select(-cand_list_id) |>
    to_wide() |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    ) |>
    as.data.frame()

  # election_info_info <- election_info_info |>
  #   dplyr::select(-cand_list_id) |>
  #   to_wide() |>
  #   as.data.frame()

  # Candidate information
  election_cand_list_info <- election_cand_info |>
    to_wide() |>
    tidyr::unnest_longer(
      tidyselect::everything(),
      keep_empty = TRUE
    ) |>
    as.data.frame()

  # List information
  if (nrow(election_list_info) > 0) {

    election_list_info <- election_list_info |>
      to_wide() |>
      tidyr::unnest_longer(
        tidyselect::everything(),
        keep_empty = TRUE
      ) |>
      as.data.frame() |>
      dplyr::select(-cand_list_id)

    # Add to election cand list info
    election_cand_list_info <- election_cand_list_info |>
      dplyr::right_join(election_list_info, by = c(
        "electionGroup_element_id",
        "candidate_candidateIdentification" = "candidatePosition_candidateIdentification")
      )

  }

  # Add election info
  election_info <- election_info_info |>
    dplyr::right_join(election_cand_list_info, by = "electionGroup_element_id")

  # Add election group info
  electionGroup_info_complete <- electionGroup_info |>
    dplyr::bind_cols(election_info)

  # Change some names to adjust to result file
  names(electionGroup_info_complete)[grepl("candidateIdentification", names(electionGroup_info_complete))] <- "candidateIdentification"
  names(electionGroup_info_complete)[grepl("electionIdentification", names(electionGroup_info_complete))] <- "electionIdentification"

  return(electionGroup_info_complete)

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

  # Load file and strip namespaces
  xml_data_stripped <- strip_namespaces(input_path)

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
  electionGroupResult_indices <- which(grepl("electionGroupResult", xml2::xml_name(xml2::xml_children(node_electionResultDelivery))))

  # Stop if there are no result nodes
  if (length(electionGroupResult_indices) == 0) {
    stop(paste0("There are no votes matching your defined domains of influence (", paste0(doi, collapse = ", "), ")."))
  }


  ## Parse Relevant Elections --------------------------------------------------


  # Transform results data to list
  out_list <- lapply(electionGroupResult_indices, function(electionGroupResult_index) {

    read_electionGroupResult(
      xml_node = node_electionResultDelivery,
      index = electionGroupResult_index
    )

  })

  # Transform relevant data to df
  out_df <- dplyr::bind_rows(out_list)


  ## Finalise Data -------------------------------------------------------------


  # Add contest information and mutate non-optional boolean
  out_df <- out_df |>
    dplyr::mutate(
      cantonId = cantonId,
      pollingDay = pollingDay,
      resultData_isFullyCounted = dplyr::case_when(
        resultData_isFullyCounted == "true" ~ TRUE,
        TRUE ~ FALSE
      ),
      isElectionResultComplete = dplyr::case_when(
        isElectionResultComplete == "true" ~ TRUE,
        TRUE ~ FALSE
      )
    )

  # Mutate optional boolean
  if ("electedCandidate_isElectedByDraw" %in% names(out_df)) {
    out_df <- out_df |>
      dplyr::mutate(electedCandidate_isElectedByDraw = dplyr::case_when(
          electedCandidate_isElectedByDraw == "true" ~ TRUE,
          electedCandidate_isElectedByDraw == "false" ~ FALSE,
          TRUE ~ NA
      ))
  }

  if ("elected" %in% names(out_df)) {
  out_df <- out_df |>
    dplyr::mutate(elected = dplyr::case_when(
      elected == "true" ~ TRUE,
      TRUE ~ FALSE
    ))
  }

  if("candidate_incumbentYesNo" %in% names(out_df)) {
    out_df <- out_df |>
      dplyr::mutate(candidate_incumbentYesNo = dplyr::case_when(
        candidate_incumbentYesNo == TRUE ~ TRUE,
        TRUE ~ FALSE
      ))
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
  electionGroupIdentification <- electionGroupResult[["electionGroupIdentification"]] |>
    unlist()

  # Get election result indices
  electionResult_indices <- which(grepl("electionResult", names(electionGroupResult)))

  # Parse through election results
  out_list <- lapply(electionResult_indices, function(electionResult_index) {

    # Define electionGroupResult element
    electionResult <- electionGroupResult[[electionResult_index]]

    # Get election ID
    electionResult_electionIdentification <- electionResult[["electionIdentification"]] |>
      unlist()

    # Get counting circle result indices
    countingCircleResult_indices <- which(grepl("ountingCircleResult", names(electionResult)))

    # Get elected index (always only one)
    elected_index <- which(grepl("elected", names(electionResult)))


    ### Counting Circle Results ------------------------------------------------


    # Parse through counting circle election results
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
        electionIdentification <- electionResult[["electionIdentification"]][[1]]
        electionResult[["electionIdentification"]] <- NULL

        # Divide into candidate/list result bit and rest
        result_indices <- grep("Result", names(electionResult[[1]]))
        votes_indices <- which(!grepl("Result", names(electionResult[[1]])))

        # Apply through result list
        out_list <- lapply(result_indices, function(result_index) {

          # Define candidate or list result element
          result <- electionResult[[1]][[result_index]]

          # Unlist the list
          result_unlist <- unlist(result)

          # List to df and add unique id
          result_df_long <- to_df(result_unlist, names(result_unlist))

          # Transform to table
          result <- result_df_long |>
            to_wide() |>
            tidyr::unnest_longer(
              tidyselect::everything(),
              keep_empty = TRUE
            )

          # return(candidate_result)

        })

        # Transform relevant data to df
        out_df <- dplyr::bind_rows(out_list)

        # Add invalid/empty votes
        if (length(votes_indices) > 0) {
          votes <- electionResult[[1]][votes_indices]

          # Unlist
          votes_unlist <- unlist(votes)

          # List to df and add unique id
          votes_df_long <- to_df(votes_unlist, names(votes_unlist))

          # Transform to table
          votes <- votes_df_long |>
            to_wide() |>
            tidyr::unnest_longer(
              tidyselect::everything(),
              keep_empty = TRUE
            )

          # Add votes to results
          out_df <- out_df |>
            dplyr::bind_cols(votes)

        }

        # Add contest information
        out_df <- out_df |>
          dplyr::mutate(
            electionIdentification = electionIdentification
          )

        countingCircle_result <- countingCircle_result |>
          dplyr::bind_cols(out_df)

      }

      return(countingCircle_result)

    })

    # Transform relevant data to df
    out_df <- dplyr::bind_rows(out_list)
    names(out_df)[grepl("candidateIdentification", names(out_df))] <- "candidateIdentification" # necessary for the join with maj/prop bellow

    ### Elected ----------------------------------------------------------------


    if (length(elected_index) > 0) {

      # Define elected element
      elected <- electionResult[[elected_index]][[1]]

      # Define absolute majority
      absoluteMajority <- elected[["absoluteMajority"]] |>
        unlist()

      # Define election result completion
      isElectionResultComplete <- elected[["isElectionResultComplete"]] |>
        unlist()

      # Define your nodes of interest (either electedCandidate for majority of list [one node above] for proportion) --> this is the only difference between prop and maj
      if (grepl("majorityElection", names(electionResult[[elected_index]]))) {
        electedCandidate <- elected[grep("electedCandidate", names(elected))]
      } else if (grepl("proportionalElection", names(electionResult[[elected_index]]))) {
        electedCandidate <- elected[grep("list", names(elected))]
      }

      # If there are elected candidates, turn into df
      if (length(electedCandidate) > 0) {

        # Number the names to create unique names
        names(electedCandidate) <- paste0(1:length(electedCandidate),"_", names(electedCandidate))

        # Unlist the list
        electedCandidate_unlist <- unlist(electedCandidate)

        # List to df and add unique id
        electedCandidate_info <- to_df(electedCandidate_unlist, names(electedCandidate_unlist)) |>
          dplyr::mutate(unique_id = gsub("^(\\d+)_.*", "\\1", var)) |>
          dplyr::filter(grepl("candidateIdentification", var_short) | grepl("isElectedByDraw", var_short)) |>
          to_wide() |>
          dplyr::select(-unique_id) |>
          tidyr::unnest_longer(
            tidyselect::everything(),
            keep_empty = TRUE
          ) |>
          dplyr::mutate(elected = "true")
        names(electedCandidate_info)[grepl("candidateIdentification", names(electedCandidate_info))] <- "candidateIdentification" # necessary for maj/prop

      } else {

        electedCandidate_info <- NULL

      }


      ## Finalise Data -----------------------------------------------------------


      # Add contest information and elected info
      out_df <- out_df |>
        dplyr::mutate(
          absoluteMajority = absoluteMajority,
          isElectionResultComplete = isElectionResultComplete
        )

      if (!is.null(electedCandidate_info)) {
        out_df <- out_df |>
          dplyr::left_join(electedCandidate_info)
      }

      out_df <- out_df |>
        dplyr::mutate(
          electionResult_electionIdentification = electionResult_electionIdentification
        )

    }

    # # Add additional values in case of proportional elections
    # if ("proportionalElection" %in% names(electionResult)) {
    #   out_df <- out_df |>
    #     dplyr::mutate(
    #       countOfChangedBallotsWithoutListDesignation = electionResult[["proportionalElection"]][["countOfChangedBallotsWithoutListDesignation"]],
    #       countOfBlankVotesOfChangedBallotsWithoutListDesignation = electionResult[["proportionalElection"]][["countOfBlankVotesOfChangedBallotsWithoutListDesignation"]]
    #     )
    # }

  })

  # Transform relevant data to df
  out_df <- dplyr::bind_rows(out_list)


  ## Finalise Data -------------------------------------------------------------


  # Add contest information
  out_df <- out_df |>
    dplyr::mutate(
      electionGroupIdentification = electionGroupIdentification
    )

  return(out_df)

}


