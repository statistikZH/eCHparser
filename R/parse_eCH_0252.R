#' Convert an eCH-0252 XML file into a dataframe
#'
#' @description
#' This function turns an eCH-0252 XML file for a contest including multiple
#' votes into a dataframe.
#'
#' @param input_path Path to your XML file.
#' @param doi Domains of influence of the votes that you want included.
#' If set to "all" (the default), all votes are included.
#' [eCH-0155](https://www.ech.ch/de/ech/ech-0155/4.1) defines the following domains of influence:
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
#' votedata <- parse_eCH_0252(system.file("extdata/eCH-0252_abraxas_vote_ZH_counting_2024-11-24.xml", package = "eCHparser"), doi = c("CH", "CT", "MU"))
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
