



read_voteBaseDelivery <- function(file, doi = c("CH", "CT")){

  # load file
  xml_data <- xml2::read_xml(file)
  xml_data_sitrox <- xml2::read_xml("/home/file-server/01_Post/Graf/dont_touch_this/ech-0252_examples/sitrox/20220530_Abstimmungen_ech0252_Resultate.xml")

  # define namespaces
  namespaces <- xml2::xml_ns(xml_data)

  # define the prefixes we need
  ns0155 <- names(namespaces[grep("155", namespaces)])[1]
  ns0252 <- names(namespaces[grep("252", namespaces)])[1]

  # load base delivery part of the file
  node_voteBaseDelivery <- xml2::xml_find_first(xml_data, paste0(".//", ns0252, ":voteBaseDelivery"))

  # define canton id
  canton_id <- xml2::xml_find_first(node_voteBaseDelivery, paste0(".//", ns0252, ":cantonId")) |>
    xml2::xml_integer()

  # define polling day
  polling_day <- xml2::xml_find_first(node_voteBaseDelivery, paste0(".//", ns0252, ":pollingDay")) |>
    xml2::xml_text()

  # define domain of influence types found in data
  domainofOnfluenceType <- xml2::xml_find_all(node_voteBaseDelivery, paste0(".//", ns0252, ":domainOfInfluence")) |>
    xml2::xml_find_all(paste0(".//", ns0155, ":domainOfInfluenceType")) |>
    xml2::xml_text()

  # define index of votes with the relevant domain of influence types (must be +2 since the first two nodes are not of interest)
  relevant <- which(grepl(paste0(doi, collapse = "|"), domainofOnfluenceType)) + 2

  # stop if there are no relevant votes
  if (length(relevant) == 0) {
    stop("There are no votes matching your defined domains of influence (doi).")
  }

  # transform relevant data to list
  out_list <- lapply(relevant, function(x) read_voteInfo(node_voteBaseDelivery, x, canton_id, polling_day))

  # transform relevant data to df
  out_df <- dplyr::bind_rows(out_list)

  return(out_df)
}






read_voteInfo <- function(xml_node, index, canton_id, polling_day){

  # get structure of the indexed node as a list
  voteInfo <- xml2::xml_child(xml_node, index) |>
    xml2::as_list()

  # number the names to create unique names
  names(voteInfo) <- paste0(1:length(voteInfo),"_", names(voteInfo))

  # unlist the list
  voteInfo_unlist <- unlist(voteInfo)

  # list to df and add unique id
  voteInfo_df_long <- to_df(voteInfo_unlist, names(voteInfo_unlist)) |>
    dplyr::mutate(unique_id = gsub("^(\\d+)_.*", "\\1", var)) |>
    dplyr::mutate(var_short = gsub("\\d_", "", var_short))

  # define vote information
  vote_info <- voteInfo_df_long |>
    dplyr::filter(grepl("vote\\.", var)) |>
    to_wide() |>
    dplyr::select(-unique_id) |>
    tidyr::unnest_longer(everything())

  # define vote results
  vote_result <- voteInfo_df_long |>
    dplyr::filter(!grepl("vote\\.", var)) |>
    to_wide()

  # deal with nested namedElement nodes
  if ("namedElement_elementName" %in% names(vote_result)){

    element_name <- vote_result |>
      dplyr::select(unique_id, namedElement_elementName, namedElement_text) |>
      tidyr::unnest_longer(everything()) |>
      tidyr::pivot_wider(names_from = namedElement_elementName, values_from = namedElement_text) |>
      tidyr::unnest_longer(everything())

    vote_result_full <- vote_result |>
      dplyr::select(-namedElement_elementName, -namedElement_text) |>
      tidyr::unnest_longer(everything(), keep_empty = TRUE) |>
      dplyr::mutate(vote_voteIdentification = vote_info$vote_voteIdentification) |>
      dplyr::left_join(element_name, by = "unique_id")

  } else {

    vote_result_full <- vote_result |>
      tidyr::unnest_longer(everything(), keep_empty = TRUE) |>
      dplyr::mutate(vote_voteIdentification = vote_info$vote_voteIdentification)

  }

  # join result and information data
  vote_data_complete <- vote_result_full |>
    dplyr::left_join(vote_info, by = "vote_voteIdentification") |>
    dplyr::mutate(canotonId = canton_id,
                  pollingDay = polling_day)

  return(vote_data_complete)
}




to_wide <- function(data){

  if ("var" %in% names(data)){
    data <- data |>
      # delete var to make the pivot work
      dplyr::select(-var)
  }

  data |>
    tidyr::pivot_wider(
      names_from = var_short,
      values_from = data,
      values_fn = list
    )

}




to_df <- function(data, names){

  data.frame(
    data = data,
    var = names
  ) |>
    dplyr::mutate(
      # search pattern starts at the end of the string and searches for the
      # first two dots. it returns the string in the brackets.
      # "datei.name.txt" --> "name.txt"
      var_short = gsub(".*\\.(.*\\..*)$", "\\1", var)
    ) |>
    dplyr::mutate(var_short = gsub("\\.", "_", var_short))
}





