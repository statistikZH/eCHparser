#' Read nested nodes that define a language.
#'
#' @description
#' In eCH-0252, 0157 and 0159, there are nested elements, that define a language in the first child.
#' This helper function defines a logic to parse those elements and define new column names,
#' including the language.
#'
#'
#' @param xml_node A node of the XML file that contains nested children containing the element language.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
read_language_text_node <- function(xml_node) {

  # Define the name of the node
  name_0 <- xml2::xml_name(xml_node)

  # Extract the children (level 1) of the node
  children_1 <- xml2::xml_children(xml_node)

  # Extract the children (level 2) of the node
  children_2 <- xml2::xml_children(children_1)

  # Stop, if we are not inside of a language text node
  if (!"language" %in% xml2::xml_name(children_2)) {
    stop(paste0(name_0, " is not a language text node."))
  }

  # Extract only nodes with language children
  # This needs to be done once for ns0155...
  language_nodes_0155 <- xml2::xml_find_all(voteInfo_xml, paste0(".//", ns0155, ":language")) |>
    xml2::xml_parent() |>
    xml2::xml_name()

  # ...and once for ns0252...
  language_nodes_0252 <- xml2::xml_find_all(voteInfo_xml, paste0(".//", ns0252, ":language")) |>
    xml2::xml_parent() |>
    xml2::xml_name()

  # ...and then combined, since language somehow is part of namespace 01555 and 0252.
  language_nodes <- c(language_nodes_0155, language_nodes_0252)

  # Get index of relevant nodes
  relevant <- which(grepl(paste0(language_nodes, collapse = "|"), xml2::xml_name(children_1)))

  # Keep only the relevant children of the node
  # First, define which ones to drop
  to_remove <- setdiff(seq_along(children_1), relevant)

  # Redefine the node
  xml_node_test <- xml2::xml_remove(children_1[to_remove])


  xml_node_test <- xml_node

  xml_node[[6]]

  xml2::









  # Convert XML nodes to a named vector
  data_raw <- stats::setNames(xml2::xml_text(children_2), xml2::xml_name(children_2))

  # Identify all "language" values and their corresponding indices
  language_values <- data_raw[names(data_raw) == "language"]
  other_keys <- unique(names(data_raw)[names(data_raw) != "language"])

  # Prepare a list to store structured data
  structured_list <- list()

  # Loop through languages and assign corresponding values dynamically
  for (i in seq_along(language_values)) {
    lang <- language_values[i]

    for (key in other_keys) {
      structured_list[[paste0(key, "_", lang)]] <- data_raw[names(data_raw) == key][i]
    }
  }

  # Convert the structured list into a data frame
  data_tbl <- as.data.frame(structured_list, stringsAsFactors = FALSE, row.names = NULL)

  return(data_tbl)

}









