#' Read nested nodes that define a language.
#'
#' @description
#' In eCH-0252, 0157 and 0159, there are nested elements, that define a language in the first child.
#' This helper function defines a logic to parse those elements and define new column names,
#' including the language.
#'
#'
#' @param xml_node A node of the XML file that contains nested children containing the element language.
#' The language element must be found on the 3rd level inside the node
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

  # Extract the children (level 3) of the node
  children_3 <- xml2::xml_children(children_2)

  # Stop, if we are not inside of a language text node
  if (!"language" %in% xml2::xml_name(children_3)) {
    stop(paste0(name_0, " is not a language text node."))
  }


  # LANGUAGE CHILDREN NODES ====================================================


  # This needs to be done once for ns0155...
  if ("ns0155" %in% objects()) {
    language_nodes_0155 <- xml2::xml_find_all(xml_node, paste0(".//", ns0155, ":language")) |>
      xml2::xml_parent() |>
      xml2::xml_parent() |>
      xml2::xml_name()
  } else {
    language_nodes_0155 <- NULL
  }

  # ...and once for ns0252...
  if ("ns0252" %in% objects()) {
    language_nodes_0252 <- xml2::xml_find_all(xml_node, paste0(".//", ns0252, ":language")) |>
      xml2::xml_parent() |>
      xml2::xml_parent() |>
      xml2::xml_name()
  } else {
    language_nodes_0252 <- NULL
  }

  # ...and then combined, since language somehow is part of namespace 01555 and 0252.
  language_nodes <- c(language_nodes_0155, language_nodes_0252)

  # Get index of relevant nodes that contain language nodes
  relevant <- which(grepl(paste0(language_nodes, collapse = "|"), xml2::xml_name(children_1)))


  # UNNESTED DATA TO DF ========================================================


  # Create node for regular unnested info
  unnested_node <- children_1[setdiff(seq_along(children_1), relevant)]

  # Turn the unnested node to a string
  unnested_node_unlist <- unnested_node |>
    xml2::as_list() |>
    unlist()

  # Define names
  names(unnested_node_unlist) <- xml2::xml_name(unnested_node)

  # Transpose and turn to df
  data_tbl1 <- as.data.frame(t(unnested_node_unlist))


  # NESTED DATA TO DF ==========================================================


  # Create node for nested language data
  language_node_node <- children_1[relevant]

  # Define children
  language_node_children <- xml2::xml_children(language_node_node)

  # Convert XML nodes to a named vector
  data_raw <- stats::setNames(
    object = xml2::xml_text(xml2::xml_children(language_node_children)),
    nm = xml2::xml_name(xml2::xml_children(language_node_children))
  )

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
  data_tbl2 <- as.data.frame(structured_list, stringsAsFactors = FALSE, row.names = NULL)


  # JOIN DATA ==================================================================


  data_tbl <- data_tbl1 |>
    cbind(data_tbl2)

  return(data_tbl)

}









