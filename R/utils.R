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

  # Define the children of the node
  children_1 <- xml2::xml_children(xml_node)

  # Extract all children of the node
  children_2 <- xml2::xml_children(children_1)

  # Stop, if we are not inside of a language text node
  if (!"language" %in% xml2::xml_name(children_2)) {
    stop(paste0(name_0, " is not a language text node."))
  }

  # Convert XML nodes to a named list
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









