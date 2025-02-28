#' Read nested nodes that define a language.
#'
#' @description
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
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


  # Get all language nodes
  language_nodes <- xml2::xml_find_all(xml_node, ".//language") |>
    xml2::xml_parent() |>
    xml2::xml_parent() |>
    xml2::xml_name()

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


























































#' Parse through nested nodes.
#'
#' @description
#' This function parses through xml nodes. If the node is nested, it recursively
#' goes through the levels. Whenever it finds language nodes, it parses them
#' using the parse_language_node function.
#'
#'
#'
#' @param xml_node A node of the XML file.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
parse_node <- function(xml_node) {

  # Define children
  children_1 <- xml2::xml_children(xml_node)
  children_2 <- xml2::xml_children(children_1)

  # If the node is not nested, we simply parse it
  if (!length(children_2)) {

    data <- parse_unnested_node(xml_node)

  # However, if the node is nested...
  } else {

    # Define the names of the nested elements
    nested_node_names <- children_2 |>
      xml2::xml_parent() |>
      xml2::xml_name()

    # Define the indices of the nested elements
    nested_node_indices <- which(grepl(paste0(nested_node_names, collapse = "|"), children_1))

    # Split the node into unnested and nested nodes
    xml_node_unnested <- children_1[setdiff(seq_along(children_1), nested_node_indices)]
    xml_node_nested <- children_1[nested_node_indices]


    # WORK THROUGH NESTED NODES ================================================


    ## Split Nodes into Language and Other Nested Nodes ------------------------

    # Define children
    children_3 <- xml2::xml_children(xml_node_nested)
    children_4 <- xml2::xml_children(children_3)

    # Get language nodes indices
    xml_node_nested_language_names <- xml2::xml_find_all(children_3, ".//language") |>
      xml2::xml_parent() |>
      xml2::xml_parent() |>
      xml2::xml_name()

    xml_node_nested_language_indices <- which(grepl(paste0(xml_node_nested_language_names, collapse = "|"), xml2::xml_name(xml_node_nested)))

    # Define nested non-language and language nodes
    xml_node_nested_other <- xml_node_nested[setdiff(seq_along(xml_node_nested), xml_node_nested_language_indices)]
    xml_node_nested_language <- xml_node_nested[xml_node_nested_language_indices]


    ## Go to Next Level --------------------------------------------------------


    if (length(xml_node_nested_other)) {

      # by Recursively Calling this function again
      other_node_df <- parse_node(xml_node_nested_other)

    } else {
      other_node_df <- NULL
    }


    ## Parse the Language Nodes ------------------------------------------------


    if (length(xml_node_nested_language)) {

      # Apply the parse_language_node() function to all language nodes
      language_node_list <- lapply(seq_along(xml_node_nested_language), function(index) {
        parse_language_node(xml_node_nested_language[index])
      })

      # Turn list to df
      language_node_df <- dplyr::bind_cols(language_node_list) |>
        dplyr::distinct()

    } else {
      language_node_df <- NULL
    }

      # PARSE THE UNNESTED NODES ===============================================


    if (length(xml_node_unnested)) {
      unnested_node_df <- parse_unnested_node(xml_node_unnested)
    } else {
      unnested_node_df <- NULL
    }

      # BIND DATA ==============================================================


    data <- other_node_df |>
      dplyr::bind_cols(language_node_df) |>
      dplyr::bind_cols(unnested_node_df)


  }



  # INSERT CHECKER FUNCTION HERE






  # # Recursive function to extract all XML elements dynamically
  # extract_node_data <- function(node, parent_name = "") {
  #   children <- xml_children(node)
  #
  #   if (length(children) == 0) {
  #     # Leaf node: Return named list with full path (avoids overwriting similar keys)
  #     return(setNames(list(xml_text(node)), parent_name))
  #   }
  #
  #   # Otherwise, recursively process all children
  #   map_dfr(children, ~ extract_node_data(.x, paste0(parent_name, xml_name(.x), "_")))
  # }
  #
  # # Convert all candidates into a dataframe
  # candidate_df <- map_dfr(candidates, extract_node_data)
  #
  # print(candidate_df)


}





#' Parse through a node that contains nested language elements.
#'
#' @description
#' This function parses through one node that contains multiple nodes that again
#' contain the element language. These elements have to be parsed differentely
#' since they contain the same elements with the same content in different
#' languages. Therefore, the content of the language element needs to be put
#' into the column names of the the other elements when flattening the data.
#'
#'
#' @param language_node A set of nodes of the XML file that contain children
#' that in turn contain the element language.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
parse_language_node <- function(language_node) {

  # Define children and grandchildren
  children_1 <- xml2::xml_children(language_node)
  children_2 <- xml2::xml_children(children_1)

  # Stop, if we are not inside of a language text node
  if (!"language" %in% xml2::xml_name(children_2)) {
    stop(paste0(xml2::xml_name(language_node), " is not a language text node."))
  }

  # Stop, if there are other nodes than language nodes
  if (length(xml2::xml_find_all(language_node, ".//language")) != length(xml2::xml_children(language_node))) {
    stop("The node contains not only language nodes and therefore cannot be parsed.")
  }


  # LANGUAGE NODE TO DF ========================================================


  # Convert XML nodes to a named vector
  data_raw <- stats::setNames(
    object = xml2::xml_text(children_2),
    nm = xml2::xml_name(children_2)
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
  data_tbl <- as.data.frame(structured_list, stringsAsFactors = FALSE, row.names = NULL)


  # RETURN DATAFRAME ===========================================================


  return(data_tbl)

}





#' Parse through an unnested node.
#'
#' @description
#' This function parses through one unnested node.
#'
#'
#' @param unnested_node A node of the XML file that does is not nested (i. e.
#' that does not contain grandchildren).
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
parse_unnested_node <- function(unnested_node) {

  # Turn the unnested node into a string
  unnested_node_unlist <- unnested_node |>
    xml2::as_list() |>
    # Replace empty elements with NAs
    replace(lengths(unnested_node_list) == 0, NA) |>
    unlist()

  # Define names
  names(unnested_node_unlist) <- xml2::xml_name(unnested_node)

  # Transpose and turn to df
  data_tbl <- as.data.frame(t(unnested_node_unlist))

  # Return
  return(data_tbl)

}


