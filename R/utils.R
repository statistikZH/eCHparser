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


  # HANDLE NESTED NODES ========================================================


  # Check if there are nested nodes
  if (length(children_2)) {

    # Define the names of the nested elements
    nested_node_names <- children_2 |> # could we replace this with just childre_1 |> xml_name()?
      xml2::xml_parent() |>
      xml2::xml_name()

    # Define the indices of the nested elements
    nested_node_indices <- which(grepl(paste0(nested_node_names, collapse = "|"), children_1))

    # Split the node into unnested and nested nodes
    xml_node_unnested <- children_1[setdiff(seq_along(children_1), nested_node_indices)]
    xml_node_nested <- children_1[nested_node_indices]


    ## Work Through Nested Nodes -----------------------------------------------


    # Define children
    children_3 <- xml2::xml_children(xml_node_nested)
    children_4 <- xml2::xml_children(children_3)

    # Check for language nodes
    if (length(xml2::xml_find_all(children_3, ".//language"))) {

      # Get the names of the multilingual nodes
      xml_node_nested_language_names <- xml2::xml_find_all(children_3, ".//language") |>
        xml2::xml_parent() |>
        xml2::xml_parent() |>
        xml2::xml_name()

      # Then, get their indices (if returns integer(0) it means that there are no multilingual nodes ON THIS LEVEL)
      xml_node_nested_language_indices <- which(grepl(paste0(xml_node_nested_language_names, collapse = "|"), xml2::xml_name(xml_node_nested)))

      # Define nested non-language and language nodes
      xml_node_nested_other <- xml_node_nested[setdiff(seq_along(xml_node_nested), xml_node_nested_language_indices)]
      xml_node_nested_language <- xml_node_nested[xml_node_nested_language_indices]

      # If there are no langauge nodes, define all nodes as other nested nodes
    } else {
      xml_node_nested_other <- xml_node_nested
    }


    ### Go to Next Level -------------------------------------------------------


    # Check if we have nested non-language nodes
    if (length(xml_node_nested_other)) {

      # We don't need this since we arrived at the bottom of the nodeset and we are always only looking at and parsing one node with some children. Therefore, no lapply needed.
      # # Recursively run this function for every one of these nested nodes
      # other_node_list <- lapply(seq_along(xml_node_nested_other), function(index) {
      #   parse_node(xml_node_nested_other[index])
      # })
      #
      # # Bind the elements of the list (simple, since in here, there can be no complex structures, we are at the bottom of the node and have no nestings anymore)
      # other_node_df <- dplyr::bind_cols(other_node_list)

      # Recursively call this function again to work through the next level and create a df
      other_node_df <- parse_node(xml_node_nested_other)

    # If there are no nested non-language nodes, define NULL object
    } else {
      other_node_df <- NULL
    }


    ### Parse the Language Nodes -----------------------------------------------


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


    ### Join Nested Node Data --------------------------------------------------


    nested_nodes_df <- other_node_df |>
      dplyr::bind_cols(language_node_df) # This only works if both dataframes have only one (or the same number of) row(s), which should be the case inside a node that contains language nodes

  # If there are no nested nodes...
  } else {

    # ...define a NULL object for the nested_nodes_df...
    nested_nodes_df <- NULL

    # ...and also define all children_1 as unnested nodes
    xml_node_unnested <- children_1

  }


  # HANDLE UNNESTED NODES ======================================================


  # If there are any...
  if(length(xml_node_unnested)) {

    # Parse through the unnested node
    unnested_node_df <- parse_unnested_node(xml_node_unnested)

  } else {

    # Otherwise, create NULL object
    unnested_node_df <- NULL

  }


  # BIND DATA AND RETURN =======================================================


  # Not sure if this works: We might need to handle the issue of dfs of different nrow()s. So maybe work with left_join() (add id first, then join, then delete id)
  data <- unnested_node_df |> # start with unnested_node_df since there is certainly something here
    dplyr::bind_cols(nested_nodes_df)

  return(data)

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





#' Parse through one node of unnested nodes.
#'
#' @description
#' This function parses through one node of only unnested nodes.
#'
#'
#' @param unnested_node A node of the XML file that is not nested (i. e.
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
  unnested_node_list <- unnested_node |>
    xml2::as_list()

  # Replace empty elements with NAs
  unnested_node_unlist <- unnested_node_list |>
    replace(lengths(unnested_node_list) == 0, NA) |>
    unlist()

  # Define names
  names(unnested_node_unlist) <- xml2::xml_name(unnested_node)

  # Transpose and turn to df
  data_tbl <- as.data.frame(t(unnested_node_unlist))

  # Return
  return(data_tbl)

}


