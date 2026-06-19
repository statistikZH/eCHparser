#' Strip namespaces.
#'
#' @description
#' This function strips all namespaces of eCH-0157 and eCH-0252 XML files.
#'
#' @param input_path A character vector to an xml-document.
#'
#' @return A list of class xml_document.
#'
#' @export
#'
#' @examples
#' # Load file
#' xml_data <- xml2::read_xml(
#'   system.file(
#'     "extdata",
#'     "eCH-0252_abraxas_vote_ZH_counting_2024-11-24.xml",
#'     package = "eCHparser"
#'   )
#' )
#'
#' # Strip namespaces
#' xml_data_stripped <- strip_namespaces(xml_data)
#'
strip_namespaces <- function(input_path) {

  xml_text <- paste(readLines(input_path, warn = FALSE), collapse = "\n")

  if (grepl("<deliveryHeader>", xml_text)) {
    xml_text <- gsub(' xmlns(?::[a-zA-Z0-9]+)?="[^"]*"', "", xml_text, perl = TRUE)
  } else {
    xml_text <- gsub("eCH-\\d{4}:", "", xml_text)
  }

  xml2::read_xml(xml_text)

}





#' Amend node parents' names with node specification
#'
#' @description
#' Certain nodes have a different logic, insofar as they do not themselves
#' specify their content in the name but do this using one of the children.
#' Examples of this are the namedElement or the otherIdentification nodes, or
#' also nodes, that contain multilingual content. To create unique names that
#' define their content, we specify their parents' names.
#'
#' @param node An xml node.
#' @param spec_element The name of the specification element that contains text.
#'
#' @return An xml node.
#'
specify_node <- function(node, spec_element) {

  # Define search tag
  search_tag <- paste0(".//", spec_element)

  # Define all specification nodes
  spec_nodes <- xml2::xml_find_all(node, search_tag)

  # Get all specifications
  specs <- spec_nodes |>
    xml2::xml_text()

  # Define parent nodes
  parent_nodes <- xml2::xml_parent(spec_nodes)

  # Get all parents' original names
  parents_names <- xml2::xml_name(parent_nodes)

  # Combine original names with specification to create unique names and rename nodes
  xml2::xml_name(parent_nodes) <- paste(parents_names, specs, sep = "-")

  # Remove specification nodes
  xml2::xml_remove(spec_nodes)

}





#' Amend voter information subtotalInfo nodes with node specifications
#'
#' @param node An xml node.
#'
#' @return An xml node.
#'
specify_voter_node <- function(node) {


  # AMEND WITH VOTER TYPE ======================================================


  # Define all voterType nodes
  voterType_nodes <- xml2::xml_find_all(node, ".//voterType")

  # Get all voterTypes
  voterTypes <- voterType_nodes |>
    xml2::xml_text()

  # Define parent nodes
  parent_nodes <- xml2::xml_parent(voterType_nodes)

  # Get all parents' original names
  parents_names <- xml2::xml_name(parent_nodes)

  # Combine original names with specification to create unique names and rename nodes
  xml2::xml_name(parent_nodes) <- paste0(parents_names, "-voterType", voterTypes)

  # Remove specification nodes
  xml2::xml_remove(voterType_nodes)


  # AMEND WITH SEX =============================================================


  # Define all sex nodes
  sex_nodes <- xml2::xml_find_all(node, ".//sex")

  # Define parent nodes
  parent_nodes <- xml2::xml_parent(sex_nodes)

  # Drop all write-in candidates (necessary exception for 252 maj elections)
  writeIn_indices <- grep("candidateIdentification", parent_nodes)
  sex_nodes[writeIn_indices] <- NULL
  parent_nodes[writeIn_indices] <- NULL

  # Get all sexes
  sexes <- sex_nodes |>
    xml2::xml_text()

  # Get all parents' original names
  parents_names <- xml2::xml_name(parent_nodes)

  # Combine original names with specification to create unique names and rename nodes
  xml2::xml_name(parent_nodes) <- paste0(parents_names, "-sex", sexes)

  # Remove specification nodes
  xml2::xml_remove(sex_nodes)

}





#' Convert long dataframe to wide
#'
#' @param data A long dataframe.
#'
#' @return A dataframe.
#'
to_wide <- function(data){

  if ("var" %in% names(data)){
    data <- data |>
      # Delete var to make the pivot work
      dplyr::select(-var)
  }

  data |>
    tidyr::pivot_wider(
      names_from = var_short,
      values_from = data,
      values_fn = list
    )

}





#' Unlist list into a dataframe
#'
#' @param data A dataframe.
#' @param names A string of variable names.
#'
#' @return A dataframe.
#'
to_df <- function(data, names){

  data.frame(
    data = data,
    var = names
  ) |>
    dplyr::mutate(
      # Keep only string after second to last "."
      var_short = gsub("^.*?\\.([^.]+\\.[^.]+)$", "\\1", var),
      # Remove digits followed by an underscore at the start of the string
      var_short = gsub("^\\d+_", "", var_short),
      # Replace "." with "_"
      var_short = gsub("\\.", "_", var_short)
    )

}





#' Extract all attributes from a list
#'
#' @param my_list A list.
#' @param path A character vector of length 1.
#' @param results A list.
#'
#' @return A list.
#'
extract_attributes <- function(my_list, path = character(), results = list()){

  if (is.list(my_list)) {

    for (i in seq_along(my_list)) {

      element <- my_list[[i]]
      name <- names(my_list)[i]

      # Get namespace if available
      ns <- attr(element, "xmlns")

      # Construct full path for clarity (optional)
      full_path <- c(path, name)

      # Save result
      results[[length(results) + 1]] <- list(
        path = paste(full_path, collapse = "/"),
        name = name,
        namespace = ns
      )

      # Recurse if the element is also a list
      if (is.list(element)) {
        results <- extract_attributes(element, path = full_path, results = results)
      }

      # Remove all elements with no namespaces
      results[sapply(results, function(sublist) !is.null(sublist$namespace))]
    }

  }

  # Drop duplicates
  results <- unique(results)

  return(results)

}





#' Assign attributes by their path
#'
#' @param my_list A list.
#' @param ns_info A list of path-namespace pairs.
#' @param path An empty character vector.
#'
#' @return A list.
#'
assign_namespaces_by_path <- function(my_list, ns_info, path = character()) {

  for (i in seq_along(my_list)) {

    name <- names(my_list)[i]
    elem <- my_list[[i]]

    full_path <- c(path, name)
    path_str <- paste(full_path, collapse = "/")

    # Find matching entry in ns_info by full path
    match_idx <- which(vapply(ns_info, function(info) {
      is.list(info) && !is.null(info$path) && info$path == path_str
    }, logical(1)))

    if (length(match_idx) > 0 && is.list(elem)) {
      ns <- ns_info[[match_idx[1]]]$namespace
      attr(elem, "xmlns") <- ns
    }

    # Recurse if the element is a list
    if (is.list(elem)) {
      elem <- assign_namespaces_by_path(elem, ns_info, path = full_path)
    }

    my_list[[i]] <- elem

  }

  return(my_list)

}





#' Remove all empty elements from nested list.
#'
#' @param my_list A list.
#'
#' @return A list.
#'
clean_list <- function(my_list) {
  if (is.list(my_list)) {
    # Recursively clean children
    my_list <- lapply(my_list, clean_list)

    # Drop children that became NULL
    my_list <- my_list[!vapply(my_list, is.null, logical(1))]

    # If list is empty after cleaning, remove it
    if (length(my_list) == 0) return(NULL)
    return(my_list)
  }

  # Atomic case
  if (length(my_list) == 0) return(NULL)  # remove empty vectors

  # Turn "NA" strings into actual NA
  if (is.character(my_list)) {
    my_list[my_list == "NA"] <- NA_character_
  }

  # Remove element if *all* entries are NA
  if (all(is.na(my_list))) return(NULL)

  return(my_list)
}





#' Load election table template
#'
#' @param election_type A character string specifying the election type.
#' Must be either "Majority" or "Proportion".
#'
#' @return A dataframe.
#'
#' @export
#'
#' @examples
#' my_template <- get_election_template("Proportion")
#'
get_election_template <- function(election_type) {

  # Transform input param
  election_type <- tolower(election_type)

  # Check input
  if (!election_type %in% c("proportion", "majority")) {
    stop("The parameter \"election_type\" must be either \"Majority\" or \"Proportion\". ")
  }

  # Define path to template
  template_path <- system.file("templates", paste0("eCH-0157_", election_type, "_table_template.RDS"), package = "eCHparser")

  # Get and return template
  template <- readRDS(template_path)
  return(template)

}
