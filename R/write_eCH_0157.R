#' Convert an xlsx file into an XML file with the format eCH-0157
#'
#' @description
#'
#' @param file A dataframe with the defined structure.
#' @param template_xml_path The path to the template xml file providing the structure for the final file.
#'
#' @return An XML file.
#' @export
#'
#' @examples
#'
write_eCH_0157 <- function(file, template_xml_path = ""){


  # Read template XML and preserve namespace structure
  template_doc <- read_xml(template_xml_path)
  ns <- xml_ns(template_doc)

  # Copy root and deliveryHeader from template
  new_doc <- xml_new_root("delivery", ns = ns)
  delivery_header <- xml_find_first(template_doc, ".//eCH-0058:deliveryHeader", ns)
  xml_add_child(new_doc, delivery_header)

  # Create initialDelivery node
  initial_delivery <- xml_add_child(new_doc, "initialDelivery")

  # Helper to construct electionGroupBallot
  build_ballot_node <- function(row) {
    row <- as.list(row)

    ballot <- xml_new_node("electionGroupBallot", ns = ns)
    xml_add_child(ballot, "domainOfInfluenceIdentification", row$domainOfInfluenceIdentification)

    info <- xml_add_child(ballot, "electionInformation")
    election <- xml_add_child(info, xml_new_node("eCH-0155:election", ns = ns))

    xml_add_child(election, xml_new_node("eCH-0155:electionIdentification", ns = ns), row$electionIdentification)
    xml_add_child(election, xml_new_node("eCH-0155:typeOfElection", ns = ns), as.character(row$typeOfElection))
    xml_add_child(election, xml_new_node("eCH-0155:electionPosition", ns = ns), as.character(row$electionPosition))
    xml_add_child(election, xml_new_node("eCH-0155:numberOfMandates", ns = ns), as.character(row$numberOfMandates))

    # Candidate if exists
    if (!is.na(row$candidateFirstName)) {
      candidate <- xml_add_child(info, xml_new_node("eCH-0155:candidate", ns = ns))
      xml_add_child(candidate, xml_new_node("eCH-0155:firstName", ns = ns), row$candidateFirstName)
      xml_add_child(candidate, xml_new_node("eCH-0155:familyName", ns = ns), row$candidateFamilyName)
      xml_add_child(candidate, xml_new_node("eCH-0155:dateOfBirth", ns = ns), as.character(row$candidateDOB))
      # Add more candidate subfields as needed
    }

    return(ballot)
  }

  # Apply to all rows using lapply
  ballots <- lapply(1:nrow(data), function(i) build_ballot_node(data[i, ]))
  lapply(ballots, function(node) xml_add_child(initial_delivery, node))

  # Save
  write_xml(new_doc, output_xml_path, options = "format")
  message("XML successfully written to: ", output_xml_path)




}










add_multilingual_nodes <- function(parent_node, data_row, xml_block_name, field_prefix, ns) {
  # Find all columns matching this multilingual block
  pattern <- paste0("^", field_prefix, "-([a-z]{2})_(\\w+)$")
  lang_cols <- grep(pattern, names(data_row), value = TRUE)

  if (length(lang_cols) == 0) return(invisible(NULL))

  # Get unique languages and subfields
  lang_field_matrix <- stringr::str_match(lang_cols, pattern)
  langs <- unique(lang_field_matrix[, 2])

  for (lang in langs) {
    info_node <- xml_add_child(parent_node, xml_block_name, ns = ns)
    xml_add_child(info_node, "language", lang)

    # For each field in that language
    fields <- lang_field_matrix[lang_field_matrix[, 2] == lang, , drop = FALSE]
    for (i in seq_len(nrow(fields))) {
      col_name <- paste0(field_prefix, "-", lang, "_", fields[i, 3])
      xml_add_child(info_node, fields[i, 3], data_row[[col_name]])
    }
  }
}

















contest_node <- xml_add_child(parent, xml_new_node("eCH-0155:contestDescription", ns = ns))
add_multilingual_nodes(
  parent_node = contest_node,
  data_row = row,
  xml_block_name = "eCH-0155:contestDescriptionInfo",
  field_prefix = "contestDescriptionInfo",
  ns = ns
)






























#' Convert an xlsx file into an XML file with the format eCH-0157
#'
#' @description
#'
#' @param file Path to your xlsx file.
#'
#' @return An XML file.
#' @export
#'
#' @examples
#'



