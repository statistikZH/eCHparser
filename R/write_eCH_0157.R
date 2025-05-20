#' Convert an xlsx file into an XML file with the format eCH-0157
#'
#' @description
#' This function transforms an xlsx file in a defined structure into a xml file in the format eCH-0157.
#' Use the function "open_eCH_0157_xlsx" to open a blank template file.
#'
#' @param file Path to your xlsx file.
#' @param template_xml_path The path to the template xml file that provides the structure for the output xml file.
#'
#' @return An XML file.
#' @export
#'
#' @examples
#'
write_eCH_0157 <- function(file, template_xml_path = system.file("templates", "eCH_0157_template.xml", package = "eCHparser")){

  # Read xlsx file
  data <- readxl::read_xlsx(file)

  # Split data into contest, election ballots, elections, and candidates
  contest_tbl
  election


  unique(data$contest_contestIdentification)

}









































#' #' Convert an xlsx file into an XML file with the format eCH-0157
#' #'
#' #' @description
#' #' This function transforms an xlsx file in a defined structure into a xml file in the format eCH-0157.
#' #' Use the function "open_eCH_0157_xlsx" to open a blank template file.
#' #'
#' #' @param file Path to your xlsx file.
#' #' @param template_xml_path The path to the template xml file that provides the structure for the output xml file.
#' #'
#' #' @return An XML file.
#' #' @export
#' #'
#' #' @examples
#' #'
#' write_eCH_0157 <- function(file, template_xml_path = system.file("templates", "eCH_0157_template.xml", package = "eCHparser")){
#'
#'   # Read xlsx file
#'   data <- readxl::read_xlsx(file)
#'
#'   # Read template XML and preserve namespace structure
#'   template_doc <- xml2::read_xml(template_xml_path)
#'   ns <- xml2::xml_ns(template_doc)
#'
#'   # Copy root and deliveryHeader from template
#'   new_doc <- xml2::xml_new_root("delivery", ns = ns)
#'   delivery_header <- xml2::xml_find_first(template_doc, ".//eCH-0058:deliveryHeader", ns)
#'   xml2::xml_add_child(new_doc, delivery_header)
#'
#'   # Create initialDelivery node
#'   initial_delivery <- xml2::xml_add_child(new_doc, "initialDelivery")
#'
#'   # Apply to all rows using lapply
#'   ballots <- lapply(1:nrow(data), function(i, initial_delivery, ns) {
#'     build_ballot_node(data[i, ])
#'   })
#'
#'   lapply(ballots, function(node) {
#'     xml_add_child(initial_delivery, node)
#'   })
#'
#'   # Save
#'   write_xml(new_doc, output_xml_path, options = "format")
#'   message("XML successfully written to: ", output_xml_path)
#'
#'
#'
#'
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' add_multilingual_nodes <- function(parent_node, data_row, xml_block_name, field_prefix, ns) {
#'   # Find all columns matching this multilingual block
#'   pattern <- paste0("^", field_prefix, "-([a-z]{2})_(\\w+)$")
#'   lang_cols <- grep(pattern, names(data_row), value = TRUE)
#'
#'   if (length(lang_cols) == 0) return(invisible(NULL))
#'
#'   # Get unique languages and subfields
#'   lang_field_matrix <- stringr::str_match(lang_cols, pattern)
#'   langs <- unique(lang_field_matrix[, 2])
#'
#'   for (lang in langs) {
#'     info_node <- xml_add_child(parent_node, xml_block_name, ns = ns)
#'     xml_add_child(info_node, "language", lang)
#'
#'     # For each field in that language
#'     fields <- lang_field_matrix[lang_field_matrix[, 2] == lang, , drop = FALSE]
#'     for (i in seq_len(nrow(fields))) {
#'       col_name <- paste0(field_prefix, "-", lang, "_", fields[i, 3])
#'       xml_add_child(info_node, fields[i, 3], data_row[[col_name]])
#'     }
#'   }
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' contest_node <- xml_add_child(parent, xml_new_node("eCH-0155:contestDescription", ns = ns))
#' add_multilingual_nodes(
#'   parent_node = contest_node,
#'   data_row = row,
#'   xml_block_name = "eCH-0155:contestDescriptionInfo",
#'   field_prefix = "contestDescriptionInfo",
#'   ns = ns
#' )
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' #' Helper to construct electionGroupBallot
#' #'
#' #' @description
#' #'
#' #' @param row ?
#' #'
#' #' @return ?.
#' #' @export
#' #'
#' #' @examples
#' #'
#' build_ballot_node <- function(row, initial_delivery, ns) {
#'
#'   row <- as.list(row)
#'
#'   # Add electionGroupBallot under initialDelivery
#'   ballot <- xml2::xml_add_child(initial_delivery, "electionGroupBallot", ns = ns)
#'   xml2::xml_add_child(ballot, "domainOfInfluenceIdentification", row$domainOfInfluenceIdentification)
#'
#'   # Add electionInformation under ballot
#'   info <- xml2::xml_add_child(ballot, "electionInformation")
#'
#'   # Add election node under info
#'   election <- xml2::xml_add_child(info, "eCH-0155:election", ns = ns)
#'
#'   # Add child elements to election
#'   xml2::xml_add_child(election, "eCH-0155:electionIdentification", row$electionIdentification, ns = ns)
#'   xml2::xml_add_child(election, "eCH-0155:typeOfElection", as.character(row$typeOfElection), ns = ns)
#'   xml2::xml_add_child(election, "eCH-0155:electionPosition", as.character(row$electionPosition), ns = ns)
#'   xml2::xml_add_child(election, "eCH-0155:numberOfMandates", as.character(row$numberOfMandates), ns = ns)
#'
#'   # Add candidate if present
#'   if (!is.na(row$candidateFirstName)) {
#'     candidate <- xml2::xml_add_child(info, "eCH-0155:candidate", ns = ns)
#'     xml2::xml_add_child(candidate, "eCH-0155:firstName", row$candidateFirstName, ns = ns)
#'     xml2::xml_add_child(candidate, "eCH-0155:familyName", row$candidateFamilyName, ns = ns)
#'     xml2::xml_add_child(candidate, "eCH-0155:dateOfBirth", as.character(row$candidateDOB), ns = ns)
#'     # Additional fields can be added here
#'   }
#'
#'   return(ballot)
#' }
