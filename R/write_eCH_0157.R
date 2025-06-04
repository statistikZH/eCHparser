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


  # !!!!!!! DEV-HELPER - DELETE AFTER DEV  =====================================


  # test <- parse_eCH_0157("tests/testthat/testdata/files_unparsed/eCH-0157/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xml")
  # writexl::write_xlsx(test, "/home/file-server/01_Post/Graf/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xlsx")
  # file <- "/home/file-server/01_Post/Graf/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xlsx"
  # target_xml <- xml2::read_xml("tests/testthat/testdata/files_unparsed/eCH-0157/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xml")
  # target_list <- xml2::as_list(target_xml)


  # PREPARE DATA ===============================================================


  # Read xlsx file
  data <- readxl::read_xlsx(file) # This could/should be changed later so that the input is a tibble

  # Split data into contest...
  contest_tbl <- data |>
    dplyr::select(contest_contestIdentification:`contestDescriptionInfo-rm_contestDescription`) |>
    unique()

  # ... election group ballots, ...
  election_group_ballot_tbl <- data |>
    dplyr::select(electionGroupBallot_domainOfInfluenceIdentification) |>
    unique()

  # ...elections, and...
  election_tbl <- data |>
    dplyr::select(
      election_electionIdentification:candidate_candidateIdentification,
      -candidate_candidateIdentification,
      electionGroupBallot_domainOfInfluenceIdentification # as link to the election group ballot
    ) |>
    unique()

  # ...candidates.
  candidates_tbl <- data |>
    dplyr::select(
      candidate_candidateIdentification:ncol(data),
      election_electionIdentification # as link to the election
    )


  # WRITE LIST =================================================================


  # Candidates -----------------------------------------------------------------









  initialDelivery <- list()

  initialDelivery <- append(initialDelivery, list("contest"))


}





#' Create candidate lists
#'
#' @description
#' This helper function transforms candidate information to a candidate list.
#'
#' @param data A tibble containing the necessary candidate information for the eCH-0157.
#'
#' @return An list.
#' @export
#'
#' @examples
#'
create_candidate_list <- function(data){


  # !!!!!!! DEV-HELPER - DELETE AFTER DEV  =====================================


  # data <- candidates_tbl[1, ]


  # PREPARE NESTED LISTS =======================================================


  # Define and transform nested data
  data_nested <- data |>
    dplyr::select(
      dplyr::contains("-")
    ) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "name",
      values_to = "value"
    ) |>
    dplyr::mutate(
      language = sub(".*-([a-zA-Z]{2}).*", "\\1", name),
      name_list_element = sub(".*_", "", name),
      name_parent_element = sub("-.*", "", name)
    )

  # Create lists
  list <- lapply(data_nested$name_parent_element |> unique(), function(list_name) {

    # Filter data
    data_nested_parent <- data_nested |>
      dplyr::filter(name_parent_element == list_name)

    list <- lapply(1:nrow(data_nested_parent), function(element_number) {

      # Select relevant row
      data_nested_child <- data_nested_parent[element_number, ]

      # Build list
      list <- list()
      list[["language"]] <- data_nested_child$language
      list[[data_nested_child$name_list_element]] <- data_nested_child$value

      return(list)

    })

    # Rename list elements
    names(list) <- rep(unique(data_nested_parent$name_list_element), length(list))

    return(list)

  })


  # Split Up -------------------------------------------------------------------


  # Define names
  nested_list_names <- unique(data_nested$name_parent_element)

  for (i in 1:length(nested_list_names)) {
    assign(nested_list_names[i], list[[i]])
  }


  # PREPARE ADDRESS ============================================================


  # Country List ---------------------------------------------------------------


  country <- list(
    "countryId" = list(data$country_countryId),
    "countryIdISO2" = list(data$country_countryIdISO2),
    "countryNameShort" = list(data$country_countryNameShort)
  )




  # STAND HIER




  # PREPARE ORIGIN =============================================================


  # PUT TOGETHER ===============================================================




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
