#' Create and save a new template XLSX file that can be converted into an XML
#' file in the format eCH-0157
#'
#' @description
#' This function creates an empty xlsx file that can be transformed into an XML
#' file in the format eCH-0157.
#'
#' @inheritParams get_election_template
#' @param output_path A character string specifying the path the file should be
#' written to.
#' @param overwrite A logical. Determines whether to replace an existing file
#' if it exists under the output_path defined.
#'
#' @return An XLSX file, saved to the given output_path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' new_election_template("path/to/my/new/blank/template.xlsx")
#' }
#'
new_election_template <- function(election_type, output_path, overwrite = FALSE){

  # Get election table template
  data <- get_election_template(election_type = election_type)

  if (!grepl(".xlsx$", output_path)) {
    output_path <- paste0(output_path, ".xlsx")
  }

  # Write file
  if (file.exists(output_path) & overwrite == FALSE) {
    stop("The file already exists and overwrite is set to FALSE. Set overwrite to TRUE if you want to replace the file.")
  } else {
    writexl::write_xlsx(data, output_path)
  }

}





#' Convert an election dataframe (created by read_election_template) to an
#' eCH-0157 XML file and write it to a specified location
#'
#' @description
#' This function turns tabular election data into a valid XML file and saves it
#' to the defined location. The structure of the tabular data corresponds to
#' a data frame created using read_election_template() function on a template,
#' created with the new_election_template() function.
#'
#' @inheritParams build_eCH_0157
#' @param output_path A character string specifying the path the file should be
#' written to.
#'
#' @return A valid eCH-0157 XML file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_eCH_0157(my_df, "path/to/my/xml_file.xml")
#' }
#'
write_eCH_0157 <- function(input_path, output_path, election_type){

  data <- build_eCH_0157(input_path, election_type)

  xml2::write_xml(data, output_path)

}





#' Read filled election template, convert it to an eCH-0157 XML file and write
#' the file to a specified location.
#'
#' @description
#' This function reads tabular election data, the templates for which can be
#' generated with the write_election_template function. It then turns the data
#' into a valid XML file and saves it to the defined location.
#'
#' @inheritParams write_eCH_0157
#' @inheritParams read_election_template
#'
#' @return A valid eCH-0157 XML file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_eCH_0157("path/to/my/template.xlsx", "path/to/my/xml_file.xml")
#' }
#'
convert_to_eCH_0157 <- function(input_path, output_path, election_type, date, election_title_short, election_title_long, mandates){

  data_df <- read_election_template(input_path, election_type, date, election_title_short, election_title_long, mandates)

  write_eCH_0157(data_df, output_path, election_type)

}
