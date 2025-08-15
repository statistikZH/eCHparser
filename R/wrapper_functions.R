#' Convert an xlsx file into an XML file with the format eCH-0157
#'
#' @description
#' This function transforms an xlsx file in a defined structure into a xml file or vice versa and saves it under the same path.
#' Use the function "open_eCH_0157_xlsx" to open a blank template file.
#'
#' @param file Path to your file.
#' @param type Type of eCH-File you want to transform. Either "" or ""
#'
#' @return An XML or XLSX file.
#' @export
#'
#' @examples
#'
transform_ech <- function(file, type){


  # writexl::write_xlsx(test, "/home/file-server/01_Post/Graf/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xlsx")
  # file <- "/home/file-server/01_Post/Graf/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xlsx"
  # target_xml <- xml2::read_xml("tests/testthat/testdata/files_unparsed/eCH-0157/eCH-0157_abraxas_elections_ZH_majority_2026-06-16.xml")
  # target_list <- xml2::as_list(target_xml)


  # Check inputs
  type <- as.numeric(type)

  if (type %in% c(157, 252)) {
    stop("\"type\" must be either \"0157\" or \"0252\".")
  }

  if(!file.exists(file)) {
    stop("The filepath referenced under \"file\" does not exist. Please make sure that the path given is correct.")
  }

  # Transform file
  if (type == 157) {

    if(endsWith(file, ".xlsx")) {

      # Read xlsx
      # Trasform to xml
      # Write xml

    } else if (endsWith(file, ".xml")) {

      # Read xml
      # Trasform to table
      # Write xlsx

    } else {

      stop("Your file does not seem to be an XLSX or XML file.")

    }
  } else if (type == 252) {

    if (endsWith(file, ".xml")) {

      # Read xml
      # Trasform to table
      # Write xlsx

    } else {

      stop("Your file does not seem to be an XML file. At the moment we can only transform XML to XLSX in the eCH-0252.")

    }

  }

}





#' Create and save a template XLSX file that can be converte into an XML file in the format eCH-0157
#'
#' @description
#' This function creates an empty xlsx file that can be transformed into an XML file in the format eCH-0157.
#'
#' @param path Path to your xlsx file.
#'
#' @return An XLSX file.
#' @export
#'
#' @examples
#'
open_eCH_0157_xlsx <- function(path){

}
