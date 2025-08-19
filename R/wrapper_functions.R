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


  # CHECK INPUTS ===============================================================


  type <- as.numeric(type)

  if (type %in% c(157, 252)) {
    stop("\"type\" must be either \"0157\" or \"0252\".")
  }

  if(!file.exists(file)) {
    stop("The filepath referenced under \"file\" does not exist. Please make sure that the path given is correct.")
  }


  # TRANSFORM XLSX FILE ========================================================


  if (endsWith(file, ".xlsx")) {

    # Define new name
    new_name <- gsub(".xlsx", ".xml", file)

    # Read xlsx
    data <- readxl::read_xlsx(file)

    if (type == 157) {

      # Trasform to xml
      data <- write_eCH_0157(data)

    } else {

      stop("Unfortunately we can only transform XML to XLSX in eCH-0252.")

    }

    # Write xml
    xml2::write_xml(data, new_name)


  # TRANSFORM XML FILE =========================================================


  } else if (endsWith(file, ".xml")) {

    # Define new name
    new_name <- gsub(".xml", ".xslx", file)

    # Read xlsx
    data <- xml2::read_xml(file)

    if (type == 157) {

      # Trasform to xml
      data <- parse_eCH_0157(data)

    } else if (type == 252) {

      # Trasform to xml
      data <- parse_eCH_0252(data)

    }

    # Write xml
    xml2::write_xml(data, new_name)

  }

}





#' Create and save a template XLSX file that can be converted into an XML file in the format eCH-0157
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

  # file <- read

}
