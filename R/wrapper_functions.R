#' Convert an xlsx file into an XML file with the format eCH-0157
#'
#' @description
#' This function transforms an xlsx file in a defined structure into a xml file
#' or vice versa and saves it under the same path.
#' Use the function "open_eCH_0157_xlsx" to open a blank template file.
#'
#' @param file Path to your file.
#' @param type Type of eCH-File you want to transform. Either "" or "".
#' @param overwrite Logical. Whether to overwrite an existing file.
#'
#' @return The function saves the corresponding XML or XLSX file of your
#' defined input file under the same name with only the different file extension.
#' @export
#'
#' @examples
#'
transform_ech <- function(file, type, overwrite = TRUE){


  # CHECK INPUTS ===============================================================


  type <- as.numeric(type)

  if (!type %in% c(157, 252)) {
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
    if (overwrite == TRUE || !file.exists(new_name)) {
      xml2::write_xml(data, new_name)
    }


  # TRANSFORM XML FILE =========================================================


  } else if (endsWith(file, ".xml")) {

    # Define new name
    new_name <- gsub(".xml", ".xlsx", file)

    if (type == 157) {

      # Trasform to xml
      data <- parse_eCH_0157(file)

    } else if (type == 252) {

      # Trasform to xml
      data <- parse_eCH_0252(file)

    }

    # Write xlsx
    if (overwrite == TRUE || !file.exists(new_name)) {
      writexl::write_xlsx(data, new_name)
    }

  }

}





#' Create and save a template XLSX file that can be converted into an XML file
#' in the format eCH-0157
#'
#' @description
#' This function creates an empty xlsx file that can be transformed into an XML
#' file in the format eCH-0157.
#'
#' @inheritParams get_election_table_template
#' @param path A character string specifying the path the file should be written
#' to.
#'
#' @return An XLSX file, saved to the given path.
#' @export
#'
#' @examples
#'
create_election_template <- function(path){

  # Define path to template
  template_path <- system.file("templates", "eCH_0157_export_template.RDS", package = "eCHparser")

  # Create object to export
  file <- readRDS(template_path)

}




#' Create and save a template XLSX file that can be converted into an XML file
#' in the format eCH-0157
#'
#' @description
#' This function creates an empty xlsx file that can be transformed into an XML
#' file in the format eCH-0157.
#'
#' @param path Path to your xlsx file.
#'
#' @return An XLSX file, saved to the given path.
#' @export
#'
#' @examples
#'
read_election_template <- function(path){

  # Read the file
  data <- readxl::read_xlsx(path)

  # Annotate and rename file
  data <- data |>
    dplyr::rename(dplyr::all_of(
      contest_contestDate = "Datum",
      `electionDescriptionInfo-de_electionDescriptionShort` = "Wahlkurzbezeichnung",
      `electionDescriptionInfo-de_electionDescription` = "Wahllangbezeichnung",
      election_numberOfMandates = "Mandate",
      candidate_familyName = "Nachname",
      candidate_firstName = "Vorname",
      candidate_callName = "Rufname",
      candidate_dateOfBirth = "Geburtsdatum",
      candidate_sex = "Geschlecht",
      dwellingAddress_town = "Wohnort",
      `partyAffiliationInfo-de_partyAffiliationShort` = "Parteikurzbezeichnung",
      list_listIndentureNumber = "Listennummer",
      `listDescriptionInfo-de_listDescriptionShort` = "Listenkurzbezeichnung",
      `listDescriptionInfo-de_listDescription` = "Listenlangbezeichnung",
      list_listIndentureNumber = "Listennummer",
      candidatePosition_positionOnList = "Kandidierendenposition",
      candidatePosition_candidateReferenceOnPosition = "Kandidierendennummer",
      list_emptyListPositions = "Leere Zeilen",
      `occupationalTitleInfo-de_occupationalTitle` = "Berufsbezeichnung (und Titel)",
      dwellingAddress_swissZipCode = "PLZ"
    ))



}
