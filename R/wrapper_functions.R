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
#' @param path A character string specifying the path the file should be
#' written to.
#' @param overwrite A logical. Determines whether to replace an existing file
#' if it exists under the path defined.
#'
#' @return An XLSX file, saved to the given path.
#' @export
#'
#' @examples
#'
write_election_template <- function(election_type, path, overwrite = FALSE){

  # Get election table template
  file <- get_election_table_template(election_type = election_type)

  if (!grepl(".xlsx$", path)) {
    path <- paste0(path, ".xlsx")
  }

  # Write file
  if (file.exists(path) & overwrite == FALSE) {
    stop("The file already exists and overwrite is set to FALSE. Set overwrite to TRUE if you want to replace the file.")
  } else {
    writexl::write_xlsx(file, path)
  }

}




#' Read an xlsx file, created by the write_election_template function
#'
#' @description
#' This function reads tabular election data, the templates for which can be
#' generated with the write_election_template function. Additionally, the user
#' has to define additional information such as the date of the election or the
#' name of it.
#'
#' @param path Path to your xlsx file.
#' @inheritParams get_election_table_template
#' @param date A character string with the format "YYYY-MM-DD".
#' @param election_title_short A character string with a maximum of 100
#' characters.
#' @param election_title_long A character string with a maximum of 255
#' characters.
#' @param mandates A numeric string indicating the number of mandates for the
#' election.
#' @param name description
#' @param name description
#' @param name description
#'
#' @return An XLSX file, saved to the given path.
#' @export
#'
#' @examples
#'
read_election_template <- function(path, election_type, date, election_title_short, election_title_long, mandates){

  # Check inputs
  if (!grepl("^(19|20)\\d{2}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])$", date)) {
    stop("Your \"date\" does not have the correct format. A correct example would be \"2008-09-15\".")
  } else if (nchar(election_title_short > 100)) {
    stop("Your \"election_title_short\" exceeds 100 characters.")
  } else if (nchar(election_title_long > 255)) {
    stop("Your \"election_title_short\" exceeds 255 characters.")
  } else if (!is.numeric(mandates)) {
    stop("Your input \"mandates\" must be numeric.")
  }

  # Read the file
  data <- readxl::read_xlsx(path)

  # Add information from params
  data <- data |>
    dplyr::mutate(
      contest_contestDate = date,
      `electionDescriptionInfo-de_electionDescriptionShort` = election_title_short,
      `electionDescriptionInfo-de_electionDescription` = election_title_long,
      election_numberOfMandates = mandates
    )

  # Annotate and rename file
  data <- data |>
    dplyr::rename(dplyr::all_of(
      candidate_familyName = "nachname",
      candidate_firstName = "amtl_vorname",
      candidate_callName = "pol_vorname",
      candidate_dateOfBirth = "geburtsdatum",
      candidate_sex = "geschlecht",
      strasse = dwellingAddress_street,
      hausnummer = dwellingAddress_houseNumber,
      plz = dwellingAddress_swissZipCode,
      dwellingAddress_town = "ort",
      `partyAffiliationInfo-de_partyAffiliationShort` = "Parteikurzbezeichnung",
      list_listIndentureNumber = "Listennummer",
      `listDescriptionInfo-de_listDescriptionShort` = "Listenkurzbezeichnung",
      `listDescriptionInfo-de_listDescription` = "Listenlangbezeichnung",
      list_listIndentureNumber = "Listennummer",
      candidatePosition_candidateReferenceOnPosition = "Kandidierendennummer",
      list_emptyListPositions = "Leere Zeilen",
      `occupationalTitleInfo-de_occupationalTitle` = "beruf",
      titel = candidate_title,
      dwellingAddress_swissZipCode = "PLZ",
      parteikurzbezeichnung = `partyAffiliationInfo-de_partyAffiliationShort`,
    ))


  # Allgemein zusätzlich
  country_countryId,
  country_countryIdISO2,
  country_countryNameShort,
  swiss_origin,
  candidate_mrMrs,
  candidate_languageOfCorrespondence,
  candidate_candidateReference,

  # Majorz zusätzlich
  kand_nummer = candidate_candidateReference
  parteilangbezeichnung = `partyAffiliationInfo-de_partyAffiliationLong`,





  # Proporz zusätzlich



  # list_listIdentification,
  listennummer = list_listIndentureNumber,
  listenkurzbezeichnung = `listDescriptionInfo-de_listDescriptionShort`,
  listenlangbezeichnung = `listDescriptionInfo-de_listDescription`,
  list_isEmptyList,
  list_listOrderOfPrecedence,
  list_totalPositionsOnList,
  listenposition = candidatePosition_positionOnList,
  kand_nummer = candidatePosition_candidateReferenceOnPosition,
  # candidatePosition_candidateIdentification,
  leere_zeilen = list_emptyListPositions






}
