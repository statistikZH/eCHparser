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
#' @return A dataframe that can be transformed into a valid eCH-0157 XML file
#' with the write_eCH_0157() function.
#' @export
#'
#' @examples
#'
read_election_template <- function(path, election_type, date, election_title_short, election_title_long, mandates){

  # Transform input params
  election_type <- tolower(election_type)

  # Read the file
  data <- readxl::read_xlsx(path)

  # Check input params
  if (!grepl("^(19|20)\\d{2}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])$", date)) {
    stop("Your \"date\" does not have the correct format. A correct example would be \"2008-09-15\".")
  } else if (nchar(election_title_short > 100)) {
    stop("Your \"election_title_short\" exceeds 100 characters.")
  } else if (nchar(election_title_long > 255)) {
    stop("Your \"election_title_short\" exceeds 255 characters.")
  } else if (!is.numeric(mandates)) {
    stop("Your input \"mandates\" must be numeric.")
  } else if (!election_type %in% c("proportion", "majority")) {
    stop("The parameter \"election_type\" must be either \"Majority\" or \"Proportion\". ")
  }





  # DEV =================================================================================================================
  data <- readRDS("inst/templates/eCH-0157_majority_table_template.RDS")
  data <- readRDS("inst/templates/eCH-0157_proportion_table_template.RDS")


  # NOTE ================================================================================================================
  # ATM, the bisher column is missing since it is not being exported in the eCH-0157 files.
  # Once it will be included, all the templates have to be replaced.


  election_type <- "proportional"
  date <- "2027-06-16"
  election_title_short <- "Testwahl Prop"
  election_title_long <- "Testwahl Proporz - offizielle Langbezeichnung"
  mandates <- 5
  # DEV =================================================================================================================




  # Check input file
  base_msg <- "Die Datei kann nicht verarbeitet werden. "

  if (election_type == "majority" && sort(names(data)) != sort(names(readRDS("inst/templates/eCH-0157_majority_table_template.RDS")))) {
    stop(paste0(base_msg, "Es sind nicht alle nötigen Spalten aus dem Template vorhanden."))
  } else if (election_type == "proportion" && sort(names(data)) != sort(names(readRDS("inst/templates/eCH-0157_proportion_table_template.RDS")))) {
    stop(paste0(base_msg, "Es sind nicht alle nötigen Spalten aus dem Template vorhanden."))
  } else if (any(sapply(data, function(x) any(grepl(c("[", "]"), x))))) { # gesamtes file auf Sonderzeichen prüfen
    stop(paste0(base_msg, "Die Datei enthält nicht erlaubte Sonderzeichen."))
  } else if (any(is.na(data$nachname) | nchar(data$nachname) > 100)) {
    stop(paste0(base_msg, "Es muss für alle Kandidierenden ein Nachname von maximal 100 Zeichen erfasst sein."))
  } else if (any(nchar(data$amtl_vorname) > 100)) {
    stop(paste0(base_msg, "Vornamen dürfen maximal 100 Zeichen lang sein."))
  } else if (any(is.na(data$pol_vorname) | nchar(data$pol_vorname) > 100)) {
    stop(paste0(base_msg, "Es muss für alle Kandidierenden ein Vorname von maximal 100 Zeichen erfasst sein."))
  } else if (any(!grepl("\\d{2}\\.\\d{2}\\.\\d{4}", data$geburtsdatum))) {
    stop(paste0(base_msg, "Alle Geburtsdaten müssen im Format TT.MM.JJJJ erfasst sein (also z. B. 19.04.1979)."))
  } else if (any(!tolower(data$geschlecht) %in% c("männlich", "mann", "m", "weiblich", "w", "frau", "f"))) {
    stop(paste0(base_msg, "Das Geschlecht aller Kandidierenden muss gemäss den Informationen aus dem Einwohnerregister als \"m\" oder \"w\" erfasst sein."))
  } else if (any(!tolower(data$bisher) %in% c("ja", "nein", NA))) {
    stop(paste0(base_msg, "Die Spalte bisher darf nur die Werte \"Ja\" oder \"Nein\" enthalten oder leer sein."))
  } else if (any(nchar(data$beruf) > 250)) {
    stop(paste0(base_msg, "Die Berufsbezeichnung darf maximal 250 Zeichen enthalten."))
  } else if (any(nchar(data$titel) > 250)) {
    stop(paste0(base_msg, "Der Titel darf maximal 250 Zeichen enthalten."))
  } else if (any(nchar(data$strasse) > 150)) {
    stop(paste0(base_msg, "Die Strasse darf maximal 150 Zeichen enthalten."))
  } else if (any(nchar(data$hausnummer) > 30)) {
    stop(paste0(base_msg, "Die Hausnummer darf maximal 30 Zeichen enthalten."))
  } else if (any(!nchar(data$plz) == 4) | !is.numeric(data$plz)) {
    stop(paste0(base_msg, "Die Postleitzahl muss genau 4 Ziffern lang sein."))
  } else if (any(nchar(data$ort) > 40)) {
    stop(paste0(base_msg, "Der Wohnort darf maximal 40 Zeichen enthalten."))
  } else if (any(nchar(data$kand_nummer) > 10)) {
    stop(paste0(base_msg, "Die Kandidierendennummer darf maximal 10 Zeichen enthalten."))
  } else if (any(nchar(data$parteikurzbezeichnung) > 12)) {
    stop(paste0(base_msg, "Die Parteikurzbezeichnung darf maximal 12 Zeichen enthalten."))
  } else if (election_type == "majority" & any(nchar(data$parteilangbezeichnung)  > 100)) {
    stop(paste0(base_msg, "Die Parteibezeichnung darf maximal 100 Zeichen enthalten."))
  } else if (election_type == "proportion" & any(!is.numeric(data$listenposition))) {
    stop(paste0(base_msg, "Die Listenposition muss eine Zahl sein. Sie bezeichnet die genaue Position von Kandidierenden auf der Liste."))
  } else if (election_type == "proportion" & any(nchar(data$listennummer) > 12)) {
    stop(paste0(base_msg, "Die Listennummer darf maximal 12 Zeichen enthalten."))
  } else if (election_type == "proportion" & any(nchar(data$listenkurzbezeichnung) > 20)) {
    stop(paste0(base_msg, "Die Listenkurzbezeichnung darf maximal 20 Zeichen enthalten."))
  } else if (election_type == "proportion" & any(nchar(data$listenlangbezeichnung) > 100)) {
    stop(paste0(base_msg, "Die Listenbezeichnung darf maximal 100 Zeichen enthalten."))
  } else if (election_type == "proportion" & any(!is.numeric(data$leere_zeilen))) {
    stop(paste0(base_msg, "Die Anzahl leere Zeilen muss eine Zahl sein."))
  }

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
    # rename variables
    dplyr::rename(dplyr::all_of(c(
      candidate_familyName = "nachname",
      candidate_firstName = "amtl_vorname",
      candidate_callName = "pol_vorname",
      candidate_dateOfBirth = "geburtsdatum",
      candidate_sex = "geschlecht",
      candidate_incumbentYesNo = "bisher",
      dwellingAddress_street = "strasse",
      dwellingAddress_houseNumber = "hausnummer",
      dwellingAddress_swissZipCode = "plz",
      dwellingAddress_town = "ort",
      `partyAffiliationInfo-de_partyAffiliationShort` = "parteikurzbezeichnung",
      `occupationalTitleInfo-de_occupationalTitle` = "beruf",
      candidate_title = "titel"
    ))) |>
    dplyr::mutate(
      # adjust first name
      candidate_firstName = ifelse(is.na(candidate_firstName), candidate_callName, candidate_firstName),
      # adjust date of birth
      candidate_dateOfBirth = as.character(paste0(
        stringr::str_sub(candidate_dateOfBirth, 7, 10),
        "-",
        stringr::str_sub(candidate_dateOfBirth, 4, 5),
        "-",
        stringr::str_sub(candidate_dateOfBirth, 1, 2)
      )),
      # change sex to numeric
      candidate_sex = ifelse(tolower(candidate_sex) %in% c("m", "männlich", "mann", "herr"), 1, 2),
      candidate_incumbentYesNo = ifelse(tolower(candidate_incumbentYesNo) == "yes", "true", "false"),
      # add variables
      country_countryId = 8100,
      country_countryIdISO2 = "CH",
      country_countryNameShort = "Schweiz",
      # swiss_origin = ,
      candidate_mrMrs = ifelse(candidate_sex == 1, 2, 1), # eCH-0010 and 0044 have different numerics for male/mr. -.-
      candidate_languageOfCorrespondence = "de"
    )

  # Majority specific adjustments
  if (tolower(election_type) == "majority") {

    data <- data |>
      dplyr::rename(dplyr::all_of(c(
        candidate_candidateReference = "kand_nummer",
        `partyAffiliationInfo-de_partyAffiliationLong` = "parteilangbezeichnung"
      ))) |>
      dplyr::mutate(
        candidate_candidateReference = stringr::str_pad(candidate_candidateReference, 2, "left", "0")
      )

  }

  # Proportion specific adjustments
  if (tolower(election_type) == "proportion") {

    data <- data |>
      dplyr::rename(dplyr::all_of(c(
        list_listIndentureNumber = "listennummer",
        `listDescriptionInfo-de_listDescriptionShort` = "listenkurzbezeichnung",
        `listDescriptionInfo-de_listDescription` = "listenlangbezeichnung",
        list_emptyListPositions = "leere_zeilen",
        candidatePosition_positionOnList = "listenposition",
        candidatePosition_candidateReferenceOnPosition = "kand_nummer"
      ))) |>
      dplyr::group_by(list_listIndentureNumber) |>
      dplyr::mutate(n_kand = n()) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        list_listIndentureNumber = stringr::str_pad(as.numeric(list_listIndentureNumber), 2, "left", "0"),
        # candidate number: padded list number, padded last two positions on the cand number given
        candidatePosition_candidateReferenceOnPosition = paste0(
          list_listIndentureNumber,
          ".",
          stringr::str_pad(stringr::str_remove(candidatePosition_candidateReferenceOnPosition, 2, "left", ellipsis = ""), 2, "left", "0")
        ),
        # list_emptyListPositions = mandates - n_kand,
        list_isEmptyList = "false",
        list_listOrderOfPrecedence,
        list_totalPositionsOnList,
        candidatePosition_candidateIdentification,
        list_listIdentification
      )

  }

}
