#' Read an xlsx file, created by the write_election_template function
#'
#' @description
#' This function reads tabular election data, the templates for which can be
#' generated with the write_election_template function. The user has to define
#' additional information such as the date of the election or the name of it.
#'
#' @param input_path Path to your xlsx file.
#' @inheritParams get_election_template
#' @param date A character string with the format "YYYY-MM-DD".
#' @param election_title_short A character string with a maximum of 100
#' characters.
#' @param election_title_long A character string with a maximum of 255
#' characters.
#' @param mandates A numeric string indicating the number of mandates for the
#' election.
#'
#' @return A dataframe that can be transformed into a valid eCH-0157 XML file
#' with the write_eCH_0157() function.
#' @export
#'
#' @examples
#' \dontrun{
#' my_df <- read_election_template("path/to/my/template.xlsx")
#' }
#'
read_election_template <- function(input_path, election_type, date, election_title_short, election_title_long, mandates){

  # Transform input params
  election_type <- tolower(election_type)

  # Read the file
  data <- readxl::read_xlsx(input_path)

  # Check input params
  if (!grepl("^(19|20)\\d{2}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])$", date)) {
    stop("Your \"date\" does not have the correct format. A correct example would be \"2008-09-15\".")
  } else if (nchar(election_title_short) > 100) {
    stop("Your \"election_title_short\" exceeds 100 characters.")
  } else if (nchar(election_title_long) > 255) {
    stop("Your \"election_title_long\" exceeds 255 characters.")
  } else if (!is.numeric(mandates)) {
    stop("Your input \"mandates\" must be numeric.")
  } else if (!election_type %in% c("proportion", "majority")) {
    stop("The parameter \"election_type\" must be either \"Majority\" or \"Proportion\". ")
  }

  # Check input file
  base_msg <- "Die Datei kann nicht verarbeitet werden. "

  # General checks
  if (any(is.na(data$nachname) | nchar(data$nachname) > 100)) {
    stop(paste0(base_msg, "Es muss für alle Kandidierenden ein Nachname von maximal 100 Zeichen erfasst sein."))
  } else if (any(nchar(data$amtl_vorname, keepNA = FALSE) > 100)) {
    stop(paste0(base_msg, "Vornamen dürfen maximal 100 Zeichen lang sein."))
  } else if (any(is.na(data$pol_vorname) | nchar(data$pol_vorname) > 100)) {
    stop(paste0(base_msg, "Es muss für alle Kandidierenden ein Vorname von maximal 100 Zeichen erfasst sein."))
    # } else if (any(!grepl("\\d{2}\\.\\d{2}\\.\\d{4}", data$geburtsdatum))) {
  } else if (any(!grepl("\\d{4}\\-\\d{2}\\-\\d{2}", data$geburtsdatum))) {
    stop(paste0(base_msg, "Alle Geburtsdaten müssen im Format TT.MM.JJJJ erfasst sein (also z. B. 19.04.1979)."))
  } else if (any(!tolower(data$geschlecht) %in% c("männlich", "mann", "m", "weiblich", "w", "frau", "f"))) {
    stop(paste0(base_msg, "Das Geschlecht aller Kandidierenden muss gemäss den Informationen aus dem Einwohnerregister als \"m\" oder \"w\" erfasst sein."))
  } else if (any(!tolower(data$bisher) %in% c("ja", "nein", NA))) {
    stop(paste0(base_msg, "Die Spalte bisher darf nur die Werte \"Ja\" oder \"Nein\" enthalten oder leer sein."))
  } else if (any(nchar(data$beruf, keepNA = FALSE) > 250)) {
    stop(paste0(base_msg, "Die Berufsbezeichnung darf maximal 250 Zeichen enthalten."))
  } else if (any(nchar(data$titel, keepNA = FALSE) > 250)) {
    stop(paste0(base_msg, "Der Titel darf maximal 250 Zeichen enthalten."))
  } else if (any(nchar(data$strasse, keepNA = FALSE) > 150)) {
    stop(paste0(base_msg, "Die Strasse darf maximal 150 Zeichen enthalten."))
  } else if (any(nchar(data$hausnummer, keepNA = FALSE) > 30)) {
    stop(paste0(base_msg, "Die Hausnummer darf maximal 30 Zeichen enthalten."))
  } else if (any(!nchar(data$plz, keepNA = FALSE) %in% c(2, 4))) {
    stop(paste0(base_msg, "Die Postleitzahl muss genau 4 Ziffern lang sein."))
  } else if (any(nchar(data$ort, keepNA = FALSE) > 40)) {
    stop(paste0(base_msg, "Der Wohnort darf maximal 40 Zeichen enthalten."))
  } else if (any(nchar(data$kand_nummer, keepNA = FALSE) > 10)) {
    stop(paste0(base_msg, "Die Kandidierendennummer darf maximal 10 Zeichen enthalten."))
  } else if (any(nchar(data$parteikurzbezeichnung, keepNA = FALSE) > 12)) {
    stop(paste0(base_msg, "Die Parteikurzbezeichnung darf maximal 12 Zeichen enthalten."))

    # Majority specific checks
  } else if (election_type == "majority") {

    if (!all(sort(names(data)) == sort(names(readRDS("inst/templates/eCH-0157_majority_table_template.RDS"))))) {
      stop(paste0(base_msg, "Es sind nicht alle nötigen Spalten aus dem Template vorhanden."))
    } else if (any(nchar(data$parteilangbezeichnung, keepNA = FALSE)  > 100)) {
      stop(paste0(base_msg, "Die Parteibezeichnung darf maximal 100 Zeichen enthalten."))
    }

    # Proportional specific checks
  } else if (election_type == "proportion") {

    if (!all(sort(names(data)) == sort(names(readRDS("inst/templates/eCH-0157_proportion_table_template.RDS"))))) {
      stop(paste0(base_msg, "Es sind nicht alle nötigen Spalten aus dem Template vorhanden."))
    } else if (any(!grepl("^[0-9]+$", data$listenposition))) {
      stop(paste0(base_msg, "Die Listenposition muss eine Zahl sein. Sie bezeichnet die genaue Position von Kandidierenden auf der Liste."))
    } else if (any(nchar(data$listennummer, keepNA = FALSE) > 12)) {
      stop(paste0(base_msg, "Die Listennummer darf maximal 12 Zeichen enthalten."))
    } else if (any(nchar(data$listenkurzbezeichnung, keepNA = FALSE) > 20)) {
      stop(paste0(base_msg, "Die Listenkurzbezeichnung darf maximal 20 Zeichen enthalten."))
    } else if (any(nchar(data$listenlangbezeichnung, keepNA = FALSE) > 100)) {
      stop(paste0(base_msg, "Die Listenbezeichnung darf maximal 100 Zeichen enthalten."))
    } else if (any(!grepl("^[0-9]+$", data$leere_zeilen))) {
      stop(paste0(base_msg, "Die Anzahl leere Zeilen muss eine Zahl sein."))
    }

  }


  # Add information from params
  data <- data |>
    dplyr::mutate(
      contest_contestDate = date,
      `electionDescriptionInfo-de_electionDescriptionShort` = election_title_short,
      `electionDescriptionInfo-de_electionDescription` = election_title_long,
      election_numberOfMandates = mandates
    )


  # Rename columns
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
    )))


  # Add columns
  data <- data |>
    dplyr::mutate(
      # Add necessary columns with boiler plate content that will not be used by the receiving system but is necessary for valid eCH
      contest_contestIdentification = paste0("contest-", contest_contestDate),
      `contestDescriptionInfo-de_contestDescription` = paste0("urnengang_vom_", contest_contestDate),
      electionGroupBallot_domainOfInfluenceIdentification = 1,
      election_electionIdentification = paste0("wahl_vom_", contest_contestDate),
      candidate_candidateIdentification = stringr::str_trunc(paste0(contest_contestDate, kand_nummer, candidate_familyName, candidate_firstName), 36, "right", ""),
      # `candidateTextInfo-de_candidateText` = "candidateText", # not needed
      swiss_origin = "-", # not needed

      # Mutate columns with information to be used by receiving system
      election_typeOfElection = ifelse(election_type == "proportion", 1, 2),
      election_electionPosition = 0,
      electionGroupBallot_index = 1,
      candidate_firstName = ifelse(is.na(candidate_firstName), candidate_callName, candidate_firstName),
      candidate_sex = ifelse(tolower(candidate_sex) %in% c("m", "männlich", "mann", "herr"), 1, 2),
      candidate_incumbentYesNo = ifelse(tolower(candidate_incumbentYesNo) %in% c("yes", "ja", "bisher", "true"), "true", "false"),
      country_countryId = 8100,
      country_countryIdISO2 = "CH",
      country_countryNameShort = "Schweiz",
      candidate_mrMrs = ifelse(candidate_sex == 1, 2, 1), # eCH-0010 and 0044 have different numerics for male/mr. -.-
      candidate_languageOfCorrespondence = "de"
    )


  # Majority specific adjustments
  if (election_type == "majority") {

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
  if (election_type == "proportion") {

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
      dplyr::mutate(n_kand = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        `partyAffiliationInfo-de_partyAffiliationLong` = `partyAffiliationInfo-de_partyAffiliationShort`,
        list_listIndentureNumber = stringr::str_pad(as.numeric(list_listIndentureNumber), 2, "left", "0"),
        # candidate number: padded list number, padded last two positions on the cand number given
        candidatePosition_candidateReferenceOnPosition = paste0(
          list_listIndentureNumber,
          ".",
          stringr::str_pad(stringr::str_trunc(candidatePosition_candidateReferenceOnPosition, 2, "left", ellipsis = ""), 2, "left", "0")
        ),
        candidate_candidateReference = candidatePosition_candidateReferenceOnPosition,
        list_isEmptyList = "false",
        list_listOrderOfPrecedence = as.numeric(list_listIndentureNumber),
        candidatePosition_candidateIdentification = candidatePosition_candidateReferenceOnPosition,
        list_listIdentification = paste0("list_id", list_listIndentureNumber, "-", stringr::str_remove_all(`listDescriptionInfo-de_listDescription`, " "))
      ) |>
      dplyr::group_by(
        `listDescriptionInfo-de_listDescriptionShort`,
        `listDescriptionInfo-de_listDescription`,
        list_listIndentureNumber
      ) |>
      dplyr::mutate(
        list_totalPositionsOnList = max(as.numeric(candidatePosition_positionOnList)),
        list_emptyListPositions = mandates - list_totalPositionsOnList,
      ) |>
      dplyr::ungroup()

  }


  return(data)

}
