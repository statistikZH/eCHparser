# INFORMATION ==================================================================


# This script parses can be used to parse through stored data and create templates for exporting.
# It


# CREATE TEMPLATES FROM TEST FILES =============================================


if(tolower(svDialogs::dlg_input("Are the files in tests/testthat/testdata/files_unparsed/eCH-0157/ are up to date?")$res) == "yes") {


  ## Define Paths --------------------------------------------------------------


  # Define path to testfiles
  path_origin_raw <- "tests/testthat/testdata/files_unparsed/eCH-0157/"

  # Define path to destination
  path_destination_raw <- "inst/templates/"

  # Define the two original files
  original_file_maj <- paste0(path_origin_raw, "eCH-0157_v4_0_abraxas_election_ZH_majority_2030-01-01.xml")
  original_file_prop <- paste0(path_origin_raw, "eCH-0157_v4_0_abraxas_election_ZH_proportion_2030-01-01.xml")

  # Define destinations
  template_path_maj <- paste0(path_destination_raw, "eCH-0157_majority_table_template.RDS")
  template_path_prop <- paste0(path_destination_raw, "eCH-0157_proportion_table_template.RDS")


  ## Build Majority Template ---------------------------------------------------


  # Prepare template
  template_majority_raw <- parse_eCH_0157(original_file_maj)

  template_majority <- template_majority_raw[0,] |>
    dplyr::select(
      # contest_contestIdentification,
      # datum = contest_contestDate,
      # contestDescriptionInfo-de_contestDescription,
      # electionGroupBallot_domainOfInfluenceIdentification,
      # electionGroupBallot_index,
      # election_electionIdentification,
      # election_typeOfElection,
      # election_electionPosition,
      # `referencedElection-1_referencedElection`,
      # `referencedElection-2_referencedElection`,
      # candidate_candidateIdentification,
      # contest_contestDate,
      # wahlkurzbezeichnung = `electionDescriptionInfo-de_electionDescriptionShort`,
      # wahllangbezeichnung = `electionDescriptionInfo-de_electionDescription`,
      # mandate = election_numberOfMandates,
      nachname = candidate_familyName,
      amtl_vorname = candidate_firstName,
      pol_vorname = candidate_callName,
      geburtsdatum = candidate_dateOfBirth,
      geschlecht = candidate_sex,
      beruf = `occupationalTitleInfo-de_occupationalTitle`,
      titel = candidate_title,
      strasse = dwellingAddress_street,
      hausnummer = dwellingAddress_houseNumber,
      plz = dwellingAddress_swissZipCode,
      ort = dwellingAddress_town,
      kand_nummer = candidate_candidateReference,
      bisher = candidate_incumbentYesNo,
      # country_countryId,
      # country_countryIdISO2,
      # country_countryNameShort,
      # swiss_origin,
      # candidate_mrMrs,
      # candidate_languageOfCorrespondence,
      # candidate_candidateReference,
      parteikurzbezeichnung = `partyAffiliationInfo-de_partyAffiliationShort`,
      parteilangbezeichnung = `partyAffiliationInfo-de_partyAffiliationLong`
    )

  # Save template
  saveRDS(template_majority, template_path_maj)


  ## Build Proportion Template ---------------------------------------------------


  # Prepare template
  template_proportion_raw <- parse_eCH_0157(original_file_prop)

  template_proportion <- template_proportion_raw[0,] |>
    dplyr::select(
      # contest_contestIdentification,
      # datum = contest_contestDate,
      # `contestDescriptionInfo-de_contestDescription`,
      # electionGroupBallot_domainOfInfluenceIdentification,
      # electionGroupBallot_index,
      # election_electionIdentification,
      # election_typeOfElection,
      # election_electionPosition,
      # wahlkurzbezeichnung = `electionDescriptionInfo-de_electionDescriptionShort`,
      # wahllangbezeichnung = `electionDescriptionInfo-de_electionDescription`,
      # mandate = election_numberOfMandates,
      # candidate_candidateIdentification,
      nachname = candidate_familyName,
      amtl_vorname = candidate_firstName,
      pol_vorname = candidate_callName,
      geburtsdatum = candidate_dateOfBirth,
      geschlecht = candidate_sex,
      beruf = `occupationalTitleInfo-de_occupationalTitle`,
      titel = candidate_title,
      strasse = dwellingAddress_street,
      hausnummer = dwellingAddress_houseNumber,
      plz = dwellingAddress_swissZipCode,
      ort = dwellingAddress_town,
      # country_countryId,
      # country_countryIdISO2,
      # country_countryNameShort,
      # swiss_origin,
      # candidate_mrMrs,
      # candidate_languageOfCorrespondence,
      # candidate_candidateReference,
      listenposition = candidatePosition_positionOnList,
      kand_nummer = candidatePosition_candidateReferenceOnPosition,
      bisher = candidate_incumbentYesNo,
      parteikurzbezeichnung = `partyAffiliationInfo-de_partyAffiliationShort`,
      # parteilangbezeichnung = `partyAffiliationInfo-de_partyAffiliationLong`,
      # list_listIdentification,
      listennummer = list_listIndentureNumber,
      listenkurzbezeichnung = `listDescriptionInfo-de_listDescriptionShort`,
      listenlangbezeichnung = `listDescriptionInfo-de_listDescription`,
      # list_isEmptyList,
      # list_listOrderOfPrecedence,
      # list_totalPositionsOnList,
      # candidatePosition_candidateIdentification,
      leere_zeilen = list_emptyListPositions
    )

  # Save template
  saveRDS(template_proportion, template_path_prop)


  ## Feedback ------------------------------------------------------------------


  svDialogs::dlg_message(paste0("The templates at ", path_destination_raw, " were updated."))

} else {

  svDialogs::dlg_message("The templates were not updated.")

}
