# INFORMATION ==================================================================


# This script parses can be used to parse through stored data and create templates for exporting.
# It


# PARSE ALL TEST FILES AND SAVE THEM ===========================================


if(svDialogs::dlg_input("Are the files in tests/testthat/testdata/files_unparsed/eCH-0157/ are up to date?")$res == "Yes") {

  # Define path to testfiles
  path_raw <- "tests/testthat/testdata/files_unparsed/eCH-0157/"

  # Define the two original files
  original_file_maj <- paste0(path_raw, "eCH-0157_v4-0_abraxas_elections_ZH_majority_2026-06-16.xml")
  original_file_prop <- paste0(path_raw, "eCH-0157_v4-0_abraxas_elections_ZH_proportion_2026-06-16.xml")

  # Prepare Majority Template
  template_raw <- parse_eCH_0157(original_file_maj)

  template <- template_raw[0,] |>
    dplyr::select(
      # contest_contestIdentification,
      # contest_contestDate,
      # contestDescriptionInfo-de_contestDescription,
      # electionGroupBallot_domainOfInfluenceIdentification,
      # electionGroupBallot_index,
      # election_electionIdentification,
      # election_typeOfElection,
      # election_electionPosition,
      # `referencedElection-1_referencedElection`,
      # `referencedElection-2_referencedElection`,
      # candidate_candidateIdentification,
      datum = contest_contestDate,
      wahlkurzbezeichnung = `electionDescriptionInfo-de_electionDescriptionShort`,
      wahllangbezeichnung = `electionDescriptionInfo-de_electionDescription`,
      mandate = election_numberOfMandates,
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
      parteikurzbezeichnung = `partyAffiliationInfo-de_partyAffiliationShort`,
      parteilangbezeichnung = `partyAffiliationInfo-de_partyAffiliationLong`
    )



    # saveRDS(template, rds_filepath)

  }

} else {

  svDialogs::dlg_message("The templates were not updated.")

}
