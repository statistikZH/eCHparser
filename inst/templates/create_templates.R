# INFORMATION ==================================================================


# This script parses can be used to parse through stored data and create templates for exporting.
# It depends on up to date files in test/testthat/testdata/files_unparsed.


# SETUP ========================================================================


# Define path to testfiles
path_xml_0157_raw <- "tests/testthat/testdata/files_unparsed/eCH-0157/"

# Define path to destination
path_destination_raw <- "inst/templates/"

# Define the two original files
xml_0157_maj <- paste0(path_xml_0157_raw, "eCH-0157_v4_0_abraxas_election_ZH_majority_2030-01-01.xml")
xml_0157_prop <- paste0(path_xml_0157_raw, "eCH-0157_v4_0_abraxas_election_ZH_proportion_2030-01-01.xml")

# Define destinations
path_table_template_maj <- paste0(path_destination_raw, "eCH-0157_majority_table_template.RDS")
path_table_template_prop <- paste0(path_destination_raw, "eCH-0157_proportion_table_template.RDS")
path_ns_template_maj <- paste0(path_destination_raw, "eCH-0157_majority_namespaces.RDS")
path_ns_template_prop <- paste0(path_destination_raw, "eCH-0157_proportion_namespaces.RDS")


# TABLE TEMPLATES ==============================================================


## Build Majority Template ---------------------------------------------------


# Prepare template
table_template_maj_raw <- parse_eCH_0157(xml_0157_maj)

table_template_maj <- table_template_maj_raw[0,] |>
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
saveRDS(table_template_maj, path_table_template_maj)


## Build Proportion Template ---------------------------------------------------


# Prepare template
table_template_prop_raw <- parse_eCH_0157(xml_0157_prop)

table_template_prop <- table_template_prop_raw[0,] |>
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
saveRDS(table_template_prop, path_table_template_prop)


# NAMESPACE RDS FILES ==========================================================


## eCH-0157 --------------------------------------------------------------------


### Majority -------------------------------------------------------------------


ns_0157_maj_raw <- xml2::read_xml(xml_0157_maj) |>
  xml2::as_list()

ns_0157_maj_raw <- ns_0157_maj_raw[[1]] |>
  extract_attributes()

ns_0157_maj <- ns_0157_maj_raw[sapply(ns_0157_maj_raw, function(x) !any(sapply(x, is.null)))]

# Save template
saveRDS(ns_0157_maj, path_ns_template_maj)


### Proportion -----------------------------------------------------------------


ns_0157_prop_raw <- xml2::read_xml(xml_0157_prop) |>
  xml2::as_list()

ns_0157_prop_raw <- ns_0157_prop_raw[[1]] |>
  extract_attributes()

ns_0157_prop <- ns_0157_prop_raw[sapply(ns_0157_prop_raw, function(x) !any(sapply(x, is.null)))]

# Save template
saveRDS(ns_0157_prop, path_ns_template_prop)


# HEADER TEMPLATES =============================================================


## eCH-0157 --------------------------------------------------------------------


