
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->

# eCHparser

<!-- badges: start -->

[![tic](https://github.com/statistikZH/eCHparser/workflows/tic/badge.svg)](https://github.com/statistikZH/eCHparser/actions)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

Convert XML files in the standardised formats of eCH-0252 and eCH-0157
into easy-to-use dataframes using the eCHparser package. The package
also provides templates for election and candidate data, that can be
easily turned into XML files in the eCH-0157 format. These files can be
imported into the VOTING application.

## Installation

You can install the development version of eCHparser from
[GitHub](https://github.com/statistikZH/eCHparser) with:

``` r
# install.packages("devtools")
devtools::install_github("statistikZH/eCHparser")
```

## Example: eCH-0252

The [eCH-0252 standard](https://www.ech.ch/de/ech/ech-0252) is used to
feed voting data from cantonal IT systems to the voteInfo API, which
then uses the data to display (live) vote counting information, for
example via the voteInfo App or the [Results and Information
Plattform](https://app.statistik.zh.ch/wahlen_abstimmungen/prod/Actual)
of the Canton of Zurich.

The function `parse_eCH_0252()` allows users to transform these XML
files to easy-to-use dataframes.

``` r
library(eCHparser)

# list all sample files
example_files <- system.file("extdata", package = "eCHparser") |>
  list.files()

# pick a random sample file
file <- sample(seq_along(example_files), 1)

# define filepath to sample file
filepath <- system.file("extdata", testfiles[file], package = "eCHparser")

# parse file, using only federal and cantonal votes
vote_df <- parse_eCH_0252(filepath, doi = c("CH", "CT"))
```

## Example: eCH-0157

The [eCH-0157 standard](https://www.ech.ch/de/ech/ech-0157) is used to
import elections to and export elections from the IT systems used to
manage election results management systems (i. e. VOTING in the canton
of Zurich).

The function `parse_eCH_0157()` allows users to transform these XML
files to easy-to-use dataframes.

``` r
library(eCHparser)

# list all sample files that contain 0252
example_files <- system.file("extdata", package = "eCHparser") |>
  list.files() |> 
  grep(pattern = "0252", x = _, value = TRUE)

# select the first of the files
file <- example_files[1]

# define filepath to sample file
filepath <- system.file(paste0("extdata/", file), package = "eCHparser")

# parse file, using only federal and cantonal votes
vote_df <- parse_eCH_0252(filepath, doi = c("CH", "CT"))
```

The function `new_election_template()` generates an XLSX template into
which information about candidates can be entered. These templates can
then be transformed into valid XML files in the format eCH-0157 using
the function `read_election_template()` and either `build_eCH_0157()` to
create the object in your R environment or `write_eCH_0157()` to
directly write the XML file onto your disc. Alternatively, you can use
the wrapper function `convert_to_eCH_0157()` to directly convert the
XLSX file on your disk to the corresponding XML file.

``` r
library(eCHparser)

# list all XLSX sample files
example_files <- system.file("extdata", package = "eCHparser") |>
  list.files() |> 
  grep(pattern = "\\.xlsx$", x = _, value = TRUE)

# select the first of the files
file <- grep(pattern = "maj", example_files, value = TRUE)

# define filepath to sample file
filepath <- system.file(paste0("extdata/", file), package = "eCHparser")

# read the election template and add necessary data
election_df <- read_election_template(
  filepath,
  election_type = "Majority",
  date = "2030-01-01",
  election_title_short = "Test Election",
  election_title_long = "Test Election - Long Title",
  mandates = 5
)

# turn data into xml object, ready to be exported
election_xml <- build_eCH_0157(election_df, "Majority")
```
