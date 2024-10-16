
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->

# eCHparser

<!-- badges: start -->
<!-- badges: end -->

Use the eCHparser package to convert XML files in various eCH standard
formats of the political rights domain to simple to use dataframes.

## Installation

You can install the development version of eCHparser from
[GitHub](https://github.com/statistikZH/eCHparser) with:

``` r
# install.packages("devtools")
devtools::install_github("statistikZH/eCHparser")
```

## Example: eCH-0252

The [eCH-0252 standard](https://www.ech.ch/de/ech/ech-0252/1.0.0) is
used to feed information from cantonal IT systems to the voteInfo API,
which then uses the data to display live counting information on votes,
for example via the voteInfo App or the [Results and Information
Plattform](https://app.statistik.zh.ch/wahlen_abstimmungen/prod/Actual)
of the Canton of Zurich. This data is not yet publicly available in this
form but will be starting in 2025. There are examples of the files
available in this package (see example bellow)

The function `parse_eCH_0252()` allows user to transform these XML files
to simple to use dataframes.

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
