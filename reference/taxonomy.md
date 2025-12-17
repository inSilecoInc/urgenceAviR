# Taxonomy Reference Table

A reference table combining avian core data (French/Latin names) and
taxonomy vulnerability data (milieu_marin, groupe_fonctionnel). This
dataset is used internally by all load\_\* functions to enrich bird
observation data with taxonomic information.

## Usage

``` r
data(taxonomy)
```

## Format

A data frame with 2474 rows and 6 variables:

- code_id:

  Species ID code (character)

- nom_francais:

  French common name (character)

- nom_latin:

  Scientific/Latin name (character)

- milieu_marin:

  Marine habitat indicator (character)

- groupe_fonctionnel:

  Functional group classification (character)

- code_fr:

  Normalized French name code for matching (character)

## Source

Environment and Climate Change Canada (ECCC) Avian Core Database

## Details

This dataset is automatically loaded when the package is loaded. It is
generated from two source files:

- ECCC_Avian_Core_20241025.csv - Contains species IDs, French and Latin
  names

- taxo_vulnerabilite.csv - Contains marine habitat and functional group
  data

The dataset is built using the `data-raw/build_taxonomy.R` script, which
joins and processes the source data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Access the taxonomy dataset
head(taxonomy)

# Filter by marine habitat
marine_birds <- taxonomy[taxonomy$milieu_marin == "Oui", ]

# Find a specific species
taxonomy[taxonomy$code_id == "MALL", ]
} # }
```
