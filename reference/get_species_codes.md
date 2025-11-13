# Clean and Process Species Codes

Functions to clean and process species codes from a DBF file, join them
with metadata, and optionally drop subspecies.

## Usage

``` r
get_species_codes(
  species_path = external_files$species_codes$path,
  metadata_path = external_files$species_metadata$path,
  drop_subspecies = TRUE
)
```

## Arguments

- species_path:

  Path to the DBF file containing species codes.

- metadata_path:

  Path to the CSV file containing metadata.

- drop_subspecies:

  Logical, whether to drop subspecies from the resulting data frame
  (default: TRUE).

## Value

A cleaned data frame of species codes with optional subspecies filtered
out.

## Details

Clean and Process Species Codes

This file contains functions to clean and process species codes from a
DBF file, join them with metadata, and optionally drop subspecies from
the output.

## Examples

``` r
if (FALSE) { # \dontrun{
species_ref <- get_species_codes(
 species_path = "data/CodesEspeces.dbf", 
 metadata_path = "data/metadata_species.csv", 
 drop_subspecies = TRUE
)
} # }
```
