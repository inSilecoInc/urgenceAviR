# Load and process the Sauvagine Fleuve dataset

This function loads and processes the "Sauvagine Fleuve" dataset from a
predefined external source. It validates the file and columns, applies
transformations, and integrates species codes using a reference table.

## Usage

``` r
load_sauvagine_fleuve()
```

## Value

A processed `data.frame` with standardized columns and integrated
species codes.

## Examples

``` r
if (FALSE) { # \dontrun{
processed_sauvagine_fleuve <- load_sauvagine_fleuve()
} # }
```
