# Load and process the Oies dataset

This function loads and processes the "Oies" dataset from a predefined
external source. It validates the file and columns, applies
transformations, and integrates species codes using a reference table.

## Usage

``` r
load_oies()
```

## Value

A processed `data.frame` with standardized columns and integrated
species codes.

## Examples

``` r
if (FALSE) { # \dontrun{
processed_oies <- load_oies()
} # }
```
