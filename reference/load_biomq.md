# Load and process the BIOMQ dataset

This function loads and processes the "BIOMQ" dataset from a predefined
external Excel file. It validates the file and columns, applies
transformations, and integrates species codes using a reference table.

## Usage

``` r
load_biomq()
```

## Value

A processed `data.frame` with standardized columns and integrated
species codes.

## Examples

``` r
if (FALSE) { # \dontrun{
processed_biomq <- load_biomq()
} # }
```
