# Load and process the Eider Hiver dataset

This function loads and processes the "Eider Hiver" dataset from a
predefined external source. It validates the file and columns, applies
transformations, and integrates species codes using a reference table.

## Usage

``` r
load_eider_hiver()
```

## Value

A processed `data.frame` with standardized columns and integrated
species codes.

## Examples

``` r
if (FALSE) { # \dontrun{
processed_eider <- load_eider_hiver()
} # }
```
