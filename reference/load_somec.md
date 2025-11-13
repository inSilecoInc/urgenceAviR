# Load and process the SOMEC dataset

This function loads and processes the "SOMEC" dataset from a predefined
external source. It validates the file and columns, applies
transformations, and integrates species codes using a reference table.

## Usage

``` r
load_somec()
```

## Value

A processed `data.frame` with standardized columns and integrated
species codes.

## Examples

``` r
if (FALSE) { # \dontrun{
processed_somec <- load_somec()
} # }
```
