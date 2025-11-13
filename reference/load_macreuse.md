# Load and process the Macreuse dataset

This function loads and processes the "Macreuse" dataset from a
predefined external source. It validates the file and columns, applies
transformations, and integrates species codes using a reference table.

## Usage

``` r
load_macreuse()
```

## Value

A processed `data.frame` with standardized columns and integrated
species codes.

## Examples

``` r
if (FALSE) { # \dontrun{
processed_macreuse <- load_macreuse()
} # }
```
