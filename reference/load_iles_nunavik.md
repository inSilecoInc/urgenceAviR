# Load and process iles_nunavik.gdb dataset

This function loads and processes the "iles_nunavik.gdb" dataset from a
predefined external source. It validates the file and columns, applies
transformations, and integrates species codes using a reference table.

## Usage

``` r
load_iles_nunavik()
```

## Value

A processed `data.frame` with standardized columns and integrated
species codes.

## Examples

``` r
if (FALSE) { # \dontrun{
processed_iles_nunavik <- load_iles_nunavik()
} # }
```
