# Load and process all datasets

This function sequentially calls all the `load_*` dataset processing
functions and combines their outputs into a single `data.frame` or a
list of `data.frames`.

## Usage

``` r
load_all_datasets(combine = TRUE)
```

## Arguments

- combine:

  Logical, if TRUE (default), combines all datasets into a single
  `data.frame`. If FALSE, returns a named list of individual
  `data.frames`.

## Value

A combined `data.frame` or a list of `data.frames` containing all
processed datasets.

## Examples

``` r
if (FALSE) { # \dontrun{
all_data <- load_all_datasets()
} # }
```
