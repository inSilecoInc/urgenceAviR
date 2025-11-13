# Get species rank

Get species rank based on scientific name

## Usage

``` r
get_species_rank(scientific_name)
```

## Arguments

- scientific_name:

  The scientific name of the species to be ranked

## Value

A character string indicating the rank: "genre", "subspecies", or
"species".

## Examples

``` r
get_species_rank("Aquila chrysaetos")
#> [1] "species"
```
