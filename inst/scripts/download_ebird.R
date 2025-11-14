#!/usr/bin/env Rscript

#' Download eBird observations using auk package for a specific spatial area
#' 
#' This script demonstrates how to use the auk package to download eBird 
#' observations for a specific spatial area, following patterns from 
#' urgenceAviR::get_ebird() and assessThreatv2.R
#' 
#' Author: Generated for urgenceAviR package
#' Date: 2025-09-22

library(auk)
library(sf)
library(dplyr)
library(urgenceAviR)
library(rebird)  # For eBird API access

#' Setup for different data sources
#' ================================

# Option 1: Using eBird API (for smaller datasets, recent data)
# Get your API key from: https://ebird.org/api/keygen
# Sys.setenv(EBIRD_KEY = "your_api_key_here")

# Option 2: Using eBird Basic Dataset (for large datasets, historical data)
# Download from: https://ebird.org/data/download
ebd_path <- "path/to/ebd_relXXX.txt"  # Replace with actual path
# auk_set_ebd_path(ebd_path)

#' Example 1: API data for specific species in polygon area
#' ========================================================

# Define a polygon area (Île d'Orléans example)
ile_orleans_coords <- data.frame(
  lon = c(-70.9, -70.5, -70.5, -70.9, -70.9),
  lat = c(46.85, 46.85, 47.0, 47.0, 46.85)
)

study_area <- ile_orleans_coords |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  summarise() |>
  st_cast("POLYGON")

# Get center point of polygon for API query
area_center <- st_centroid(study_area)
area_coords <- st_coordinates(area_center)

# Target species (using similar list as assessThreatv2.R)
target_species_codes <- c("snogoo", "cangoo", "ambduc", "mallar", "rinduc")

# Download data for each species
species_data <- list()
for (species_code in target_species_codes) {
  cat("Downloading data for species:", species_code, "\n")
  
  species_obs <- ebirdgeo(
    lat = area_coords[2],
    lng = area_coords[1],
    species = species_code,
    dist = 25,  # 25km radius
    back = 365, # Last year
    max = 500
  )
  
  if (nrow(species_obs) > 0) {
    species_data[[species_code]] <- species_obs
  }
}

# Combine all species data
if (length(species_data) > 0) {
  combined_api_data <- do.call(rbind, species_data)
  
  # Convert to sf and filter to exact polygon
  combined_sf <- combined_api_data |>
    filter(!is.na(lat), !is.na(lng)) |>
    st_as_sf(coords = c("lng", "lat"), crs = 4326) |>
    st_filter(study_area)
  
  cat("Found", nrow(combined_sf), "API observations within study polygon\n")
}

#' Example 2: Using EBD file (for large historical datasets)
#' =========================================================

auk_set_ebd_path(ebd_path)

# Define spatial extent (polygon for EBD filtering)
study_area_ebd <- ile_orleans_coords |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  summarise() |>
  st_cast("POLYGON")

# Get bounding box for auk filtering
bbox_ebd <- st_bbox(study_area_ebd)

# Define species and temporal filters
target_species_ebd <- c("Snow Goose", "Canada Goose", "American Black Duck")
years_ebd <- 2020:2023

# Create auk EBD object and apply filters
ebd_filters <- auk_ebd(ebd_path) |>
  auk_species(target_species_ebd) |>
  auk_bbox(bbox = c(bbox_ebd[["xmin"]], bbox_ebd[["ymin"]], 
                    bbox_ebd[["xmax"]], bbox_ebd[["ymax"]])) |>
  auk_date(date = c(paste0(min(years_ebd), "-01-01"),
                    paste0(max(years_ebd), "-12-31"))) |>
  auk_complete()

# Define output file
output_file_ebd <- "inst/scripts/ebird_ebd_data.txt"

# Filter the dataset (this creates a filtered text file)
auk_filter(ebd_filters, file = output_file_ebd, overwrite = TRUE)

# Read and process the filtered data
ebird_ebd_data <- read_ebd(output_file_ebd)
ebird_ebd_sf <- ebird_ebd_data |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_filter(study_area_ebd)  # Keep only points within exact polygon

cat("EBD method: Found", nrow(ebird_ebd_sf), "observations within study area\n")


