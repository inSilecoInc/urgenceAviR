#' Clean and Process Species Codes
#'
#' This file contains functions to clean and process species codes from a DBF file,
#' join them with metadata, and optionally drop subspecies from the output.
#'
#' @title Clean and Process Species Codes
#' @description Functions to clean and process species codes from a DBF file, join them with metadata, and optionally drop subspecies.
#'
#' @param species_path Path to the DBF file containing species codes.
#' @param metadata_path Path to the CSV file containing metadata.
#' @param drop_subspecies Logical, whether to drop subspecies from the resulting data frame (default: TRUE).
#'
#' @return A cleaned data frame of species codes with optional subspecies filtered out.
#' @examples
#' species_ref <- get_species_codes("./data/CodesEspeces.dbf", "data/metadata_species.csv", drop_subspecies = TRUE)
#'
get_species_codes <- function(species_path, metadata_path, drop_subspecies = TRUE) {

  # Load species codes
  species_code <- foreign::read.dbf(species_path, as.is = TRUE)

  # Filter species for birds and prepare columns
  species_code <- species_code |>
    dplyr::filter(Groupe_FR == "Oiseaux") |>  # Only keep birds
    dplyr::select(Nom_FR, Nom_Scient, Name_EN, Code4_EN, Code4_FR, Alpha_Code, SousGroupe, STATUT_COS) |>
    tidyr::drop_na(SousGroupe) |>
    dplyr::distinct()

  # Read metadata and select columns
  metadata_sp <- read.csv(metadata_path, encoding = "UTF-8") |>
    dplyr::select(Name_SC, Species_ID) |>
    tidyr::drop_na()

  # Add species ID to species codes
  species_code <- dplyr::left_join(species_code, metadata_sp, by = c("Nom_Scient" = "Name_SC"))

  # Create reference table
  species_ref <- species_code |>
    dplyr::mutate(
      # Create CODE_ID column
      CODE_ID = case_when(
        !is.na(Code4_EN) ~ Code4_EN,
        !is.na(Species_ID) ~ Species_ID,
        TRUE ~ NA_character_  # Explicitly handle NA cases
      ),
      # Rank species
      category = sapply(Nom_Scient, get_species_rank),
      Nom_FR = stringr::str_replace(Nom_FR, "non identifié|non identifé|sp\\.e", "sp.")
    ) |> 
    dplyr::select(CODE_ID, Nom_Scient, STATUT_COS, Nom_FR, Code4_FR, Code4_EN, Alpha_Code, category)

  # Optionally drop subspecies
  if (isTRUE(drop_subspecies)) {
    species_ref <- species_ref |> dplyr::filter(category != "subspecies")
  }

  return(species_ref)
}

#' @title Get species rank
#' @description Get species rank based on scientific name
#'
#' @param scientific_name The scientific name of the species to be ranked
#'
#' @return A character string indicating the rank: "genre", "subspecies", or "species".
#' @examples
#' get_species_rank("Aquila chrysaetos")
#' 
#' @export
get_species_rank <- function(scientific_name) {
  if (grepl("sp\\.", scientific_name)) {
    return("genre")
  } else if (length(strsplit(scientific_name, "\\s+")[[1]]) == 3) {
    return("subspecies")
  } else {
    return("species")
  }
}
