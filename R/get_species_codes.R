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
#' \dontrun{
#' species_ref <- get_species_codes(
#'  species_path = "data/CodesEspeces.dbf", 
#'  metadata_path = "data/metadata_species.csv", 
#'  drop_subspecies = TRUE
#' )
#' }
#' @export
get_species_codes <- function(species_path = external_files$species_codes$path, metadata_path = external_files$species_metadata$path, drop_subspecies = TRUE) {

  # assert file exists
  if (!file.exists(species_path)) {
    cli::cli_abort("Could not find file { species_path }")
  }
  
  if(!file.exists(metadata_path)) {
      cli::cli_abort("Could not find file { metadata_path }")
  }

  # Load species codes
  species_code <- foreign::read.dbf(species_path, as.is = TRUE) |> tibble::as_tibble()

  # Filter species for birds and prepare columns
  species_code <- species_code |>
    dplyr::filter(Groupe_FR == "Oiseaux") |>  # Only keep birds
    dplyr::select(Nom_FR, Nom_Scient, Name_EN, Code4_EN, Code4_FR, Alpha_Code, SousGroupe, STATUT_COS) |>
    dplyr::filter(!is.na(SousGroupe)) |>
    dplyr::distinct()

  # Read metadata and select columns
  metadata_sp <- read.csv(metadata_path, encoding = "UTF-8") |>
    dplyr::select(Name_SC, Species_ID, group = Taxo_EN)

  # Add species ID to species codes
  species_code <- dplyr::left_join(
    species_code, metadata_sp,
    by = c("Nom_Scient" = "Name_SC"),
    na_matches = "never"
  )

  # Create reference table
  species_ref <- species_code |>
    dplyr::mutate(
      # Create CODE_ID column
      CODE_ID = dplyr::case_when(
        !is.na(Code4_EN) ~ Code4_EN,
        !is.na(Species_ID) ~ Species_ID,
        TRUE ~ NA_character_ # Explicitly handle NA cases
      ),
      # Rank species
      rank = sapply(Nom_Scient, get_species_rank),
      Nom_FR = stringr::str_replace(
        stringi::stri_trans_general(Nom_FR, "latin-ascii"),
        "non identifie|non identife|sp\\.e", "sp."
      ) |> tolower()
    ) |>
    dplyr::select(CODE_ID, Nom_Scient, STATUT_COS, Nom_FR, Name_EN, Code4_FR, Code4_EN, Alpha_Code, rank, group) |>
    janitor::clean_names()

  # Optionally drop subspecies
  if (isTRUE(drop_subspecies)) {
    species_ref <- species_ref |> dplyr::filter(rank != "subspecies")
  }

  cli::cli_alert_info("Load { nrow(species_ref) } species from species reference table")

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
