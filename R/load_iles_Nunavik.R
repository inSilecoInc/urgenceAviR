#' Load and process Iles_Nunavik.gdb dataset
#'
#' This function loads and processes the "Iles_Nunavik.gdb" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_Iles_Nunavik <- load_Iles_Nunavik()
#' }
#' @export
load_Iles_Nunavik <- function() {
  
  cli::cli_h2("Iles_Nunavik")
  cli::cli_alert_info("Starting integration procedure on {external_files$Iles_Nunavik$path}")
  
  # Assert file exists
  if (!file.exists(external_files$Iles_Nunavik$path)) {
    cli::cli_abort("Could not find file: {external_files$Iles_Nunavik$path}")
  }
  
<<<<<<< HEAD
  Iles_Nunavik <- read.csv2(external_files$Iles_Nunavik$path) |> tibble::as_tibble()
=======
  Iles_Nunavik <- data.table::fread(external_files$Iles_Nunavik$path, dec = ",", sep = ";") |> tibble::as_tibble()
>>>>>>> 1e14150 (Ajout de data set iles_nunavik et aérien_nunavik et nouveau equivalances code espèce)
  
  # Assert columns exist
  missing_cols <- setdiff(external_files$Iles_Nunavik$check_columns, names(Iles_Nunavik))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in dataset:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  cli::cli_alert_info("Applying transformation on {nrow(Iles_Nunavik)} rows")
  Iles_Nunavik <- Iles_Nunavik |>
    dplyr::rename(
      longitude = Longitude,
      latitude = Latitude,
      abondance = Nb_compte,
      year = Annee,
      month = Mois,
      day = Jour,
      obs=Unite
    ) |>
    dplyr::mutate(
      date = lubridate::make_date(year, month, day),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      source = "Iles_Nunavik", 
      Nom_francais = tolower(Nom_francais),
      inv_type = Methode_descriptif,  # Quel type d'inventaire
      locality = Nom_Ile,
      sampling_id=as.character(year)
    ) 
  
  # Join TAXO - Match CODE_ID using Nom_francais
<<<<<<< HEAD
  Iles_Nunavik$code_id<-NA
=======
>>>>>>> 1e14150 (Ajout de data set iles_nunavik et aérien_nunavik et nouveau equivalances code espèce)
  Iles_Nunavik <- Iles_Nunavik |>
    dplyr::mutate(
      code_id = ifelse(!is.na(taxo$Species_ID[match(Iles_Nunavik$Nom_scientifique,
                                                    taxo$Scientific_Name)]),
                       taxo$Species_ID[match(Iles_Nunavik$Nom_scientifique,
                                             taxo$Scientific_Name)],ifelse(
                                               Iles_Nunavik$Nom_scientifique %in% names(equivalences),
                                               equivalences[Iles_Nunavik$Nom_scientifique],
                                               code_id
                                             ))
      
    )
  
  
  # Re-order cols
  Iles_Nunavik <- dplyr::select(Iles_Nunavik, dplyr::all_of(final_cols))
  
  cli::cli_alert_success("Returning {nrow(Iles_Nunavik)} rows")
  
  return(Iles_Nunavik)
}
