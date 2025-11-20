#' Load and process Inventaire_aerien_Nunavik.gdb dataset
#'
#' This function loads and processes the "Inventaire_aerien_Nunavik.gdb" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_macreuse <- load_macreuse()
#' }
#' @export
load_Inventaire_aerien_Nunavik <- function() {
  
  cli::cli_h2("Inventaire_aerien_Nunavik")
  cli::cli_alert_info("Starting integration procedure on {external_files$Inventaire_aerien_Nunavik$path}")
  
  # Assert file exists
  if (!file.exists(external_files$Inventaire_aerien_Nunavik$path)) {
    cli::cli_abort("Could not find file: {external_files$Inventaire_aerien_Nunavik$path}")
  }
  
<<<<<<< HEAD
  Inventaire_aerien_Nunavik <- read.csv2(external_files$Inventaire_aerien_Nunavik$path) |> tibble::as_tibble()
=======
  Inventaire_aerien_Nunavik <- data.table::fread(external_files$Inventaire_aerien_Nunavik$path, dec = ",", sep = ";") |> tibble::as_tibble()
>>>>>>> 1e14150 (Ajout de data set iles_nunavik et aérien_nunavik et nouveau equivalances code espèce)
  
  # Assert columns exist
  missing_cols <- setdiff(external_files$Inventaire_aerien_Nunavik$check_columns, names(Inventaire_aerien_Nunavik))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in dataset:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  cli::cli_alert_info("Applying transformation on {nrow(Inventaire_aerien_Nunavik)} rows")
  
  Inventaire_aerien_Nunavik <- Inventaire_aerien_Nunavik |>
    dplyr::rename(
      longitude = Longitude,
      latitude = Latitude,
      abondance = Nb_total_ind,
      date = Date,
      obs=Observateur
    ) |>
    dplyr::mutate(
      date = lubridate::ymd(date),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      source = "Inventaire_aerien_Nunavik", 
      inv_type = "helico",  # Quel type d'inventaire
      locality = NA,
      sampling_id = as.character(date)
      
    ) 
  
  # Join TAXO - Match CODE_ID using Nom_francais
<<<<<<< HEAD
  Inventaire_aerien_Nunavik$code_id<-taxo$Species_ID[match(Inventaire_aerien_Nunavik$Nom_anglais,
                                                           taxo$English_Name)]
=======
  Inventaire_aerien_Nunavik$code_id <- taxo$Species_ID[match(Inventaire_aerien_Nunavik$Nom_anglais, taxo$English_Name)]
  
>>>>>>> 1e14150 (Ajout de data set iles_nunavik et aérien_nunavik et nouveau equivalances code espèce)
  Inventaire_aerien_Nunavik <- Inventaire_aerien_Nunavik |>
    dplyr::mutate(
      code_id = ifelse(
        Espece_code %in% names(equivalences),
        equivalences[Espece_code],
        code_id
      )
    ) 
  
<<<<<<< HEAD
  
=======
>>>>>>> 1e14150 (Ajout de data set iles_nunavik et aérien_nunavik et nouveau equivalances code espèce)
  # Re-order cols
  Inventaire_aerien_Nunavik <- dplyr::select(Inventaire_aerien_Nunavik, dplyr::all_of(final_cols))
  
  cli::cli_alert_success("Returning {nrow(Inventaire_aerien_Nunavik)} rows")
  
  return(Inventaire_aerien_Nunavik)
}
