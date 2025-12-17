#' Load and process iles_nunavik.gdb dataset
#'
#' This function loads and processes the "iles_nunavik.gdb" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_iles_nunavik <- load_iles_nunavik()
#' }
#' @export
load_iles_nunavik <- function() {
  
  cli::cli_h2("Iles Nunavik")
  cli::cli_alert_info("Starting integration procedure on {external_files()$iles_nunavik$path}")

  # Assert file exists
  if (!file.exists(external_files()$iles_nunavik$path)) {
    cli::cli_abort("Could not find file: {external_files()$iles_nunavik$path}")
  }

  iles_nunavik <- data.table::fread(external_files()$iles_nunavik$path, dec = ",", sep = ";") |> tibble::as_tibble()

  # Assert columns exist
  missing_cols <- setdiff(external_files()$iles_nunavik$check_columns, names(iles_nunavik))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in dataset:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  cli::cli_alert_info("Applying transformation on {nrow(iles_nunavik)} rows")
  iles_nunavik <- iles_nunavik |>
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
      source = "iles_nunavik", 
      Nom_francais = tolower(Nom_francais),
      inv_type = Methode_descriptif,  # Quel type d'inventaire
      locality = Nom_Ile,
      sampling_id=as.character(year)
    ) 
  
  # Join TAXO - Match CODE_ID using Nom_francais
  iles_nunavik <- iles_nunavik |>
    dplyr::mutate(
      code_fr = stringi::stri_trans_general(Nom_francais, "latin-ascii")
    )
  
  iles_nunavik <- iles_nunavik |>
    dplyr::left_join(
      get("taxonomy"),
      by = "code_fr",
      na_matches = "never"
    )
  
  # Re-order cols
  iles_nunavik <- dplyr::select(iles_nunavik, dplyr::all_of(final_cols))
  
  cli::cli_alert_success("Returning {nrow(iles_nunavik)} rows")
  
  return(iles_nunavik)
}
