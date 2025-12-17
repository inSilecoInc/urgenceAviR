#' Load and process inventaire_aerien_nunavik.csv dataset
#'
#' This function loads and processes the "inventaire_aerien_nunavik.gdb" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_macreuse <- load_macreuse()
#' }
#' @export
load_inventaire_aerien_nunavik <- function() {
  
  cli::cli_h2("inventaire_aerien_nunavik")
  cli::cli_alert_info("Starting integration procedure on {external_files()$inventaire_aerien_nunavik$path}")

  # Assert file exists
  if (!file.exists(external_files()$inventaire_aerien_nunavik$path)) {
    cli::cli_abort("Could not find file: {external_files()$inventaire_aerien_nunavik$path}")
  }

  inventaire_aerien_nunavik <- data.table::fread(external_files()$inventaire_aerien_nunavik$path, dec = ",", sep = ";") |> tibble::as_tibble()

  # Assert columns exist
  missing_cols <- setdiff(external_files()$inventaire_aerien_nunavik$check_columns, names(inventaire_aerien_nunavik))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in dataset:",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  cli::cli_alert_info("Applying transformation on {nrow(inventaire_aerien_nunavik)} rows")
  
  inventaire_aerien_nunavik <- inventaire_aerien_nunavik |>
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
      source = "inventaire_aerien_nunavik", 
      inv_type = "helico",  # Quel type d'inventaire
      locality = NA,
      sampling_id = as.character(date)
      
    ) 
  
  inventaire_aerien_nunavik <- inventaire_aerien_nunavik |>
    dplyr::mutate(
      code_id = Espece_code
    )|>
    dplyr::mutate(
        code_id = ifelse(
            code_id %in% names(get_equivalences()),
            get_equivalences()[code_id],
            code_id
        )
    ) |>
    dplyr::left_join(
      taxonomy,
      by = "code_id",
      na_matches = "never"
    )

  # Re-order cols
  inventaire_aerien_nunavik <- dplyr::select(inventaire_aerien_nunavik, dplyr::all_of(final_cols))
  
  cli::cli_alert_success("Returning {nrow(inventaire_aerien_nunavik)} rows")
  
  return(inventaire_aerien_nunavik)
}
