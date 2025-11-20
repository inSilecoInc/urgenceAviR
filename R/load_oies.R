#' Load and process the Oies dataset
#'
#' This function loads and processes the "Oies" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_oies <- load_oies()
#' }
#' @export
load_oies <- function() {

    cli::cli_h2("Oies")
    cli::cli_alert_info("Starting integration procedure on {external_files()$oies$path}")

    # Assert file exists
    if (!file.exists(external_files()$oies$path)) {
        cli::cli_abort("Could not find file: {external_files()$oies$path}")
    }

    oies <- data.table::fread(external_files()$oies$path, dec = ",", sep = ";") |> tibble::as_tibble()

    # Assert columns exist
    missing_cols <- setdiff(external_files()$oies$check_columns, names(oies))
    if (length(missing_cols) > 0) {
        cli::cli_abort(c(
            "Missing required columns in dataset:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    cli::cli_alert_info("Applying transformation on {nrow(oies)} rows")

    oies <- oies |>
        dplyr::rename(
            longitude = Longitude,
            latitude = Latitude,
            abondance = Count,
            date = Date,
            obs = Observateur
        ) |>
        dplyr::mutate(
            date = lubridate::ymd(date),
            latitude = as.numeric(latitude),
            longitude = as.numeric(longitude),
            inv_type = "aeronef",  
<<<<<<< HEAD
            inv_type = NA,  # Est-ce qu'il y a un type d'inventaire ?
            link = external_files()$oies$path,
=======
>>>>>>> 1e14150 (Ajout de data set iles_nunavik et aérien_nunavik et nouveau equivalances code espèce)
            source = "oies",
            locality = NA,
            abondance = as.numeric(abondance),
            sampling_id=date
        ) 

    # Join TAXO - Match CODE_ID using Code4_FR via right_join
<<<<<<< HEAD
    oies$code_id<-taxo$Species_ID[match(biomq$NomFR,taxo$French_Name)]
    
=======
    # TODO
    oies$code_id<- taxo$Species_ID[match(biomq$NomFR,taxo$French_Name)]
>>>>>>> 1e14150 (Ajout de data set iles_nunavik et aérien_nunavik et nouveau equivalances code espèce)
    
    oies <- oies |>
      dplyr::mutate(
        code_id = ifelse(
          NomFR %in% names(equivalences),
          equivalences[NomFR],
          code_id
        )
      )

    # Correct longitudes
    oies <- oies |>
        dplyr::mutate(longitude = -(abs(longitude)))

    # Re-order cols
    oies <- dplyr::select(oies, dplyr::all_of(final_cols))

    cli::cli_alert_success("Returning {nrow(oies)} rows")

    return(oies)
}
