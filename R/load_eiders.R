#' Load and process the Eider Hiver dataset
#'
#' This function loads and processes the "Eider Hiver" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_eider <- load_eider_hiver()
#' }
#' @export
load_eider_hiver <- function() {

    cli::cli_h2("Eider Hiver")
    cli::cli_alert_info("Starting integration procedure on {external_files()$eider_hiver$path}")

    # Assert file exists
    if (!file.exists(external_files()$eider_hiver$path)) {
        cli::cli_abort("Could not find file: {external_files()$eider_hiver$path}")
    }

    eiderhiver <- data.table::fread(external_files()$eider_hiver$path, dec = ",", sep = ";") |> tibble::as_tibble()

    # Assert columns exist
    missing_cols <- setdiff(external_files()$eider_hiver$check_columns, names(eiderhiver))
    if (length(missing_cols) > 0) {
        cli::cli_abort(c(
            "Missing required columns in dataset:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    cli::cli_alert_info("Applying transformation on {nrow(eiderhiver)} rows")

    eiderhiver <- eiderhiver |>
        dplyr::select(Region, An, Mois, Jour, Species, visuelblancs, visuelbruns, inconnus, LatDec, LongDec) |>
        dplyr::mutate(
            latitude = as.numeric(LatDec),
            longitude = as.numeric(LongDec),
            n_obs = rowSums(eiderhiver[, c("visuelblancs", "visuelbruns", "inconnus")], na.rm = T),
            date = lubridate::make_date(An, Mois, Jour),
            link = external_files()$eider_hiver$path,
            source = "Eider Hiver",
            inv_type = NA, # Helicopter bateau ?
            locality = Region,
            obs = NA, # Observateurs disponibles
            colony = FALSE
        ) |>
        dplyr::select(!c(visuelblancs, visuelbruns, inconnus, An, Mois, Jour)) |>
        dplyr::rename(
            abondance = n_obs,
            code_id = Species
        )

      dplyr::mutate(
        latitude = as.numeric(LatDec),
        longitude = as.numeric(LongDec),
        n_obs = rowSums(eiderhiver[, c("visuelblancs", "visuelbruns", "inconnus")], na.rm = T),
        date = lubridate::make_date(An, Mois, Jour),
        source = "Eider Hiver",
        inv_type = "aeronef", 
        locality = Region,
        obs = NA, # Observateurs disponibles
        sampling_id=as.character(An)
      ) |>
      dplyr::select(!c(visuelblancs, visuelbruns, inconnus, An, Mois, Jour)) |>
      dplyr::rename(
        abondance = n_obs,
        code_id = Species
      )
    
    # Re-order cols
    eiderhiver <- dplyr::select(eiderhiver, dplyr::all_of(final_cols))

    cli::cli_alert_success("Returning {nrow(eiderhiver)} rows")
    return(eiderhiver)
}
