#' Load and process the SOMEC dataset
#'
#' This function loads and processes the "SOMEC" dataset from a predefined external source.
#' It validates the file and columns, applies transformations, and integrates species codes using a reference table.
#'
#' @return A processed `data.frame` with standardized columns and integrated species codes.
#' @examples
#' \dontrun{
#' processed_somec <- load_somec()
#' }
#' @export
load_somec <- function() {

    cli::cli_h2("SOMEC")
    cli::cli_alert_info("Starting integration procedure on {external_files()$somec$path}")

    # Assert file exists
    if (!file.exists(external_files()$somec$path)) {
        cli::cli_abort("Could not find file: {external_files()$somec$path}")
    }

    somec <- data.table::fread(external_files()$somec$path) |> tibble::as_tibble()

    # Assert columns exist
    missing_cols <- setdiff(external_files()$somec$check_columns, names(somec))
    if (length(missing_cols) > 0) {
        cli::cli_abort(c(
            "Missing required columns in dataset:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    cli::cli_alert_info("Applying transformation on {nrow(somec)} rows")

    somec <- somec |>
        dplyr::rename(
            latitude = ObsLat,
            longitude = ObsLong,
            date = Date,
            abondance = Count,
            obs = Observer
        ) |>
        dplyr::select(latitude, longitude, date, abondance, obs, alpha = Alpha) |>
        dplyr::mutate(
            date = as.Date(date),
            latitude = as.numeric(latitude),
            longitude = as.numeric(longitude),
            source = "somec",
            link = external_files()$somec$path,
            inv_type = NA,
            locality = NA,
            colony = FALSE
        )

    # Join TAXO - Match CODE_ID using Alpha_Code
    somec <- somec |>
        dplyr::rename(
            code_id = alpha
        ) |>
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
    somec <- dplyr::select(somec, dplyr::all_of(final_cols))

    cli::cli_alert_success("Returning {nrow(somec)} rows")

    return(somec)
}
