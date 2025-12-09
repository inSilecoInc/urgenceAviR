#' Enrich dataset with taxonomy information
#'
#' @description
#' Joins avian core data (French/Latin names) and taxonomy vulnerability data
#' (milieu_marin, groupe_fonctionnel) to the main dataset.
#'
#' @param df Data frame to enrich
#'
#' @return Data frame with added taxonomy columns
#' @noRd
enrich_with_taxonomy <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    cli::cli_alert_warning("Empty or NULL dataframe provided to enrich_with_taxonomy")
    return(df)
  }

  cli::cli_alert_info("Enriching dataset with taxonomy information")

  # Load Avian Core data
  avian <- tryCatch({
    readr::read_delim(
      system.file("exdata/Taxonomy/ECCC_Avian_Core_20241025.csv", package = "urgenceAviR"),
      delim = ";",
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE
    )
  }, error = function(e) {
    cli::cli_alert_warning("Could not load Avian Core file: {e$message}")
    NULL
  })

  # Load taxonomy vulnerability data
  taxo <- tryCatch({
    readr::read_delim(
      system.file("exdata/Taxonomy/taxo_vulnerabilite.csv", package = "urgenceAviR"),
      delim = ";",
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE
    )
  }, error = function(e) {
    cli::cli_alert_warning("Could not load taxonomy vulnerability file: {e$message}")
    NULL
  })

  # Join with avian_core to get French and Latin names
  if (!is.null(avian)) {
    cli::cli_alert_info("Joining with Avian Core data")
    avian_info <- avian |>
      dplyr::select(
        code_id = .data$Species_ID,
        nom_francais = .data$French_Name,
        nom_latin = .data$Scientific_Name
      )

    df <- df |>
      dplyr::left_join(avian_info, by = "code_id")

    # Then join with taxo for milieu_marin and groupe_fonctionnel
    if (!is.null(taxo)) {
      cli::cli_alert_info("Joining with taxonomy vulnerability data")
      taxo_info <- taxo |>
        dplyr::select(
          code_id = .data$species_id,
          milieu_marin = .data$milieu_marin,
          groupe_fonctionnel = .data$groupe_fonctionnel
        )

      df <- df |>
        dplyr::left_join(taxo_info, by = "code_id")
    }

    cli::cli_alert_success("Taxonomy enrichment complete")
  } else {
    cli::cli_alert_warning("Skipping taxonomy enrichment - Avian Core data not available")
  }

  return(df)
}
