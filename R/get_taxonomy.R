#' Get taxonomy reference table
#'
#' @description
#' Returns a joined taxonomy table combining avian core data (French/Latin names)
#' and taxonomy vulnerability data (milieu_marin, groupe_fonctionnel).
#'
#' @return Data frame with columns: code_id, nom_francais, nom_latin, milieu_marin, groupe_fonctionnel
#' @noRd
get_taxonomy <- function() {

  cli::cli_alert_info("Loading taxonomy information")

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

  # Build taxonomy reference table
  if (!is.null(avian) && !is.null(taxo)) {
    cli::cli_alert_info("Building taxonomy reference table")

    # Extract necessary columns from avian core
    avian_info <- avian |>
      dplyr::select(
        code_id = .data$Species_ID,
        nom_francais = .data$French_Name,
        nom_latin = .data$Scientific_Name
      )

    # Extract necessary columns from taxonomy vulnerability
    taxo_info <- taxo |>
      dplyr::select(
        code_id = .data$species_id,
        milieu_marin = .data$milieu_marin,
        groupe_fonctionnel = .data$groupe_fonctionnel
      )

    # Join the two tables
    taxonomy_df <- avian_info |>
      dplyr::left_join(taxo_info, by = "code_id") |>
      dplyr::mutate(code_fr = stringi::stri_trans_general(tolower(nom_francais), "latin-ascii")) |>
      dplyr::mutate(code_fr = stringr::str_replace_all(code_fr, "non identifiee?", "sp.")) |>
      tibble::as_tibble()

    cli::cli_alert_success("Taxonomy table created with {nrow(taxonomy_df)} species")
    return(taxonomy_df)

  } else {
    cli::cli_alert_warning("Could not build taxonomy table - missing data files")
    return(NULL)
  }
}
