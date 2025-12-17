# Script to build taxonomy data for the package
# This creates data/taxonomy.rda which will be available as a dataset

cli::cli_alert_info("Loading taxonomy information")

# Load Avian Core data
avian <- tryCatch({
  readr::read_delim(
    file.path("data-raw/ECCC_Avian_Core_20241025.csv"),
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
    file.path("data-raw/taxo_vulnerabilite.csv"),
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
      code_id = Species_ID,
      nom_francais = French_Name,
      nom_latin = Scientific_Name
    )

  # Extract necessary columns from taxonomy vulnerability
  taxo_info <- taxo |>
    dplyr::select(
      code_id = species_id,
      milieu_marin = milieu_marin,
      groupe_fonctionnel = groupe_fonctionnel
    )

  # Join the two tables
  taxonomy <- avian_info |>
    dplyr::left_join(taxo_info, by = "code_id") |>
    dplyr::mutate(code_fr = stringi::stri_trans_general(tolower(nom_francais), "latin-ascii")) |>
    dplyr::mutate(code_fr = stringr::str_replace_all(code_fr, "non identifiee?", "sp.")) |>
    tibble::as_tibble()

  cli::cli_alert_success("Taxonomy table created with {nrow(taxonomy)} species")

  # Ensure data directory exists
  if (!dir.exists("data")) {
    dir.create("data")
  }

  # Save as package data (taxonomy.rda)
  save(taxonomy, file = "data/taxonomy.rda", compress = "xz")

  cli::cli_alert_success("Taxonomy data saved to data/taxonomy.rda")
  message("Rows: ", nrow(taxonomy))

} else {
  cli::cli_alert_warning("Could not build taxonomy table - missing data files")
}
