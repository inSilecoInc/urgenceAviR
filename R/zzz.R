# Package environment for storing mutable state
.pkg_env <- new.env(parent = emptyenv())

# List of external files with file names and required column names
.pkg_env$external_files <- list(
  canards_de_mer = list(
    file = "ConsultationCanardsDeMer.csv",
    path = NULL,
    check_columns = c("NomLieu", "LATITUDE", "LONGITUDE", "Annee", "Mois", "Jour", "NombreTotal", "Nom_FR")
  ),
  eider_hiver = list(
    file = "ConsultationEiderHiver.csv",
    path = NULL,
    check_columns = c("Region", "An", "Mois", "Jour", "Species", "visuelblancs", "visuelbruns", "inconnus", "LatDec", "LongDec")
  ),
  garrot = list(
    file = "ConsultationGarrot.csv",
    path = NULL,
    check_columns = c("annee", "mois", "jour", "CodeSp", "N", "Observateurs", "Lat", "Long", "loc_ID")
  ),
  macreuse = list(
    file = "ConsultationMacreuses.csv",
    path = NULL,
    check_columns = c("Date", "Observateur", "Espece", "Nombre", "Longitude", "Latitude")
  ),
  oies = list(
    file = "ConsultationOieDesNeigesPrintemps.csv",
    path = NULL,
    check_columns = c("Date", "Observateur", "Code", "Count", "Longitude", "Latitude")
  ),
  sauvagine_fleuve = list(
    file = "ConsultationSauvagineFleuve.csv",
    path = NULL,
    check_columns = c("Date", "Latitude", "Longitude", "Nombre", "Observateur")
  ),
  sriv = list(
    file = "ConsultationSRIV.csv",
    path = NULL,
    check_columns = c("debut", "obslat", "obslong", "total", "obsdro")
  ),
  somec = list(
    file = "ConsultationSOMEC.csv",
    path = NULL,
    check_columns = c("Alpha", "ObsLat", "ObsLong", "Date", "Count", "ObserverName")
  ),
  biomq = list(
    file = "consultationBIOMQ.xlsx",
    path = NULL,
    check_columns = c(
      "NomCol", "CentroideX", "CentroideY", "NomFR",
      "nb_nicheur", "methode", "nomRef", "Ann\u00e9e",
      "MoisDebut", "JourDebut"
    )
  ),
  iles_nunavik = list(
    file = "ConsultationIles_Nunavik.csv",
    path = NULL,
    check_columns = c(
      "Nom_Ile", "Longitude", "Latitude", "Nom_francais",
      "Nb_compte", "Methode_descriptif", "Annee",
      "Mois", "Jour")
  ),
  inventaire_aerien_nunavik = list(
    file = "ConsultationInventaire_aerien_Nunavik.csv",
    path = NULL,
    check_columns = c(
      "Obs_ID", "Nom_fran\u00e7ais", "Longitude", "Latitude",
      "Nb_total_ind",  "Date","Observateur")
  ),
  atlantic_colonies = list(
    file = "all_atlantic_colonies_obs.csv",
    path = NULL,
    check_columns = c(
      "ColonyId", "Species_code.full", "Long", "Lat",
      "Colony_size",  "Date","Source","CensusId")
  )
)

# Accessor function for external_files
external_files <- function() {
  .pkg_env$external_files
}

#' Set datasets folder path
#'
#' @param path Path to the datasets folder (must end with "/")
#' @export
set_datasets_folder <- function(path) {
  if(is.null(path)){
    for (i in names(.pkg_env$external_files)) {
      .pkg_env$external_files[[i]]$path <- NULL
    }
  } else {
    # Update all file paths with new base path
    for (i in names(.pkg_env$external_files)) {
      .pkg_env$external_files[[i]]$path <- file.path(path, .pkg_env$external_files[[i]]$file)
    }
  }
}

final_cols <- c(
  "latitude",
  "longitude",
  "locality",
  "date",
  "code_id",
  "abondance",
  "obs",
  "inv_type",
  "source",
  "nom_francais",
  "nom_latin",
  "milieu_marin",
  "groupe_fonctionnel"
)

.onLoad <- function(libname, pkgname) {
  taxonomy <- utils::data(taxonomy)
}
