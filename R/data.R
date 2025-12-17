#' Taxonomy Reference Table
#'
#' @name taxonomy
#' @docType data
#' @keywords datasets
#'
#' @description
#' A reference table combining avian core data (French/Latin names) and taxonomy
#' vulnerability data (milieu_marin, groupe_fonctionnel). This dataset is used
#' internally by all load_* functions to enrich bird observation data with
#' taxonomic information.
#'
#' @format A data frame with 2474 rows and 6 variables:
#' \describe{
#'   \item{code_id}{Species ID code (character)}
#'   \item{nom_francais}{French common name (character)}
#'   \item{nom_latin}{Scientific/Latin name (character)}
#'   \item{milieu_marin}{Marine habitat indicator (character)}
#'   \item{groupe_fonctionnel}{Functional group classification (character)}
#'   \item{code_fr}{Normalized French name code for matching (character)}
#' }
#'
#' @details
#' This dataset is automatically loaded when the package is loaded. It is
#' generated from two source files:
#' \itemize{
#'   \item ECCC_Avian_Core_20241025.csv - Contains species IDs, French and Latin names
#'   \item taxo_vulnerabilite.csv - Contains marine habitat and functional group data
#' }
#'
#' The dataset is built using the \code{data-raw/build_taxonomy.R} script, which
#' joins and processes the source data.
#'
#' @source Environment and Climate Change Canada (ECCC) Avian Core Database
#'
#' @examples
#' \dontrun{
#' # Access the taxonomy dataset
#' head(taxonomy)
#'
#' # Filter by marine habitat
#' marine_birds <- taxonomy[taxonomy$milieu_marin == "Oui", ]
#'
#' # Find a specific species
#' taxonomy[taxonomy$code_id == "MALL", ]
#' }
#' @usage taxonomy
NULL
