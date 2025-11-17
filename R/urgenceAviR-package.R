#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @importFrom graphics image legend
#' @importFrom stats complete.cases rnorm runif
## usethis namespace: end
NULL

# Global variables used in data.table operations
utils::globalVariables(c(
  ".data",
  "groupe_fonctionnel",
  "nomfr",
  "nomla",
  "milieu_marin",
  "nom_francais",
  "nom_latin",
  "inv_type"
))
