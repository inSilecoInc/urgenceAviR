#' Access files in the current app
#'
#' @param ... Character vectors specifying the path within the `inst/app/www` folder.
#'
#' @return The full path to the file.
#' @export
#'
#' @examples
#' \dontrun{
#'   app_sys("js", "app.js")
#' }
app_sys <- function(...) {
  system.file(..., package = "urgenceAviR")
}

#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
