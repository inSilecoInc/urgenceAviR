#' Launch the UrgenceAviR Shiny Application
#'
#' This function launches the Golem-based Shiny web application for bird data analysis.
#' The app provides a sequential workflow for target area selection, species filtering,
#' and figure generation.
#'
#' @param onStart A function that will be called before the app is actually run.
#'   This is only needed for `shinyAppObj`, since in the `shinyAppDir`
#'   case, the `global.R` file can serve this purpose.
#' @param options Named options that should be passed to the `runApp` call
#'   (these can be any of the following: "port", "launch.browser", "host", "quiet").
#' @param enableBookmarking Can be one of "url", "server", or "disable".
#'   The default value, NULL, will respect the setting from any previous calls to
#'   enableBookmarking(). See enableBookmarking() for more information on bookmarking your app.
#' @param uiPattern A regular expression that will be applied to each `GET`
#'   request to determine whether the `ui` should be used to handle the request.
#'   Note that the entire request path must match the regular expression in order
#'   for the match to be considered successful.
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @details
#' The application includes the following workflow steps:
#' \itemize{
#'   \item \strong{Target Area Selection}: Draw polygons or upload spatial files
#'   \item \strong{Species & Temporal Filtering}: Select species, years, and periods
#'   \item \strong{Figure Generation}: Create visualizations and analysis outputs
#' }
#'
#' You can set the datasets folder within the app or use \code{set_datasets_folder()}
#' before launching.
#'
#' @return A Shiny application object.
#'
#' @examples
#' \dontrun{
#' # Set datasets folder (optional)
#' set_datasets_folder("/path/to/your/datasets/")
#' 
#' # Launch the app
#' run_app()
#' 
#' # Launch with specific options
#' run_app(options = list(port = 3838, launch.browser = TRUE))
#' }
#'
#' @export
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  
  # Check if required packages are available
  required_packages <- c("shiny", "bslib", "bsicons", "leaflet", "leaflet.extras", "DT", "sf", "shinyjs", "shinyFiles", "fs", "yaml", "reactable")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(
      "Les packages suivants sont requis pour exécuter l'application Shiny mais ne sont pas installés :\n",
      paste(missing_packages, collapse = ", "), "\n\n",
      "Veuillez les installer en utilisant :\n",
      "install.packages(c(", paste0("\"", missing_packages, "\"", collapse = ", "), "))"
    )
  }
  
  # Load required packages
  require(shiny, quietly = TRUE)
  require(bslib, quietly = TRUE)
  require(bsicons, quietly = TRUE)
  require(leaflet, quietly = TRUE)
  require(leaflet.extras, quietly = TRUE)
  require(DT, quietly = TRUE)
  require(shinyjs, quietly = TRUE)
  require(shinyFiles, quietly = TRUE)
  require(fs, quietly = TRUE)
  require(yaml, quietly = TRUE)
  require(reactable, quietly = TRUE)
  
  # Check if datasets folder is set
  if (is.null(datasets_folder())) {
    message("Note : Le dossier de données n'est pas défini. Vous pouvez le définir dans l'application ou utiliser set_datasets_folder() d'abord.")
  }
  
  shinyApp(
    ui = app_ui,
    server = app_server,
    onStart = onStart,
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}
