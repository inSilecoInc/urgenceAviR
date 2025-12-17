#' datasets_config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_datasets_config_ui <- function(id){
  ns <- NS(id)
  tagList()  # This module is modal-based, no UI needed
}

#' datasets_config Server Functions
#'
#' @noRd
mod_datasets_config_server <- function(id, app_values){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get file_upload setting from config
    file_upload_mode <- isTRUE(get_golem_config("file_upload"))

    # Reactive values
    values <- reactiveValues(
      folder_configured = all(sapply(external_files(), function(x) is.null(x$path))),
      selected_folder_path = NULL,
      uploaded_files = list(),
      temp_folder = NULL
    )

    # Initialize shinyFiles for directory selection (only for folder mode)
    if (!file_upload_mode) {
      volumes <- c(Home = fs::path_home(), "Root" = "/")
    }

    # Check if datasets folder needs to be configured
    observe({
      # Watch for changes in datasets_folder_configured
      if (!isTRUE(app_values$datasets_folder_configured)) {
        # Check if external files are configured
        if (all(sapply(external_files(), function(x) is.null(x$path)))) {
          cli::cli_alert_warning("Datasets folder not set, showing configuration modal")
          if (file_upload_mode) {
            show_upload_modal(ns)
          } else {
            show_folder_modal(ns)
          }
        }
      } else {
        values$folder_configured <- TRUE
      }
    })

    # Setup observers based on mode
    if (!file_upload_mode) {
      # Setup folder selection modal observers (local mode)
      setup_folder_modal_observers(input, output, session, ns, values, app_values, volumes)
    } else {
      # Setup file upload modal observers (production mode)
      setup_upload_modal_observers(input, output, session, ns, values, app_values)
    }
  })
}
