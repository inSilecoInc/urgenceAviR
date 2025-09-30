#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {
  
  # Initialize CLI logging
  cli::cli_alert_info("UrgenceAviR Shiny application started")
  

  # Set datasets_folder if provided in config
  if (!is.null(get_golem_config("datasets_folder"))) {
    cli::cli_alert_info("Setting datasets folder from config: {get_golem_config('datasets_folder')}")
    set_datasets_folder(get_golem_config("datasets_folder"))
  }
  
  # Reactive values for cross-module communication
  app_values <- shiny::reactiveValues(
    available_species = NULL,
    available_years = NULL,
    datasets_loaded = FALSE
  )
  
  # Initialize datasets configuration module
  datasets_config_result <- mod_datasets_config_server("datasets_config")
  
  # Load datasets when folder is configured
  shiny::observe({
    req(datasets_config_result())
    
    if (datasets_config_result()$configured && !app_values$datasets_loaded) {
      cli::cli_alert_info("Loading datasets on startup")
      load_datasets_startup()
    }
  })
  
  # Load datasets function
  load_datasets_startup <- function() {
    tryCatch({
      withProgress(message = 'Loading datasets...', value = 0, {
        incProgress(0.3, detail = "Reading datasets")
        
        all_data <- load_all_datasets(combine = TRUE) |>
          dplyr::filter(lubridate::year(date) > 1900) |>
          dplyr::filter(!is.na(latitude) | !is.na(longitude)) |>
          dplyr::filter(!is.na(code_id))
        
        incProgress(0.6, detail = "Processing species list")
        species_list <- sort(unique(all_data$code_id))
        
        incProgress(0.8, detail = "Processing years")
        if ("date" %in% names(all_data)) {
          years <- sort(unique(as.numeric(format(as.Date(all_data$date), "%Y"))))
          years <- years[!is.na(years)]
        } else {
          years <- NULL
        }
        
        app_values$available_species <- species_list
        app_values$available_years <- years
        app_values$all_data <- all_data
        app_values$datasets_loaded <- TRUE
        
        incProgress(1, detail = "Complete")
        
        cli::cli_alert_success("Loaded {length(species_list)} species and {length(years)} years")
        shiny::showNotification("Datasets loaded successfully!", type = "message")
      })
    }, error = function(e) {
      cli::cli_alert_danger("Error loading datasets: {e$message}")
      shiny::showNotification(paste("Error loading datasets:", e$message), type = "error")
    })
  }
  
  # Module servers - pass app_values to species module
  target_area_result <- mod_target_area_server("target_area", app_values)
  species_temporal_result <- mod_species_temporal_server("species_temporal", target_area_result, app_values)
  grid_config_result <- mod_grid_config_server("grid_config", target_area_result, species_temporal_result, app_values)
  figure_generation_result <- mod_figure_generation_server("figure_generation", target_area_result, grid_config_result)
  
  
  
  
  # Session end logging
  session$onSessionEnded(function() {
    cli::cli_alert_info("UrgenceAviR Shiny session ended")
  })
}
