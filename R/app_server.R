#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {
  
  # Initialize CLI logging
  cli::cli_alert_info("UrgenceAviR Shiny application started")
  
  # Reactive values for cross-module communication
  app_values <- reactiveValues(
    datasets_loaded = FALSE,
    datasets_folder_configured = FALSE,
    target_area_locked = FALSE,
    navigate_to_tab = NULL
  )

  # Set datasets_folder if provided in config
  if (!is.null(get_golem_config("datasets_folder"))) {
    cli::cli_alert_info("Setting datasets folder from config: {get_golem_config('datasets_folder')}")
    set_datasets_folder(get_golem_config("datasets_folder"))
    app_values$datasets_folder_configured <- TRUE
  }
  
  # Initialize datasets configuration module
  mod_datasets_config_server("datasets_config", app_values)

  # Load datasets when folder is configured
  observe({
    req(app_values$datasets_folder_configured)

    if (app_values$datasets_folder_configured && !app_values$datasets_loaded) {
      cli::cli_alert_info("Loading datasets on startup")
      load_datasets_startup()
    }
  })
  
  # Load datasets function
  load_datasets_startup <- function() {
    tryCatch({
      shinycssloaders::showPageSpinner(caption = "Chargement des donn\u00e9es...")
      
      cli::cli_alert_info("Starting dataset loading process...")
      
      all_data <- load_all_datasets(combine = TRUE) |>
        dplyr::filter(lubridate::year(.data$date) > 1900) |>
        dplyr::filter(!is.na(.data$latitude) | !is.na(.data$longitude)) |>
        dplyr::filter(!is.na(.data$code_id)) |>
        dplyr::filter(.data$abondance > 0)

      app_values$all_df <- all_data
      app_values$datasets_loaded <- TRUE
      
      shinycssloaders::hidePageSpinner()
      
      cli::cli_alert_success("Loaded {nrow(all_data)} observations")
      showNotification("Donn\u00e9es charg\u00e9es avec succ\u00e8s !", type = "message")
    }, error = function(e) {
      shinycssloaders::hidePageSpinner()
      cli::cli_alert_danger("Error loading datasets: {e$message}")
      showNotification(paste("Erreur lors du chargement des donn\u00e9es :", e$message), type = "error")
    })
  }
  
  # Module servers - pass app_values to modules
  tryCatch({
    mod_target_area_server("target_area", app_values)
  }, error = function(e) {
    cli::cli_alert_danger("Error initializing target_area module: {e$message}")
  })

  tryCatch({
    mod_species_temporal_server("species_temporal", app_values)
  }, error = function(e) {
    cli::cli_alert_danger("Error initializing species_temporal module: {e$message}")
  })

  tryCatch({
    mod_make_grid_server("make_grid", app_values)
  }, error = function(e) {
    cli::cli_alert_danger("Error initializing make_grid module: {e$message}")
  })

  # Disable tabs 2 and 3 initially (when no area is locked)
  observe({
    priority = 100  # High priority to run early

    if (!isTRUE(app_values$target_area_locked)) {
      cli::cli_alert_info("Target area not locked - disabling tabs 2 and 3")
      shinyjs::addClass(selector = "a[data-value='species_temporal']", class = "disabled")
      shinyjs::addClass(selector = "a[data-value='make_grid']", class = "disabled")

      # Add CSS to make disabled tabs appear grayed out and non-clickable
      shinyjs::runjs("
        $('a[data-value=\"species_temporal\"]').css({
          'pointer-events': 'none',
          'opacity': '0.5',
          'cursor': 'not-allowed'
        });
        $('a[data-value=\"make_grid\"]').css({
          'pointer-events': 'none',
          'opacity': '0.5',
          'cursor': 'not-allowed'
        });
      ")
    }
  })

  # Enable tabs when area is locked
  observe({
    req(app_values$target_area_locked)

    cli::cli_alert_info("Target area locked - enabling tabs 2 and 3")
    shinyjs::removeClass(selector = "a[data-value='species_temporal']", class = "disabled")
    shinyjs::removeClass(selector = "a[data-value='make_grid']", class = "disabled")

    # Remove CSS to re-enable tabs
    shinyjs::runjs("
      $('a[data-value=\"species_temporal\"]').css({
        'pointer-events': 'auto',
        'opacity': '1',
        'cursor': 'pointer'
      });
      $('a[data-value=\"make_grid\"]').css({
        'pointer-events': 'auto',
        'opacity': '1',
        'cursor': 'pointer'
      });
    ")
  })

  # Handle navigation requests from modules
  observe({
    req(app_values$navigate_to_tab)

    cli::cli_alert_info("Navigating to tab: {app_values$navigate_to_tab}")
    updateNavbarPage(
      session = session,
      inputId = "main_nav",
      selected = app_values$navigate_to_tab
    )

    # Reset navigation signal
    app_values$navigate_to_tab <- NULL
  })

}
