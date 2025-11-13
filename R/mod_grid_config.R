#' grid_config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_grid_config_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        h3("Étape 3 : Configuration de la grille", class = "text-primary"),
        p("Configurez les paramètres de grille pour l'analyse spatiale et l'agrégation des données.")
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h4("Paramètres de la grille"),
          
          fluidRow(
            column(
              width = 6,
              numericInput(
                ns("grid_size"),
                "Taille des cellules de grille (km) :",
                value = 10,
                min = 1,
                max = 100,
                step = 1
              )
            ),
            column(
              width = 6,
              selectInput(
                ns("grid_type"),
                "Type de grille :",
                choices = list(
                  "Carré" = "square",
                  "Hexagonale" = "hexagonal"
                ),
                selected = "square"
              )
            )
          ),
          
          numericInput(
            ns("min_observations"),
            "Observations minimales par cellule :",
            value = 5,
            min = 1,
            max = 100,
            step = 1
          )
        )
      ),
      
      column(
        width = 6,
        wellPanel(
          h4("Aperçu des données"),
          
          actionButton(
            ns("load_preview"),
            "Charger l'aperçu des données",
            icon = icon("eye"),
            class = "btn-info"
          ),
          

          
          conditionalPanel(
            condition = "output.preview_loaded",
            ns = ns,
            div(
              class = "alert alert-success",
              icon("check-circle"), " Aperçu des données chargé avec succès !"
            )
          ),
          
          br(),
          
          actionButton(
            ns("validate_config"),
            "Valider la configuration",
            icon = icon("check"),
            class = "btn-success",
            disabled = TRUE
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        conditionalPanel(
          condition = "output.config_validated",
          ns = ns,
          div(
            class = "alert alert-success",
            icon("check-circle"), " Configuration de la grille validée !",
            br(),
            "Vous pouvez maintenant procéder à la génération de figures."
          )
        )
      )
    )
  )
}

#' grid_config Server Functions
#'
#' @noRd 
mod_grid_config_server <- function(id, species_temporal_result, app_values){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      preview_data = NULL,
      config_validated = FALSE,
      preview_loaded = FALSE
    )
    
    # Load data preview
    observeEvent(input$load_preview, {
      req(target_area())
      req(species_temporal_result())
      
      cli::cli_alert_info("Loading data preview for grid configuration")
      
      withProgress(message = 'Chargement de l\'aperçu...', value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Using pre-loaded datasets")
          
          # Use pre-loaded data
          req(app_values$all_df)
          all_data <- app_values$all_df
          
          incProgress(0.4, detail = "Using spatially filtered data")
          
          # Use spatially filtered data from target_area module
          spatial_filtered <- all_data
          
          incProgress(0.6, detail = "Filtering by time")
          
          # Filter by years from species_temporal_result
          species_config <- species_temporal_result()
          selected_years <- species_config$selected_years
          
          if ("date" %in% names(spatial_filtered)) {
            date_col <- as.Date(spatial_filtered$date)
            year_col <- as.numeric(format(date_col, "%Y"))
            temporal_filtered <- spatial_filtered[year_col %in% selected_years, ]
            
            # Filter by yearly period
            yearly_period <- species_config$yearly_period
            month_day <- format(date_col, "%m-%d")
            period_start <- format(yearly_period[1], "%m-%d")
            period_end <- format(yearly_period[2], "%m-%d")
            
            if (period_start <= period_end) {
              period_filtered <- temporal_filtered[
                format(as.Date(temporal_filtered$date), "%m-%d") >= period_start &
                format(as.Date(temporal_filtered$date), "%m-%d") <= period_end,
              ]
            } else {
              period_filtered <- temporal_filtered[
                format(as.Date(temporal_filtered$date), "%m-%d") >= period_start |
                format(as.Date(temporal_filtered$date), "%m-%d") <= period_end,
              ]
            }
          } else {
            period_filtered <- spatial_filtered
          }
          
          incProgress(0.8, detail = "Filtering by species")
          
          # Filter by species from species_temporal_result
          if (species_config$species_method == "individual" && 
              !is.null(species_config$selected_species) && 
              length(species_config$selected_species) > 0) {
            final_filtered <- period_filtered[period_filtered$code_id %in% species_config$selected_species, ]
          } else if (species_config$species_method == "group") {
            # This would need to be implemented based on your species grouping logic
            final_filtered <- period_filtered
          } else {
            final_filtered <- period_filtered
          }
          
          values$preview_data <- final_filtered
          values$preview_loaded <- TRUE
          
          # Enable validation button
          shinyjs::enable("validate_config")
          
          incProgress(1, detail = "Complete")
          
          cli::cli_alert_success("Grid preview loaded: {nrow(final_filtered)} records")
          
        }, error = function(e) {
          cli::cli_alert_danger("Error loading grid preview: {e$message}")
          showNotification(paste("Erreur de chargement de l'aperçu :", e$message), type = "error")
        })
      })
    })
    
    # Validate configuration
    observeEvent(input$validate_config, {
      req(values$preview_data)
      
      cli::cli_alert_info("Validating grid configuration")
      
      # Validation checks
      n_records <- nrow(values$preview_data)
      
      if (n_records == 0) {
        cli::cli_alert_warning("No records found with current configuration")
        showNotification("Avertissement : Aucun enregistrement trouvé avec la configuration actuelle. Veuillez ajuster vos filtres.", type = "warning")
        return()
      } else if (n_records < input$min_observations) {
        cli::cli_alert_warning("Very few records found ({n_records})")
        showNotification("Avertissement : Très peu d'enregistrements trouvés. Les résultats peuvent ne pas être fiables.", type = "warning")
      }
      
      # Estimate grid cells
      req(target_area())
      area_km2 <- target_area()$area_km2
      cell_area <- input$grid_size^2
      estimated_cells <- ceiling(area_km2 / cell_area)
      
      values$config_validated <- TRUE
      
      cli::cli_alert_success("Grid configuration validated: {n_records} records, ~{estimated_cells} cells")
      showNotification("Configuration de la grille validée avec succès !", type = "message")
    })
    
    # Output for conditional panels
    output$preview_loaded <- reactive({
      values$preview_loaded
    })
    outputOptions(output, "preview_loaded", suspendWhenHidden = FALSE)
    
    output$config_validated <- reactive({
      values$config_validated
    })
    outputOptions(output, "config_validated", suspendWhenHidden = FALSE)
    
    # Return the validated configuration for use by other modules
    return(reactive({
      if (values$config_validated) {
        list(
          grid_size = input$grid_size,
          grid_type = input$grid_type,
          min_observations = input$min_observations,
          preview_data = values$preview_data
        )
      } else {
        NULL
      }
    }))
  })
}
