#' species_temporal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_species_temporal_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        h3("Étape 2 : Sélection des espèces et des périodes temporelles", class = "text-primary"),
        p("Sélectionnez les espèces cibles, définissez les filtres temporels et configurez les paramètres de grille pour l'analyse."),
        hr()
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        bslib::card(
          bslib::card_header("Sélection des espèces"),
          bslib::card_body(
          
          radioButtons(
            ns("species_method"),
            "Méthode de sélection :",
            choices = list(
              "Espèces individuelles" = "individual",
              "Groupe d'espèces" = "group",
              "Toutes les espèces" = "all"
            ),
            selected = "individual"
          ),
          
          conditionalPanel(
            condition = "input.species_method == 'individual'",
            ns = ns,
            div(
              style = "position: relative; z-index: 1000;",
              selectizeInput(
                ns("selected_species"),
                "Choisissez les espèces :",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Tapez pour rechercher des espèces...",
                  maxItems = 20
                )
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.species_method == 'group'",
            ns = ns,
            selectInput(
              ns("species_group"),
              "Choisissez un groupe d'espèces :",
              choices = list(
                "Sauvagine" = "waterfowl",
                "Oiseaux marins" = "seabirds", 
                "Oiseaux de rivage" = "shorebirds",
                "Rapaces" = "raptors",
                "Passereaux" = "passerines"
              ),
              multiple = TRUE
            )
          ),
          
          conditionalPanel(
            condition = "input.species_method == 'all'",
            ns = ns,
            div(
              class = "alert alert-info",
              icon("info-circle"), " Toutes les espèces disponibles seront incluses dans l'analyse."
            )
          )
          )
        )
      ),
      
      column(
        width = 4,
        bslib::card(
          bslib::card_header("Filtrage temporel"),
          bslib::card_body(
          
          h5("Sélection des années"),
          sliderInput(
            ns("selected_years"),
            "Sélectionnez la plage d'années :",
            min = 2000,
            max = 2024,
            value = c(2019, 2024),
            step = 1,
            sep = ""
          ),
          
          h5("Période annuelle"),
          dateRangeInput(
            ns("yearly_period"),
            "Sélectionnez la période dans chaque année :",
            start = "2024-01-01",
            end = "2024-12-31",
            format = "mm-dd",
            startview = "month"
          ),
          
          helpText("Ceci définit la fenêtre temporelle dans chaque année sélectionnée (ex. : période de migration).")
          )
        )
      ),
      
      column(
        width = 4,
        bslib::card(
          bslib::card_header("Source des données"),
          bslib::card_body(
          
          div(
            style = "position: relative; z-index: 999;",
            selectizeInput(
              ns("selected_sources"),
              "Sélectionnez les sources de données :",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Toutes les sources sélectionnées par défaut...",
                plugins = list('remove_button')
              )
            )
          ),
          
          helpText("Filtrez par source de données (laissez vide pour inclure toutes les sources).")
          )
        )
      )
    ),
    
    
    fluidRow(
      column(
        width = 12,
        bslib::card(
          bslib::card_header("Tableau de données"),
          bslib::card_body(
            reactable::reactableOutput(ns("obs_table"))
          )
        )
      )
    )
  )
}

#' species_temporal Server Functions
#'
#' @noRd 
mod_species_temporal_server <- function(id, target_area, app_values){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      available_species = NULL,
      available_years = NULL,
      available_sources = NULL,
      preview_data = NULL,
      preview_loaded = FALSE
    )
    
    # Update UI choices when datasets are loaded
    observe({
      req(app_values$datasets_loaded)
      
      cli::cli_alert_info("Using pre-loaded species and years data")
      
      # Get species data from app_values
      species_list <- app_values$available_species
      species_choices <- setNames(species_list, species_list)
      years <- app_values$available_years
      
      # Get available data sources
      if (!is.null(app_values$all_data) && "source" %in% names(app_values$all_data)) {
        sources <- unique(app_values$all_data$source)
        sources <- sources[!is.na(sources)]
        source_choices <- setNames(sources, sources)
      } else {
        source_choices <- NULL
      }
      
      values$available_species <- species_choices
      values$available_years <- years
      values$available_sources <- source_choices
      
      # Update UI choices
      updateSelectizeInput(
        session, "selected_species",
        choices = species_choices,
        server = TRUE
      )
      
      if (!is.null(years)) {
        updateSliderInput(
          session, "selected_years",
          min = min(years),
          max = max(years),
          value = c(min(years), max(years))  # Select all years by default
        )
      }
      
      # Update datasource choices
      if (!is.null(source_choices)) {
        updateSelectizeInput(
          session, "selected_sources",
          choices = source_choices,
          selected = source_choices,  # Select all sources by default
          server = TRUE
        )
      }
      
      cli::cli_alert_success("Updated UI with {length(species_list)} species, {length(years)} years, and {length(source_choices %||% 0)} data sources")
    })
    
    
    
    # Reactive data filtering based on user inputs
    filtered_data <- reactive({
      req(input$selected_years)
      req(app_values$all_data)
      
      tryCatch({
        # Use pre-loaded data
        all_data <- app_values$all_data
        
        # Filter by spatial extent only if target area is available
        if (!is.null(target_area())) {
          bounds <- sf::st_bbox(target_area()$geometry)
          spatial_filtered <- all_data[
            all_data$longitude >= bounds[1] & 
            all_data$longitude <= bounds[3] &
            all_data$latitude >= bounds[2] & 
            all_data$latitude <= bounds[4],
          ]
        } else {
          # Use all data if no target area is selected
          spatial_filtered <- all_data
        }
        
        # Filter by years
        if ("date" %in% names(spatial_filtered)) {
          date_col <- as.Date(spatial_filtered$date)
          year_col <- as.numeric(format(date_col, "%Y"))
          year_range <- seq(input$selected_years[1], input$selected_years[2])
          temporal_filtered <- spatial_filtered[year_col %in% year_range, ]
          
          # Filter by yearly period
          month_day <- format(date_col, "%m-%d")
          period_start <- format(input$yearly_period[1], "%m-%d")
          period_end <- format(input$yearly_period[2], "%m-%d")
          
          if (period_start <= period_end) {
            # Normal period (e.g., 03-01 to 08-31)
            period_filtered <- temporal_filtered[
              format(as.Date(temporal_filtered$date), "%m-%d") >= period_start &
              format(as.Date(temporal_filtered$date), "%m-%d") <= period_end,
            ]
          } else {
            # Cross-year period (e.g., 11-01 to 02-28)
            period_filtered <- temporal_filtered[
              format(as.Date(temporal_filtered$date), "%m-%d") >= period_start |
              format(as.Date(temporal_filtered$date), "%m-%d") <= period_end,
            ]
          }
        } else {
          period_filtered <- spatial_filtered
        }
        
        # Filter by species
        if (input$species_method == "individual" && 
            !is.null(input$selected_species) && 
            length(input$selected_species) > 0) {
          species_filtered <- period_filtered[period_filtered$code_id %in% input$selected_species, ]
        } else if (input$species_method == "group") {
          # This would need to be implemented based on your species grouping logic
          species_filtered <- period_filtered
        } else {
          species_filtered <- period_filtered
        }
        
        # Filter by data sources
        if (!is.null(input$selected_sources) && 
            length(input$selected_sources) > 0 && 
            "source" %in% names(species_filtered)) {
          final_filtered <- species_filtered[species_filtered$source %in% input$selected_sources, ]
        } else {
          final_filtered <- species_filtered
        }
        
        return(final_filtered)
        
      }, error = function(e) {
        cli::cli_alert_danger("Error filtering data: {e$message}")
        return(data.frame())
      })
    })
    
    # Update values when filtered data changes
    observe({
      data <- filtered_data()
      values$preview_data <- data
      values$preview_loaded <- TRUE
      
      if (nrow(data) > 0) {
        cli::cli_alert_success("Data filtered: {nrow(data)} records")
      }
    })
    
    
    # Render observation table
    output$obs_table <- reactable::renderReactable({
      data <- filtered_data()
      
      # Check if data is empty or NULL
      if (is.null(data) || nrow(data) == 0) {
        cli::cli_alert_warning("No data available for reactable")
        return(reactable::reactable(
          data.frame(Message = "Aucune donnée disponible à afficher"),
          columns = list(Message = reactable::colDef(name = ""))
        ))
      }
      
      # Check for required columns
      required_cols <- c("date", "code_id", "abondance", "obs", "inv_type", "source", "colony")
      missing_cols <- setdiff(required_cols, names(data))
      
      if (length(missing_cols) > 0) {
        cli::cli_alert_warning("Missing columns for reactable: {paste(missing_cols, collapse = ', ')}")
        return(reactable::reactable(
          data.frame(Erreur = paste("Colonnes requises manquantes :", paste(missing_cols, collapse = ", "))),
          columns = list(Erreur = reactable::colDef(name = "Erreur"))
        ))
      }
      
      # Select relevant columns for display
      tryCatch({
        display_data <- data |>
          dplyr::select(
            Date = date,
            Espèce = code_id,
            Abondance = abondance,
            Observateur = obs,
            Type_inv = inv_type,
            Source = source,
            Colonie = colony
          ) |>
          dplyr::arrange(dplyr::desc(Date))
      }, error = function(e) {
        cli::cli_alert_danger("Error processing data for reactable: {e$message}")
        return(reactable::reactable(
          data.frame(Erreur = paste("Erreur de traitement des données :", e$message)),
          columns = list(Erreur = reactable::colDef(name = "Erreur"))
        ))
      })
      
      reactable::reactable(
        display_data,
        defaultPageSize = 50,
        searchable = TRUE,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100),
        columns = list(
          Abondance = reactable::colDef(
            format = reactable::colFormat(digits = 0),
            minWidth = 80
          ),
          Date = reactable::colDef(
            format = reactable::colFormat(date = TRUE),
            minWidth = 100
          ),
          Espèce = reactable::colDef(
            minWidth = 80
          ),
          Observateur = reactable::colDef(
            minWidth = 80
          ),
          Type_inv = reactable::colDef(
            name = "Type inv.",
            minWidth = 80
          ),
          Source = reactable::colDef(
            minWidth = 80
          ),
          Colonie = reactable::colDef(
            minWidth = 80,
            align = "center",
            cell = function(value) {
              if (is.na(value)) {
                ""
              } else if (isTRUE(value) || value == TRUE || value == 1 || tolower(as.character(value)) %in% c("true", "yes", "1")) {
                htmltools::tags$span(style = "color: green; font-weight: bold;", "✓")
              } else {
                htmltools::tags$span(style = "color: red; font-weight: bold;", "✗")
              }
            }
          )
        ),
        theme = reactable::reactableTheme(
          stripedColor = "#f8f9fa"
        )
      )
    })
    
    
    
    
    # Return the selection for use by other modules (auto-validated)
    return(reactive({
      req(input$selected_years)
      req(values$preview_data)
      
      list(
        species_method = input$species_method,
        selected_species = if (input$species_method == "individual") input$selected_species else NULL,
        species_group = if (input$species_method == "group") input$species_group else NULL,
        selected_years = seq(input$selected_years[1], input$selected_years[2]),
        yearly_period = input$yearly_period,
        selected_sources = input$selected_sources,
        preview_data = values$preview_data
      )
    }))
  })
}
