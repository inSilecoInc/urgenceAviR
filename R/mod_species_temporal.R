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
        p("Sélectionnez les espèces cibles, définissez les filtres temporels et les sources de données")
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
                  plugins = list('remove_button')
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
            "Sélectionnez la période annuelle :",
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
              style = "position: relative; z-index:1000;",
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
            )
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
mod_species_temporal_server <- function(id, app_values){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize filtered_df when datasets are loaded
    observe({
      req(app_values$all_df)

      cli::cli_alert_info("Initializing filtered df from app_values")

      # Initialize filtered_df with all available df
      app_values$filtered_df <- app_values$all_df

      # Get metadata for UI updates
      all_df <- app_values$all_df
      species <- sort(unique(all_df$code_id))
      years <- sort(unique(as.numeric(format(as.Date(all_df$date), "%Y")), na.rm = TRUE))
      sources <- sort(unique(all_df$source))

      # Update UI choices
      updateSelectizeInput(
        session, "selected_species",
        choices = species,
        server = TRUE
      )

      updateSliderInput(
        session, "selected_years",
        min = min(years),
        max = max(years),
        value = c(min(years), max(years))
      )

      updateSelectizeInput(
        session, "selected_sources",
        choices = sources,
        selected = sources,
        server = TRUE
      )

      cli::cli_alert_success("Updated UI with {length(species)} species, {length(years)} years, and {length(sources)} df sources")
    })
    
    # observeEvent for species selection changes
    observeEvent(input$selected_species, {
      req(app_values$all_df)
      cli::cli_alert_info("Species selection changed: {length(input$selected_species)} species selected")

      app_values$selected_species <- if (input$species_method == "individual") input$selected_species else NULL

      # Apply species filter
      df <- app_values$all_df

      if (input$species_method == "individual" && length(input$selected_species) > 0) {
        df <- df |> dplyr::filter(code_id %in% input$selected_species)
        cli::cli_alert_info("Filtered to {nrow(df)} observations for selected species")
      }

      app_values$filtered_df <- df
    }, ignoreInit = TRUE)

    # observeEvent for df sources selection changes
    observeEvent(input$selected_sources, {
      req(app_values$filtered_df)
      cli::cli_alert_info("df sources selection changed: {length(input$selected_sources)} sources selected")

      app_values$selected_sources <- input$selected_sources

      # Apply sources filter
      df <- app_values$filtered_df

      if (length(input$selected_sources) > 0) {
        df <- df |> dplyr::filter(source %in% input$selected_sources)
        cli::cli_alert_info("Filtered to {nrow(df)} observations for selected sources")
      }

      app_values$filtered_df <- df
    }, ignoreInit = TRUE)

    # observeEvent for species method changes
    observeEvent(input$species_method, {
      req(app_values$all_df)
      cli::cli_alert_info("Species method changed to: {input$species_method}")

      # Clear species selection when method changes from individual
      if (input$species_method != "individual") {
        updateSelectizeInput(session, "selected_species", selected = character(0))
      }

      app_values$species_method <- input$species_method

      # Apply species method filter
      df <- app_values$all_df

      if (input$species_method == "individual" && length(input$selected_species) > 0) {
        df <- df |> dplyr::filter(code_id %in% input$selected_species)
        cli::cli_alert_info("Filtered to {nrow(df)} observations for species method")
      }

      app_values$filtered_df <- df
    }, ignoreInit = TRUE)

    # observeEvent for year selection changes
    observeEvent(input$selected_years, {
      req(app_values$filtered_df)
      cli::cli_alert_info("Year selection changed: {input$selected_years[1]} to {input$selected_years[2]}")

      app_values$selected_years <- seq(input$selected_years[1], input$selected_years[2])

      # Apply year filter
      df <- app_values$filtered_df
      df <- df |> dplyr::filter(lubridate::year(date) %in% app_values$selected_years)
      cli::cli_alert_info("Filtered to {nrow(df)} observations for selected years")

      app_values$filtered_df <- df
    }, ignoreInit = TRUE)

    # observeEvent for yearly period changes
    observeEvent(input$yearly_period, {
      req(app_values$filtered_df)
      cli::cli_alert_info("Yearly period changed")

      app_values$yearly_period <- input$yearly_period

      # Apply yearly period filter
      df <- app_values$filtered_df

      start_day <- format(input$yearly_period[1], "%m-%d")
      end_day <- format(input$yearly_period[2], "%m-%d")

      df <- df |>
        dplyr::filter(
          format(date, "%m-%d") >= start_day,
          format(date, "%m-%d") <= end_day
        )
      cli::cli_alert_info("Filtered to {nrow(df)} observations for yearly period")

      app_values$filtered_df <- df
    }, ignoreInit = TRUE)
    
    # Render observation table
    output$obs_table <- reactable::renderReactable({
      df <- app_values$filtered_df

      # Check if df is empty or NULL
      if (is.null(df) || nrow(df) == 0) {
        cli::cli_alert_warning("No df available for reactable")
        return(reactable::reactable(
          data.frame(Message = "Aucune donnée disponible"),
          columns = list(Message = reactable::colDef(name = ""))
        ))
      }

      # Clean UTF-8 encoding and select relevant columns
      display_data <- df |>
        dplyr::mutate(
          dplyr::across(
            dplyr::where(is.character),
            ~ iconv(.x, from = "UTF-8", to = "UTF-8", sub = "")
          )
        ) |>
        dplyr::select(date, code_id, abondance, obs, inv_type, source)

      reactable::reactable(
        display_data,
        defaultPageSize = 50,
        searchable = TRUE,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100),
        columns = list(
          date = reactable::colDef(
            name = "Date",
            format = reactable::colFormat(date = TRUE, locales = "en-CA"),
            minWidth = 100
          ),
          code_id = reactable::colDef(
            name = "Espèce",
            minWidth = 80
          ),
          abondance = reactable::colDef(
            name = "Abondance",
            format = reactable::colFormat(digits = 0),
            minWidth = 80
          ),
          obs = reactable::colDef(
            name = "Observateur",
            minWidth = 80
          ),
          inv_type = reactable::colDef(
            name = "Type inv.",
            minWidth = 80
          ),
          source = reactable::colDef(
            name = "Source",
            minWidth = 80
          )
        )
      )
    })
    
  })
}
