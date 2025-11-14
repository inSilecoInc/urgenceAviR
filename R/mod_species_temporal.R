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
        width = 6,
        h3("Étape 2 : Sélection des espèces et des périodes temporelles", class = "text-primary"),
        p("Sélectionnez les espèces cibles, définissez les filtres temporels et les sources de données")
      ),
      column(
        width = 6,
        class = "d-flex align-items-center justify-content-end",
        div(
          style = "padding-top: 10px;",
          actionButton(
            ns("reset_filters"),
            HTML("<i class='fa fa-undo'></i> &nbsp;Réinitialiser"),
            class = "btn-secondary me-2"
          ),
          actionButton(
            ns("lock_filters"),
            HTML("Verrouiller les filtres &nbsp;<i class='fa fa-arrow-right'></i>"),
            class = "btn-success"
          )
        )
      )
    ),

    fluidRow(
      column(
        width = 4,
        bslib::card(
          bslib::card_header(h5("Sélection des espèces")),
          bslib::card_body(
            fluidRow(
              column(
                width = 6,
                radioButtons(
                  ns("species_method"),
                  "Méthode de sélection :",
                  choices = list(
                    "Toutes les espèces" = "all",
                    "Espèces spécifiques" = "individual",
                    "Groupe fonctionnel" = "functional_group",
                    "Milieu" = "habitat"
                  ),
                  selected = "all"
                )
              ),

              column(
                width = 6,
                conditionalPanel(
                  condition = "input.species_method == 'functional_group'",
                  ns = ns,
                  div(
                    style = "position: relative;",
                    selectizeInput(
                      ns("functional_groups"),
                      div(
                          actionButton(
                            ns("info_functional_group"),
                            label = NULL,
                            icon = icon("info-circle"),
                            class = "btn btn-link",
                            style = "padding: 0; margin-left: 5px; vertical-align: baseline;"
                          ),
                        "Choisissez les groupes fonctionnels:"
                      ),
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        plugins = list("remove_button"),
                        dropdownParent = "body"
                      )
                    )
                  )
                ),

                conditionalPanel(
                  condition = "input.species_method == 'habitat'",
                  ns = ns,
                  div(
                    style = "position: relative;",
                    selectizeInput(
                      ns("habitats"),
                      div(
                        actionButton(
                          ns("info_habitat"),
                          label = NULL,
                          icon = icon("info-circle"),
                          class = "btn btn-link",
                          style = "padding: 0; margin-left: 5px; vertical-align: baseline;"
                        ),
                        "Choisissez les milieux :"
                      ),
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        plugins = list("remove_button"),
                        dropdownParent = "body"
                      )
                    )
                  )
                ),

                conditionalPanel(
                  condition = "input.species_method == 'individual'",
                  ns = ns,
                  selectizeInput(
                    ns("selected_species"),
                    "Choisissez les espèces :",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(
                      plugins = list('remove_button'),
                      dropdownParent = "body"
                    )
                  )
                )
              )
            )
          )
        )
      ),

      column(
        width = 4,
        bslib::card(
          bslib::card_header(h5("Filtrage temporel")),
          bslib::card_body(
            fluidRow(
              column(
                width = 6,
                sliderInput(
                  ns("selected_years"),
                  "Sélectionnez la plage d'années :",
                  min = 2000,
                  max = 2024,
                  value = c(2019, 2024),
                  step = 1,
                  sep = ""
                )
              ),

              column(
                width = 6,
                dateRangeInput(
                  ns("yearly_period"),
                  "Sélectionnez la période annuelle :",
                  start = "2024-01-01",
                  end = "2024-12-31",
                  language="fr",
                  format = "dd-mm",
                  startview = "month"
                ),

                helpText("Ceci définit la fenêtre temporelle à l'intérieur de chaque année.")
              )
            )
          )
        )
      ),

      column(
        width = 4,
        bslib::card(
          bslib::card_header(h5("Source des données")),
          bslib::card_body(
            div(
              selectizeInput(
                ns("selected_sources"),
                "Sélectionnez les sources de données :",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Toutes les sources sélectionnées par défaut...",
                  plugins = list('remove_button'),
                  dropdownParent = "body"
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
          bslib::card_header(h5("Tableau de données")),
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

    # Load taxonomy data
    taxo_vuln <- reactive({
      tryCatch({
        readr::read_delim(
          system.file("exdata/Taxonomy/taxo_vulnerabilite.csv", package = "urgenceAviR"),
          delim = ";",
          locale = readr::locale(encoding = "UTF-8"),
          show_col_types = FALSE
        )
      }, error = function(e) {
        cli::cli_alert_warning("Could not load taxonomy file: {e$message}")
        NULL
      })
    })

    # Load Avian Core data for French names
    avian_core <- reactive({
      tryCatch({
        readr::read_delim(
          system.file("exdata/Taxonomy/ECCC_Avian_Core_20241025.csv", package = "urgenceAviR"),
          delim = ";",
          locale = readr::locale(encoding = "UTF-8"),
          show_col_types = FALSE
        )
      }, error = function(e) {
        cli::cli_alert_warning("Could not load Avian Core file: {e$message}")
        NULL
      })
    })

    # Helper function to create species choices with French names
    create_species_choices <- function(species_codes) {
      avian <- avian_core()

      if (!is.null(avian) && length(species_codes) > 0) {
        species_choices <- setNames(species_codes, species_codes)

        # Add French names to labels
        for (sp in species_codes) {
          french_name <- avian$French_Name[avian$Species_ID == sp]
          if (length(french_name) > 0 && !is.na(french_name[1]) && french_name[1] != "") {
            names(species_choices)[species_choices == sp] <- paste0(sp, " - ", french_name[1])
          }
        }
        return(species_choices)
      } else {
        return(species_codes)
      }
    }

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

      # Update UI choices with French names
      updateSelectizeInput(
        session, "selected_species",
        choices = create_species_choices(species),
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

    observe({
      taxo <- taxo_vuln()
      req(taxo)

      functional_groups <- sort(unique(taxo$groupe_fonctionnel))
      habitats <- sort(unique(taxo$milieu_marin))

      updateSelectInput(
        session, "functional_groups",
        choices = functional_groups
      )

      updateSelectInput(
        session, "habitats",
        choices = habitats
      )

      cli::cli_alert_success("Loaded {length(functional_groups)} functional groups and {length(habitats)} habitats from taxonomy")
    })

    # Info button for functional groups
    observeEvent(input$info_functional_group, {
      taxo <- taxo_vuln()
      req(taxo)

      cli::cli_alert_info("Showing functional group taxonomy modal")

      # Prepare data grouped by functional group
      taxo_grouped <- taxo |>
        dplyr::select(groupe_fonctionnel, nomfr, nomla, species_id) |>
        dplyr::arrange(groupe_fonctionnel, nomfr)

      showModal(
        modalDialog(
          title = "Table taxonomique - Groupes fonctionnels",
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Fermer"),
          reactable::reactableOutput(ns("taxo_functional_table"))
        )
      )

      output$taxo_functional_table <- reactable::renderReactable({
        reactable::reactable(
          taxo_grouped,
          filterable = TRUE,
          searchable = TRUE,
          defaultPageSize = 15,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 15, 25, 50),
          striped = TRUE,
          highlight = TRUE,
          columns = list(
            groupe_fonctionnel = reactable::colDef(
              name = "Groupe fonctionnel",
              minWidth = 150,
              filterable = TRUE
            ),
            nomfr = reactable::colDef(
              name = "Nom français",
              minWidth = 150
            ),
            nomla = reactable::colDef(
              name = "Nom latin",
              minWidth = 150,
              style = list(fontStyle = "italic")
            ),
            species_id = reactable::colDef(
              name = "Code",
              minWidth = 80
            )
          ),
          defaultColDef = reactable::colDef(
            align = "left",
            headerStyle = list(background = "#f7f7f8")
          )
        )
      })
    })

    # Info button for habitats
    observeEvent(input$info_habitat, {
      taxo <- taxo_vuln()
      req(taxo)

      cli::cli_alert_info("Showing habitat taxonomy modal")

      # Prepare data grouped by habitat
      taxo_grouped <- taxo |>
        dplyr::select(milieu_marin, nomfr, nomla, species_id) |>
        dplyr::arrange(milieu_marin, nomfr)

      showModal(
        modalDialog(
          title = "Table taxonomique - Milieux",
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Fermer"),
          reactable::reactableOutput(ns("taxo_habitat_table"))
        )
      )

      output$taxo_habitat_table <- reactable::renderReactable({
        reactable::reactable(
          taxo_grouped,
          filterable = TRUE,
          searchable = TRUE,
          defaultPageSize = 15,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 15, 25, 50),
          striped = TRUE,
          highlight = TRUE,
          columns = list(
            milieu_marin = reactable::colDef(
              name = "Milieu marin",
              minWidth = 120,
              filterable = TRUE
            ),
            nomfr = reactable::colDef(
              name = "Nom français",
              minWidth = 150
            ),
            nomla = reactable::colDef(
              name = "Nom latin",
              minWidth = 150,
              style = list(fontStyle = "italic")
            ),
            species_id = reactable::colDef(
              name = "Code",
              minWidth = 80
            )
          ),
          defaultColDef = reactable::colDef(
            align = "left",
            headerStyle = list(background = "#f7f7f8")
          )
        )
      })
    })

    # observeEvent for functional group selection changes
    observeEvent(input$functional_groups, {
      req(app_values$all_df)
      taxo <- taxo_vuln()
      req(taxo)

      cli::cli_alert_info("Functional group selection changed: {length(input$functional_groups)} groups selected")

      if (length(input$functional_groups) > 0) {
        # Get species codes for selected functional groups
        selected_species_codes <- taxo |>
          dplyr::filter(groupe_fonctionnel %in% input$functional_groups) |>
          dplyr::pull(species_id) |>
          unique()

        cli::cli_alert_info("Found {length(selected_species_codes)} species for selected functional groups")

        # Update the species selection UI to show which species are selected
        all_species <- sort(unique(app_values$all_df$code_id))
        updateSelectizeInput(
          session, "selected_species",
          choices = create_species_choices(all_species),
          selected = selected_species_codes,
          server = TRUE
        )

        # Filter data by selected species codes
        app_values$filtered_df <- app_values$all_df |>
          dplyr::filter(code_id %in% selected_species_codes)

        cli::cli_alert_info("Filtered to {nrow(app_values$filtered_df)} observations for selected functional groups")
      } else {
        # If no functional groups selected, reset to all data
        app_values$filtered_df <- app_values$all_df
        updateSelectizeInput(session, "selected_species", selected = character(0))
      }
    }, ignoreInit = TRUE)

    # observeEvent for habitat selection changes
    observeEvent(input$habitats, {
      req(app_values$all_df)
      taxo <- taxo_vuln()
      req(taxo)

      cli::cli_alert_info("Habitat selection changed: {length(input$habitats)} habitats selected")

      if (length(input$habitats) > 0) {
        # Get species codes for selected habitats
        selected_species_codes <- taxo |>
          dplyr::filter(milieu_marin %in% input$habitats) |>
          dplyr::pull(species_id) |>
          unique()

        cli::cli_alert_info("Found {length(selected_species_codes)} species for selected habitats")

        # Update the species selection UI to show which species are selected
        all_species <- sort(unique(app_values$all_df$code_id))
        updateSelectizeInput(
          session, "selected_species",
          choices = create_species_choices(all_species),
          selected = selected_species_codes,
          server = TRUE
        )

        # Filter data by selected species codes
        app_values$filtered_df <- app_values$all_df |>
          dplyr::filter(code_id %in% selected_species_codes)

        cli::cli_alert_info("Filtered to {nrow(app_values$filtered_df)} observations for selected habitats")
      } else {
        # If no habitats selected, reset to all data
        app_values$filtered_df <- app_values$all_df
        updateSelectizeInput(session, "selected_species", selected = character(0))
      }
    }, ignoreInit = TRUE)

    # observeEvent for species selection changes
    observeEvent(input$selected_species, {
      req(app_values$all_df)
      cli::cli_alert_info("Species selection changed: {length(input$selected_species)} species selected")

      app_values$selected_species <- if (input$species_method %in% c("individual", "functional_group", "habitat")) input$selected_species else NULL

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

      # Clear all selection inputs when method changes
      updateSelectizeInput(session, "selected_species", selected = character(0))
      updateSelectizeInput(session, "functional_groups", selected = character(0))
      updateSelectizeInput(session, "habitats", selected = character(0))

      app_values$species_method <- input$species_method

      # Reset filtered data when switching to "all" method
      if (input$species_method == "all") {
        app_values$filtered_df <- app_values$all_df
        cli::cli_alert_info("Species method set to 'all' - showing all {nrow(app_values$all_df)} observations")
      }
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

    # Reset filters button
    observeEvent(input$reset_filters, {
      cli::cli_alert_info("Resetting all filters")

      # Reset species method to "all"
      updateRadioButtons(session, "species_method", selected = "all")

      # Clear all selection inputs
      updateSelectizeInput(session, "selected_species", selected = character(0))
      updateSelectizeInput(session, "functional_groups", selected = character(0))
      updateSelectizeInput(session, "habitats", selected = character(0))

      # Reset temporal filters
      if (!is.null(app_values$all_df)) {
        all_data <- app_values$all_df
        years <- sort(unique(as.numeric(format(as.Date(all_data$date), "%Y")), na.rm = TRUE))
        sources <- sort(unique(all_data$source))

        updateSliderInput(
          session, "selected_years",
          value = c(min(years), max(years))
        )

        updateDateRangeInput(
          session, "yearly_period",
          start = paste0(max(years), "-01-01"),
          end = paste0(max(years), "-12-31")
        )

        # Reset sources to all
        updateSelectizeInput(
          session, "selected_sources",
          selected = sources
        )
      }

      # Reset filtered data to all data
      app_values$filtered_df <- app_values$all_df

      cli::cli_alert_success("All filters reset to default values")
      showNotification("Tous les filtres ont été réinitialisés", type = "message")
    })

    # Lock filters and navigate to grid config
    observeEvent(input$lock_filters, {
      cli::cli_alert_info("Locking filters and navigating to grid configuration")

      # Store the locked filter state
      app_values$filters_locked <- TRUE
      app_values$locked_filtered_df <- app_values$filtered_df

      # Log filter summary
      n_obs <- nrow(app_values$filtered_df)
      n_species <- length(unique(app_values$filtered_df$code_id))
      cli::cli_alert_success("Filters locked: {n_obs} observations, {n_species} species")

      showNotification(
        paste0("Filtres verrouillés : ", n_obs, " observations, ", n_species, " espèces"),
        type = "message"
      )

      # Navigate to grid configuration tab
      app_values$navigate_to_tab <- "grid_config"
    })

    # Render observation table
    output$obs_table <- reactable::renderReactable({
      df <- app_values$filtered_df
      taxo <- taxo_vuln()

      # Check if df is empty or NULL
      if (is.null(df) || nrow(df) == 0) {
        cli::cli_alert_warning("No df available for reactable")
        return(reactable::reactable(
          data.frame(Message = "Aucune donnée disponible"),
          columns = list(Message = reactable::colDef(name = ""))
        ))
      }

      # Join with taxonomy data to get French names and ecological info
      if (!is.null(taxo)) {
        taxo_info <- taxo |>
          dplyr::select(
            code_id = species_id,
            nom_francais = nomfr,
            nom_latin = nomla,
            milieu_marin,
            groupe_fonctionnel
          )

        display_data <- df |>
          dplyr::left_join(taxo_info, by = "code_id") |>
          dplyr::mutate(
            dplyr::across(
              dplyr::where(is.character),
              ~ iconv(.x, from = "UTF-8", to = "UTF-8", sub = "")
            )
          ) |>
          dplyr::select(date, code_id, nom_francais, nom_latin, milieu_marin, groupe_fonctionnel, abondance, obs, inv_type, source)
      } else {
        # Fallback if taxonomy not available
        display_data <- df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::where(is.character),
              ~ iconv(.x, from = "UTF-8", to = "UTF-8", sub = "")
            )
          ) |>
          dplyr::select(date, code_id, abondance, obs, inv_type, source)
      }

      reactable::reactable(
        display_data,
        defaultPageSize = 50,
        searchable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100),
        columns = list(
          date = reactable::colDef(
            name = "Date",
            format = reactable::colFormat(date = TRUE, locales = "en-CA"),
            minWidth = 100
          ),
          code_id = reactable::colDef(
            name = "Code",
            minWidth = 70
          ),
          nom_francais = reactable::colDef(
            name = "Nom français",
            minWidth = 120
          ),
          nom_latin = reactable::colDef(
            name = "Nom latin",
            minWidth = 120,
            style = list(fontStyle = "italic")
          ),
          milieu_marin = reactable::colDef(
            name = "Milieu",
            minWidth = 100
          ),
          groupe_fonctionnel = reactable::colDef(
            name = "Groupe fonctionnel",
            minWidth = 120
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
