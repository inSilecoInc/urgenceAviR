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
        h3("\u00c9tape 2 : S\u00e9lection des esp\u00e8ces et des p\u00e9riodes temporelles", class = "text-primary"),
        p("S\u00e9lectionnez les esp\u00e8ces cibles, d\u00e9finissez les filtres temporels et les sources de donn\u00e9es")
      ),
      column(
        width = 6,
        class = "d-flex align-items-center justify-content-end",
        div(
          style = "padding-top: 10px;",
          actionButton(
            ns("reset_filters"),
            HTML("<i class='fa fa-undo'></i> &nbsp;R\u00e9initialiser"),
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
          bslib::card_header(h5("S\u00e9lection des esp\u00e8ces")),
          bslib::card_body(
            fluidRow(
              column(
                width = 6,
                radioButtons(
                  ns("species_method"),
                  "M\u00e9thode de s\u00e9lection :",
                  choices = list(
                    "Toutes les esp\u00e8ces" = "all",
                    "Esp\u00e8ces sp\u00e9cifiques" = "individual",
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
                    "Choisissez les esp\u00e8ces :",
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
                  "S\u00e9lectionnez la plage d'ann\u00e9es :",
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
                  "S\u00e9lectionnez la p\u00e9riode annuelle :",
                  start = "2024-01-01",
                  end = "2024-12-31",
                  language="fr",
                  format = "dd-mm",
                  startview = "month"
                ),

                helpText("Ceci d\u00e9finit la fen\u00eatre temporelle \u00e0 l'int\u00e9rieur de chaque ann\u00e9e.")
              )
            )
          )
        )
      ),

      column(
        width = 4,
        bslib::card(
          bslib::card_header(h5("Source des donn\u00e9es")),
          bslib::card_body(
            div(
              selectizeInput(
                ns("selected_sources"),
                "S\u00e9lectionnez les sources de donn\u00e9es :",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Toutes les sources s\u00e9lectionn\u00e9es par d\u00e9faut...",
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
          bslib::card_header(h5("Tableau de donn\u00e9es")),
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
      req(app_values$spatially_filtered_data)

      cli::cli_alert_info("Initializing filtered df from spatially_filtered_data")

      # Initialize filtered_df with spatially filtered data
      app_values$filtered_df <- app_values$spatially_filtered_data

      # Get metadata for UI updates
      all_df <- app_values$spatially_filtered_data
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

    # Initialize taxonomy info module
    taxonomy_info <- mod_taxonomy_info_server("taxonomy_info", taxo_vuln)

    # Info button for functional groups
    observeEvent(input$info_functional_group, {
      taxonomy_info$show_functional_group_table()
    })

    # Info button for habitats
    observeEvent(input$info_habitat, {
      taxonomy_info$show_habitat_table()
    })

    # observeEvent for functional group selection changes
    observeEvent(input$functional_groups, {
      req(app_values$spatially_filtered_data)
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
        all_species <- sort(unique(app_values$spatially_filtered_data$code_id))
        updateSelectizeInput(
          session, "selected_species",
          choices = create_species_choices(all_species),
          selected = selected_species_codes,
          server = TRUE
        )

        # Filter data by selected species codes
        app_values$filtered_df <- app_values$spatially_filtered_data |>
          dplyr::filter(code_id %in% selected_species_codes)

        cli::cli_alert_info("Filtered to {nrow(app_values$filtered_df)} observations for selected functional groups")
      } else {
        # If no functional groups selected, reset to spatially filtered data
        app_values$filtered_df <- app_values$spatially_filtered_data
        updateSelectizeInput(session, "selected_species", selected = character(0))
      }
    }, ignoreInit = TRUE)

    # observeEvent for habitat selection changes
    observeEvent(input$habitats, {
      req(app_values$spatially_filtered_data)
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
        all_species <- sort(unique(app_values$spatially_filtered_data$code_id))
        updateSelectizeInput(
          session, "selected_species",
          choices = create_species_choices(all_species),
          selected = selected_species_codes,
          server = TRUE
        )

        # Filter data by selected species codes
        app_values$filtered_df <- app_values$spatially_filtered_data |>
          dplyr::filter(code_id %in% selected_species_codes)

        cli::cli_alert_info("Filtered to {nrow(app_values$filtered_df)} observations for selected habitats")
      } else {
        # If no habitats selected, reset to spatially filtered data
        app_values$filtered_df <- app_values$spatially_filtered_data
        updateSelectizeInput(session, "selected_species", selected = character(0))
      }
    }, ignoreInit = TRUE)

    # observeEvent for species selection changes
    observeEvent(input$selected_species, {
      req(app_values$spatially_filtered_data)
      cli::cli_alert_info("Species selection changed: {length(input$selected_species)} species selected")

      app_values$selected_species <- if (input$species_method %in% c("individual", "functional_group", "habitat")) input$selected_species else NULL

      # Apply species filter
      df <- app_values$spatially_filtered_data

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
      req(app_values$spatially_filtered_data)
      cli::cli_alert_info("Species method changed to: {input$species_method}")

      # Clear all selection inputs when method changes
      updateSelectizeInput(session, "selected_species", selected = character(0))
      updateSelectizeInput(session, "functional_groups", selected = character(0))
      updateSelectizeInput(session, "habitats", selected = character(0))

      app_values$species_method <- input$species_method

      # Reset filtered data when switching to "all" method
      if (input$species_method == "all") {
        app_values$filtered_df <- app_values$spatially_filtered_data
        cli::cli_alert_info("Species method set to 'all' - showing all {nrow(app_values$spatially_filtered_data)} observations")
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
      if (!is.null(app_values$spatially_filtered_data)) {
        all_data <- app_values$spatially_filtered_data
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

      # Reset filtered data to spatially filtered data
      app_values$filtered_df <- app_values$spatially_filtered_data

      cli::cli_alert_success("All filters reset to default values")
      showNotification("Tous les filtres ont \u00e9t\u00e9 r\u00e9initialis\u00e9s", type = "message")
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
        paste0("Filtres verrouill\u00e9s : ", n_obs, " observations, ", n_species, " esp\u00e8ces"),
        type = "message"
      )

      # Navigate to make grid tab
      app_values$navigate_to_tab <- "make_grid"
    })

    # Render observation table
    output$obs_table <- reactable::renderReactable({
      df <- app_values$filtered_df
      taxo <- taxo_vuln()

      # Check if df is empty or NULL
      if (is.null(df) || nrow(df) == 0) {
        cli::cli_alert_warning("No df available for reactable")
        return(reactable::reactable(
          data.frame(Message = "Aucune donn\u00e9e disponible"),
          columns = list(Message = reactable::colDef(name = ""))
        ))
      }

      # Join with avian_core to get French and Latin names, then taxo for ecological info
      avian <- avian_core()

      if (!is.null(avian)) {
        # Prepare avian core data
        avian_info <- avian |>
          dplyr::select(
            code_id = Species_ID,
            nom_francais = French_Name,
            nom_latin = Scientific_Name
          )

        # Join with avian core first
        display_data <- df |>
          dplyr::left_join(avian_info, by = "code_id")

        # Then join with taxo for milieu_marin and groupe_fonctionnel
        if (!is.null(taxo)) {
          taxo_info <- taxo |>
            dplyr::select(
              code_id = species_id,
              milieu_marin,
              groupe_fonctionnel
            )

          display_data <- display_data |>
            dplyr::left_join(taxo_info, by = "code_id")
        }

        # Merge French and Latin names in the same cell and reorder columns
        display_data <- display_data |>
          dplyr::mutate(
            nom_espece = paste0(nom_francais, " <br><i>", nom_latin, "</i>"),
            dplyr::across(
              dplyr::where(is.character),
              ~ iconv(.x, from = "UTF-8", to = "UTF-8", sub = "")
            )
          ) |>
          dplyr::select(
            date, milieu_marin, code_id, nom_espece, groupe_fonctionnel,
            obs, abondance, inv_type, source
          )
      } else {
        # Fallback if avian core not available
        display_data <- df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::where(is.character),
              ~ iconv(.x, from = "UTF-8", to = "UTF-8", sub = "")
            )
          ) |>
          dplyr::select(code_id, obs, abondance, inv_type, source)
      }

      reactable::reactable(
        display_data,
        defaultPageSize = 10,
        searchable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100),
        language = reactable::reactableLang(
          searchPlaceholder = "Rechercher...",
          searchLabel = "Rechercher",
          noData = "Aucune donn\u00e9e disponible",
          pageSizeOptions = "Afficher {rows}",
          pageInfo = "{rowStart} \u00e0 {rowEnd} sur {rows} entr\u00e9es",
          pagePrevious = "Pr\u00e9c\u00e9dent",
          pageNext = "Suivant",
          pagePreviousLabel = "Page pr\u00e9c\u00e9dente",
          pageNextLabel = "Page suivante"
        ),
        columns = list(
          date = reactable::colDef(
            name = "Date",
            format = reactable::colFormat(date = TRUE, locales = "en-CA"),
            minWidth = 100
          ),
          milieu_marin = reactable::colDef(
            name = "Milieu",
            minWidth = 100
          ),
          code_id = reactable::colDef(
            name = "Code",
            minWidth = 70
          ),
          nom_espece = reactable::colDef(
            name = "Esp\u00e8ce",
            minWidth = 200,
            html = TRUE
          ),
          groupe_fonctionnel = reactable::colDef(
            name = "Groupe fonctionnel",
            minWidth = 120
          ),
          obs = reactable::colDef(
            name = "Observateur",
            minWidth = 80
          ),
          abondance = reactable::colDef(
            name = "Abondance",
            format = reactable::colFormat(digits = 0),
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
