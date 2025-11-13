#' figure_generation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_figure_generation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        h3("Étape 4 : Génération de figures", class = "text-primary"),
        p("Générez des visualisations et des sorties d'analyse basées sur vos sélections.")
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("Configuration de l'analyse"),
          
          fluidRow(
            column(
              width = 4,
              selectInput(
                ns("figure_type"),
                "Sélectionnez le type de figure :",
                choices = list(
                  "Carte de distribution des espèces" = "distribution",
                  "Graphique d'abondance temporelle" = "temporal",
                  "Carte de chaleur basée sur la grille" = "heatmap",
                  "Courbe d'accumulation des espèces" = "accumulation",
                  "Indices de diversité" = "diversity"
                ),
                selected = "distribution"
              )
            ),
            column(
              width = 4,
              selectInput(
                ns("output_format"),
                "Format de sortie :",
                choices = list(
                  "Graphique interactif" = "interactive",
                  "PNG statique" = "png",
                  "Rapport PDF" = "pdf"
                ),
                selected = "interactive"
              )
            ),
            column(
              width = 4,
              numericInput(
                ns("figure_width"),
                "Largeur de la figure (pouces) :",
                value = 10,
                min = 5,
                max = 20,
                step = 1
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              h5("Options supplémentaires"),
              checkboxGroupInput(
                ns("figure_options"),
                "Inclure :",
                choices = list(
                  "Légende" = "legend",
                  "Lignes de grille" = "grid",
                  "Barre d'échelle" = "scale",
                  "Flèche du nord" = "north",
                  "Tableau récapitulatif des données" = "table"
                ),
                selected = c("legend", "scale"),
                inline = TRUE
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("Générer les figures"),
          
          actionButton(
            ns("generate_figures"),
            "Générer les figures",
            icon = icon("chart-bar"),
            class = "btn-primary btn-lg",
            disabled = TRUE
          ),
          

          
          div(id = ns("generation_status")),
          
          conditionalPanel(
            condition = "output.figures_generated",
            ns = ns,
            br(),
            div(
              class = "alert alert-success",
              icon("check-circle"), " Figures générées avec succès !",
              br(),
              downloadButton(
                ns("download_figures"),
                "Télécharger les résultats",
                icon = icon("download"),
                class = "btn-success"
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("Aperçu"),
          
          conditionalPanel(
            condition = "output.figures_generated",
            ns = ns,
            tabsetPanel(
              id = ns("preview_tabs"),
              tabPanel(
                "Aperçu de la figure",
                br(),
                plotOutput(ns("figure_preview"), height = "600px")
              ),
              tabPanel(
                "Résumé des données",
                br(),
                DT::dataTableOutput(ns("data_summary"))
              ),
              tabPanel(
                "Journal d'analyse",
                br(),
                verbatimTextOutput(ns("analysis_log"))
              )
            )
          ),
          
          conditionalPanel(
            condition = "!output.figures_generated",
            ns = ns,
            div(
              class = "alert alert-info",
              icon("info-circle"), " Complétez les étapes précédentes et cliquez sur 'Générer les figures' pour voir les résultats ici."
            )
          )
        )
      )
    )
  )
}

#' figure_generation Server Functions
#'
#' @noRd 
mod_figure_generation_server <- function(id, target_area, species_selection){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      figures_generated = FALSE,
      analysis_results = NULL,
      analysis_log = character(0)
    )
    
    # Enable generate button when both target area and species selection are available
    observe({
      if (!is.null(target_area()) && !is.null(species_selection())) {
        shinyjs::enable("generate_figures")
        cli::cli_alert_info("Figure generation ready - both area and species selection available")
      } else {
        shinyjs::disable("generate_figures")
      }
    })
    
    # Add log entry
    add_log <- function(message) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      log_entry <- paste0("[", timestamp, "] ", message)
      values$analysis_log <- c(values$analysis_log, log_entry)
      cli::cli_alert_info(message)
    }
    
    # Generate figures
    observeEvent(input$generate_figures, {
      req(target_area())
      req(species_selection())
      
      cli::cli_alert_info("Starting figure generation process")
      add_log("Starting figure generation process")
      
      # Reset previous results
      values$figures_generated <- FALSE
      values$analysis_results <- NULL
      values$analysis_log <- character(0)
      add_log("Initialized figure generation")
      
      withProgress(message = 'Génération des figures...', value = 0, {
        
        tryCatch({
          incProgress(0.1, detail = "Validating inputs")
          add_log("Validating inputs")
          
          # Get data from previous modules
          area_info <- target_area()
          selection_info <- species_selection()
          
          add_log(paste("Target area:", round(area_info$area_km2, 2), "km²"))
          add_log(paste("Species method:", selection_info$species_method))
          add_log(paste("Selected years:", paste(selection_info$selected_years, collapse = ", ")))
          add_log(paste("Grid size:", selection_info$grid_size, "km"))
          
          incProgress(0.2, detail = "Preparing data")
          add_log("Preparing analysis data")
          
          # Use preview data from species selection module
          analysis_data <- selection_info$preview_data
          
          if (is.null(analysis_data) || nrow(analysis_data) == 0) {
            stop("No data available for analysis")
          }
          
          add_log(paste("Analysis dataset contains", nrow(analysis_data), "records"))
          add_log(paste("Species included:", length(unique(analysis_data$code_id))))
          
          incProgress(0.4, detail = "Configuring analysis")
          add_log(paste("Figure type:", input$figure_type))
          add_log(paste("Output format:", input$output_format))
          
          # Simulate analysis process (replace with actual analysis)
          incProgress(0.6, detail = "Running analysis")
          add_log("Running spatial analysis")
          Sys.sleep(1)  # Simulate processing time
          
          incProgress(0.8, detail = "Generating visualizations")
          add_log("Generating visualizations")
          
          # Create a simple placeholder plot
          # In a real implementation, this would generate the actual figures
          plot_data <- data.frame(
            x = 1:10,
            y = runif(10, 1, 10),
            category = rep(c("A", "B"), 5)
          )
          
          incProgress(0.9, detail = "Finalizing outputs")
          add_log("Finalizing outputs")
          
          # Store results
          values$analysis_results <- list(
            plot_data = plot_data,
            summary_stats = data.frame(
              Métrique = c("Enregistrements totaux", "Espèces uniques", "Cellules de grille", "Date d'analyse"),
              Value = c(
                nrow(analysis_data),
                length(unique(analysis_data$code_id)),
                "TBD",  # Would be calculated in real analysis
                format(Sys.Date(), "%Y-%m-%d")
              )
            ),
            figure_type = input$figure_type,
            parameters = list(
              grid_size = selection_info$grid_size,
              min_observations = selection_info$min_observations,
              area_km2 = area_info$area_km2
            )
          )
          
          values$figures_generated <- TRUE
          
          incProgress(1, detail = "Complete")
          add_log("Figure generation completed successfully")
          
          cli::cli_alert_success("Figures generated successfully")
          showNotification("Figures générées avec succès !", type = "message")
          
        }, error = function(e) {
          add_log(paste("ERROR:", e$message))
          cli::cli_alert_danger("Error generating figures: {e$message}")
          showNotification(paste("Erreur de génération des figures :", e$message), type = "error")
        })
      })
    })
    
    # Figure preview
    output$figure_preview <- renderPlot({
      req(values$analysis_results)
      
      add_log("Rendering figure preview")
      
      # Create a placeholder plot based on figure type
      if (input$figure_type == "distribution") {
        # Species distribution map placeholder
        plot(1:10, runif(10), 
             main = "Carte de distribution des espèces (Placeholder)",
             xlab = "Longitude", ylab = "Latitude",
             pch = 16, col = "blue")
        if ("legend" %in% input$figure_options) {
          legend("topright", legend = "Observations", pch = 16, col = "blue")
        }
        
      } else if (input$figure_type == "temporal") {
        # Temporal plot placeholder
        dates <- seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month")
        abundance <- cumsum(rnorm(length(dates), 0, 1)) + 100
        plot(dates, abundance, type = "l", 
             main = "Abondance temporelle (Placeholder)",
             xlab = "Date", ylab = "Abondance")
        
      } else if (input$figure_type == "heatmap") {
        # Heatmap placeholder
        matrix_data <- matrix(runif(100), nrow = 10)
        image(matrix_data, 
              main = "Carte de chaleur basée sur la grille (Placeholder)",
              xlab = "Grille X", ylab = "Grille Y")
        
      } else {
        # Default plot
        plot(values$analysis_results$plot_data$x, 
             values$analysis_results$plot_data$y,
             main = paste(tools::toTitleCase(input$figure_type), "(Placeholder)"),
             xlab = "X", ylab = "Y")
      }
    })
    
    # Data summary table
    output$data_summary <- DT::renderDataTable({
      req(values$analysis_results)
      
      add_log("Rendering data summary table")
      
      DT::datatable(
        values$analysis_results$summary_stats,
        options = list(
          dom = 't',
          pageLength = -1
        ),
        rownames = FALSE
      )
    })
    
    # Analysis log
    output$analysis_log <- renderText({
      paste(values$analysis_log, collapse = "\n")
    })
    
    # Download handler (placeholder)
    output$download_figures <- downloadHandler(
      filename = function() {
        paste0("urgenceAviR_analysis_", Sys.Date(), ".zip")
      },
      content = function(file) {
        add_log("Preparing download package")
        
        # Create temporary directory
        temp_dir <- tempdir()
        
        # Save a placeholder file
        writeLines(
          c("UrgenceAviR Analysis Results",
            paste("Generated on:", Sys.time()),
            "",
            "This is a placeholder download.",
            "In a real implementation, this would contain:",
            "- Generated figures",
            "- Analysis results",
            "- Data summaries",
            "- Metadata"),
          file.path(temp_dir, "README.txt")
        )
        
        # Create zip file
        utils::zip(file, file.path(temp_dir, "README.txt"))
        
        add_log("Download package created")
      }
    )
    
    # Output for conditional panels
    output$figures_generated <- reactive({
      values$figures_generated
    })
    outputOptions(output, "figures_generated", suspendWhenHidden = FALSE)
    
    # Return analysis results for potential use by other components
    return(reactive({
      if (values$figures_generated) {
        values$analysis_results
      } else {
        NULL
      }
    }))
  })
}
