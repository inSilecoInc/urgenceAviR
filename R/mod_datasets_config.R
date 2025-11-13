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

    # Reactive values
    values <- reactiveValues(
      folder_configured = !is.null(datasets_folder()),
      selected_folder_path = NULL
    )

    # Initialize app_values
    app_values$datasets_folder_configured <- !is.null(datasets_folder())
    
    # Initialize shinyFiles for directory selection
    volumes <- c(Home = fs::path_home(), "Root" = "/")
    
    # Check if datasets folder needs to be configured on startup
    observe({
      if (is.null(datasets_folder())) {
        cli::cli_alert_warning("Datasets folder not set, showing configuration modal")
        show_config_modal()
      } else {
        values$folder_configured <- TRUE
      }
    })
    
    # Function to show configuration modal
    show_config_modal <- function() {
      showModal(
        modalDialog(
          title = "Configurer le dossier de données",
          size = "m",
          
          p("Bienvenue dans UrgenceAviR ! Veuillez définir le chemin vers votre dossier de données pour continuer."),
          
          shinyFiles::shinyDirButton(
            ns("datasets_folder_input"),
            "Choisir le dossier de données",
            "Sélectionnez le dossier contenant vos données",
            icon = icon("folder-open"),
            class = "btn-outline-primary"
          ),
          
          br(), br(),
          htmlOutput(ns("selected_folder_display")),
          
          helpText("Ce devrait être le dossier contenant des fichiers comme eBird.gdb, ConsultationCanardsDeMer.csv, etc."),
          
          footer = tagList(
            actionButton(
              ns("confirm_datasets_folder"),
              "Définir le dossier et continuer",
              class = "btn-primary"
            )
          ),
          easyClose = FALSE
        )
      )
    }
    
    # Observe folder selection
    shinyFiles::shinyDirChoose(input, "datasets_folder_input", roots = volumes)
    
    observeEvent(input$datasets_folder_input, {
      if (!is.null(input$datasets_folder_input) && !is.integer(input$datasets_folder_input)) {
        folder_selected <- shinyFiles::parseDirPath(volumes, input$datasets_folder_input)
        if (length(folder_selected) > 0) {
          values$selected_folder_path <- as.character(folder_selected)
          cli::cli_alert_info("Folder selected: {folder_selected}")
        }
      }
    })
    
    # Display selected folder
    output$selected_folder_display <- renderUI({
      if (!is.null(values$selected_folder_path)) {
        p(
          strong("Dossier sélectionné : "),
          code(values$selected_folder_path),
          class = "text-success"
        )
      } else {
        p(
          em("Aucun dossier sélectionné"),
          class = "text-muted"
        )
      }
    })
    
    # Confirm datasets folder setting
    observeEvent(input$confirm_datasets_folder, {
      folder_path <- values$selected_folder_path
      
      if (is.null(folder_path) || length(folder_path) == 0) {
        showNotification("Veuillez d'abord sélectionner un dossier", type = "error")
        return()
      }
      
      if (!dir.exists(folder_path)) {
        showNotification("Le dossier sélectionné n'existe pas. Veuillez choisir un autre dossier.", type = "error")
        return()
      }
      
      tryCatch({
        # Set the datasets folder
        set_datasets_folder(folder_path)
        values$folder_configured <- TRUE
        app_values$datasets_folder_configured <- TRUE
        app_values$datasets_folder_path <- folder_path

        cli::cli_alert_success("Datasets folder set to: {folder_path}")
        showNotification("Dossier de données défini avec succès !", type = "message")

        removeModal()

      }, error = function(e) {
        cli::cli_alert_danger("Error setting datasets folder: {e$message}")
        showNotification(paste("Erreur lors de la définition du dossier :", e$message), type = "error")
      })
    })
  })
}
