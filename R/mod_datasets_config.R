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
mod_datasets_config_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      folder_configured = !is.null(datasets_folder()),
      selected_folder_path = NULL
    )
    
    # Initialize shinyFiles for directory selection
    volumes <- c(Home = fs::path_home(), "Root" = "/")
    
    # Check if datasets folder needs to be configured on startup
    shiny::observe({
      if (is.null(datasets_folder())) {
        cli::cli_alert_warning("Datasets folder not set, showing configuration modal")
        show_config_modal()
      } else {
        values$folder_configured <- TRUE
      }
    })
    
    # Function to show configuration modal
    show_config_modal <- function() {
      shiny::showModal(
        shiny::modalDialog(
          title = "Configurer le dossier de données",
          size = "m",
          
          shiny::p("Bienvenue dans UrgenceAviR ! Veuillez définir le chemin vers votre dossier de données pour continuer."),
          
          shinyFiles::shinyDirButton(
            ns("datasets_folder_input"),
            "Choisir le dossier de données",
            "Sélectionnez le dossier contenant vos données",
            icon = shiny::icon("folder-open"),
            class = "btn-outline-primary"
          ),
          
          shiny::br(), shiny::br(),
          shiny::htmlOutput(ns("selected_folder_display")),
          
          shiny::helpText("Ce devrait être le dossier contenant des fichiers comme eBird.gdb, ConsultationCanardsDeMer.csv, etc."),
          
          footer = shiny::tagList(
            shiny::actionButton(
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
    
    shiny::observeEvent(input$datasets_folder_input, {
      if (!is.null(input$datasets_folder_input) && !is.integer(input$datasets_folder_input)) {
        folder_selected <- shinyFiles::parseDirPath(volumes, input$datasets_folder_input)
        if (length(folder_selected) > 0) {
          values$selected_folder_path <- as.character(folder_selected)
          cli::cli_alert_info("Folder selected: {folder_selected}")
        }
      }
    })
    
    # Display selected folder
    output$selected_folder_display <- shiny::renderUI({
      if (!is.null(values$selected_folder_path)) {
        shiny::p(
          shiny::strong("Dossier sélectionné : "),
          shiny::code(values$selected_folder_path),
          class = "text-success"
        )
      } else {
        shiny::p(
          shiny::em("Aucun dossier sélectionné"),
          class = "text-muted"
        )
      }
    })
    
    # Confirm datasets folder setting
    shiny::observeEvent(input$confirm_datasets_folder, {
      folder_path <- values$selected_folder_path
      
      if (is.null(folder_path) || length(folder_path) == 0) {
        shiny::showNotification("Veuillez d'abord sélectionner un dossier", type = "error")
        return()
      }
      
      if (!dir.exists(folder_path)) {
        shiny::showNotification("Le dossier sélectionné n'existe pas. Veuillez choisir un autre dossier.", type = "error")
        return()
      }
      
      tryCatch({
        # Set the datasets folder
        set_datasets_folder(folder_path)
        values$folder_configured <- TRUE
        
        cli::cli_alert_success("Datasets folder set to: {folder_path}")
        shiny::showNotification("Dossier de données défini avec succès !", type = "message")
        
        shiny::removeModal()
        
      }, error = function(e) {
        cli::cli_alert_danger("Error setting datasets folder: {e$message}")
        shiny::showNotification(paste("Erreur lors de la définition du dossier :", e$message), type = "error")
      })
    })
    
    # Return configuration status for use by other modules
    return(reactive({
      list(
        configured = values$folder_configured,
        folder_path = datasets_folder()
      )
    }))
  })
}
