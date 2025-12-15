#' Show and Handle File Upload Modal
#'
#' @description Functions for the file upload modal (production mode)
#'
#' @param ns Namespace function
#'
#' @noRd
show_upload_modal <- function(ns) {
  # Get list of expected files
  files_info <- external_files()

  showModal(
    modalDialog(
      title = "Téléverser les fichiers de données",
      size = "xl",

      p("Bienvenue dans UrgenceAviR ! Veuillez téléverser les fichiers de données requis."),

      tags$div(
        class = "table-responsive mt-3",
        tags$table(
          class = "table table-striped table-hover",
          tags$thead(
            tags$tr(
              tags$th("Fichier requis", style = "width: 40%;"),
              tags$th("Action", style = "width: 30%;"),
              tags$th("Statut", style = "width: 30%;")
            )
          ),
          tags$tbody(
            id = ns("upload_table_body"),
            lapply(names(files_info), function(dataset_name) {
              file_name <- files_info[[dataset_name]]$file
              tags$tr(
                id = ns(paste0("row_", dataset_name)),
                tags$td(
                  tags$code(file_name),
                  tags$br(),
                  tags$small(
                    class = "text-muted",
                    paste("Dataset:", dataset_name)
                  )
                ),
                tags$td(
                  fileInput(
                    ns(paste0("upload_", dataset_name)),
                    NULL,
                    accept = c(".csv", ".xlsx", ".gdb"),
                    buttonLabel = "Choisir...",
                    placeholder = "Aucun fichier",
                    width = "100%"
                  )
                ),
                tags$td(
                  id = ns(paste0("status_", dataset_name)),
                  tags$span(
                    icon("clock", class = "text-muted"),
                    " En attente",
                    class = "text-muted"
                  )
                )
              )
            })
          )
        )
      ),

      tags$div(
        class = "alert alert-info mt-3",
        icon("info-circle"),
        " Taille maximale par fichier : 50 MB"
      ),

      footer = tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-center w-100",
          tags$span(
            id = ns("upload_summary"),
            class = "text-muted",
            "0 fichier(s) téléversé(s) sur ",
            length(files_info)
          ),
          actionButton(
            ns("confirm_uploads"),
            "Confirmer et continuer",
            class = "btn-primary",
            disabled = "disabled"
          )
        )
      ),
      easyClose = FALSE
    )
  )
}

#' Setup Upload Modal Observers
#'
#' @description Sets up all observers for file upload modal
#'
#' @param input,output,session Shiny parameters
#' @param ns Namespace function
#' @param values Reactive values object
#' @param app_values Application-level reactive values
#'
#' @noRd
setup_upload_modal_observers <- function(input, output, session, ns, values, app_values) {

  # Initialize temp folder
  isolate({
    values$temp_folder <- tempdir()
  })

  temp_folder_path <- tempdir()
  cli::cli_alert_info("Created temp folder: {temp_folder_path}")

  # Create observers for each dataset file upload
  files_info <- external_files()

  lapply(names(files_info), function(dataset_name) {
    observeEvent(input[[paste0("upload_", dataset_name)]], {
      req(input[[paste0("upload_", dataset_name)]])

      uploaded <- input[[paste0("upload_", dataset_name)]]
      expected_file_name <- files_info[[dataset_name]]$file

      cli::cli_alert_info("File uploaded for {dataset_name}: {uploaded$name}")

      # Validate file name matches expected
      if (uploaded$name != expected_file_name) {
        # Update status to error
        shinyjs::html(
          paste0("status_", dataset_name),
          as.character(tags$span(
            icon("exclamation-triangle", class = "text-danger"),
            " Nom de fichier incorrect",
            tags$br(),
            tags$small(paste("Attendu:", expected_file_name)),
            class = "text-danger"
          ))
        )
        showNotification(
          paste0("Erreur: Le fichier doit s'appeler '", expected_file_name, "'"),
          type = "error",
          duration = 5
        )
        return()
      }

      # Copy file to temp folder with correct name
      dest_path <- file.path(values$temp_folder, expected_file_name)
      file.copy(uploaded$datapath, dest_path, overwrite = TRUE)

      # Store in uploaded_files
      values$uploaded_files[[dataset_name]] <- dest_path

      cli::cli_alert_success("Copied {expected_file_name} to temp folder for {dataset_name}")

      # Update status to success
      shinyjs::html(
        paste0("status_", dataset_name),
        as.character(tags$span(
          icon("check-circle", class = "text-success"),
          " Téléversé",
          class = "text-success"
        ))
      )

      # Update summary count
      upload_count <- length(values$uploaded_files)
      total_count <- length(files_info)
      shinyjs::html(
        "upload_summary",
        paste0(upload_count, " fichier(s) téléversé(s) sur ", total_count)
      )
    })
  })

  # Enable/disable confirm button based on uploaded files
  observe({
    files_info <- external_files()
    uploaded_count <- length(values$uploaded_files)
    total_count <- length(files_info)
    all_uploaded <- uploaded_count == total_count

    if (all_uploaded) {
      shinyjs::enable("confirm_uploads")
    } else {
      shinyjs::disable("confirm_uploads")
    }
  })

  # Confirm uploads and set temp folder as datasets folder
  observeEvent(input$confirm_uploads, {
    tryCatch({
      temp_folder <- values$temp_folder

      if (is.null(temp_folder) || !dir.exists(temp_folder)) {
        showNotification("Erreur : dossier temporaire introuvable", type = "error")
        return()
      }

      # Set the temp folder as datasets folder
      set_datasets_folder(temp_folder)
      values$folder_configured <- TRUE
      app_values$datasets_folder_configured <- TRUE
      app_values$datasets_folder_path <- temp_folder

      cli::cli_alert_success("Datasets folder set to temp folder: {temp_folder}")
      showNotification("Fichiers téléversés avec succès !", type = "message")

      removeModal()

    }, error = function(e) {
      cli::cli_alert_danger("Error setting datasets folder from uploads: {e$message}")
      showNotification(paste("Erreur lors de la configuration :", e$message), type = "error")
    })
  })
}
