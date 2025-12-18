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

  # Helper function to format dataset names
  format_dataset_name <- function(name) {
    tools::toTitleCase(gsub("_", " ", name))
  }

  showModal(
    modalDialog(
      title = "T\u00e9l\u00e9verser les fichiers de donn\u00e9es",
      size = "xl",

      # Welcome message and button in fluid row
      fluidRow(
        column(
          width = 8,
          p("Bienvenue dans UrgenceAviR ! Veuillez t\u00e9l\u00e9verser les fichiers de donn\u00e9es requis.")
        ),
        column(
          width = 4,
          class = "text-end",
          actionButton(
            ns("confirm_uploads"),
            "Confirmer et continuer",
            class = "btn-primary"
          )
        )
      ),

      # Upload table with all datasets
      tags$div(
        class = "table-responsive mt-3",
        tags$table(
          class = "table table-sm",
          tags$thead(
            tags$tr(
              tags$th("Jeu de donn\u00e9es", style = "width: 40%;"),
              tags$th("Fichier", style = "width: 30%;"),
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
                  tags$strong(format_dataset_name(dataset_name))
                ),
                tags$td(
                  tags$div(
                    fileInput(
                      ns(paste0("upload_", dataset_name)),
                      NULL,
                      accept = c(".csv", ".xlsx", ".gdb"),
                      buttonLabel = "Choisir...",
                      placeholder = "Aucun fichier",
                      width = "100%"
                    )
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

      footer = NULL,
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

  files_info <- external_files()

  # Helper function to format dataset names
  format_dataset_name <- function(name) {
    tools::toTitleCase(gsub("_", " ", name))
  }


  # Create observers for each dataset file upload
  lapply(names(files_info), function(dataset_name) {
    observeEvent(input[[paste0("upload_", dataset_name)]], {
      req(input[[paste0("upload_", dataset_name)]])

      uploaded <- input[[paste0("upload_", dataset_name)]]
      expected_file_name <- files_info[[dataset_name]]$file

      cli::cli_alert_info("File uploaded for {dataset_name}: {uploaded$name}")

      # Get expected extension
      expected_ext <- tools::file_ext(expected_file_name)
      uploaded_ext <- tools::file_ext(uploaded$name)

      # Validate file extension
      if (tolower(uploaded_ext) != tolower(expected_ext)) {
        # Update status to error
        shinyjs::html(
          paste0("status_", dataset_name),
          as.character(tags$span(
            icon("exclamation-triangle", class = "text-danger"),
            tags$strong(" Extension incorrecte"),
            tags$br(),
            tags$small(paste("Attendu:"), expected_ext),
            class = "text-danger"
          ))
        )
        showNotification(
          paste0(
            "Erreur: Le fichier doit avoir l'extension '.",
            expected_ext, "' (re\u00e7u: .", uploaded_ext, ")"
          ),
          type = "error",
          duration = 5
        )
        return()
      }

      # Validate columns
      required_cols <- files_info[[dataset_name]]$check_columns
      tryCatch({
        # Read file to check columns
        file_data <- if (tolower(uploaded_ext) == "csv") {
          data.table::fread(uploaded$datapath, nrows = 1)
        } else if (tolower(uploaded_ext) == "xlsx") {
          readxl::read_excel(uploaded$datapath, n_max = 1)
        } else {
          NULL
        }

        if (!is.null(file_data)) {
          file_cols <- names(file_data)
          missing_cols <- setdiff(required_cols, file_cols)

          if (length(missing_cols) > 0) {
            # Update status to error
            shinyjs::html(
              paste0("status_", dataset_name),
              as.character(tags$span(
                icon("exclamation-triangle", class = "text-danger"),
                tags$strong(" Colonnes manquantes"),
                tags$br(),
                tags$small(paste(
                  paste(missing_cols, collapse = ", "))),
                class = "text-danger"
              ))
            )
            showNotification(
              paste0(
                "Erreur: Colonnes manquantes dans le fichier: ",
                paste(missing_cols, collapse = ", ")
              ),
              type = "error",
              duration = 10
            )
            return()
          }
        }
      }, error = function(e) {
        # Update status to error
        shinyjs::html(
          paste0("status_", dataset_name),
          as.character(tags$span(
            icon("exclamation-triangle", class = "text-danger"),
            " Erreur de lecture",
            tags$br(),
            tags$small(paste("Erreur:", e$message)),
            class = "text-danger"
          ))
        )
        showNotification(
          paste0("Erreur lors de la validation: ", e$message),
          type = "error",
          duration = 5
        )
        return()
      })

      # Copy file to temp folder with correct name (automatically renaming if needed)
      dest_path <- file.path(values$temp_folder, expected_file_name)
      file.copy(uploaded$datapath, dest_path, overwrite = TRUE)

      if (uploaded$name != expected_file_name) {
        cli::cli_alert_info("File renamed from '{uploaded$name}' to '{expected_file_name}'")
      }

      # Store in uploaded_files
      values$uploaded_files[[dataset_name]] <- dest_path

      cli::cli_alert_success("Copied {expected_file_name} to temp folder for {dataset_name}")

      # Update status to success
      shinyjs::html(
        paste0("status_", dataset_name),
        as.character(tags$span(
          icon("check-circle", class = "text-success"),
          " T\u00e9l\u00e9vers\u00e9",
          class = "text-success"
        ))
      )
    })
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
      showNotification("Fichiers t\u00e9l\u00e9vers\u00e9s avec succ\u00e8s !", type = "message")

      removeModal()

    }, error = function(e) {
      cli::cli_alert_danger("Error setting datasets folder from uploads: {e$message}")
      showNotification(paste("Erreur lors de la configuration :", e$message), type = "error")
    })
  })
}
