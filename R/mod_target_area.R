#' target_area UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_target_area_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 8,
        h3("\u00c9tape 1 : S\u00e9lection de la zone d'int\u00e9r\u00eat", class = "text-primary"),
        p("D\u00e9finissez votre zone d'int\u00e9r\u00eat en tra\u00e7ant un polygone sur la carte ou en t\u00e9l\u00e9chargeant un fichier spatial.")
      ),
      column(
        width = 4,
        class = "d-flex align-items-center justify-content-end",
        div(
          style = "padding-top: 10px;",
          actionButton(
            ns("lock_area"),
            HTML("<i class='fa fa-lock'></i> &nbsp;Verrouiller la zone d'int\u00e9r\u00eat &nbsp;<i class='fa fa-arrow-right'></i>"),
            class = "btn-success",
            disabled = TRUE
          )
        )
      )
    ),

    # Main content
    div(
      class = "row",
      div(
        class = "col-lg-4",
        bslib::card(
          bslib::card_header(h5("M\u00e9thode de s\u00e9lection de la zone")),
          bslib::card_body(
            radioButtons(
              ns("area_method"),
              "Choisissez une m\u00e9thode :",
              choices = list(
                "Dessiner un polygone sur la carte" = "draw",
                "T\u00e9l\u00e9verser un fichier spatial" = "upload"
              ),
              selected = "draw"
            ),

            conditionalPanel(
              condition = "input.area_method == 'upload'",
              ns = ns,
              fileInput(
                ns("spatial_file"),
                "T\u00e9l\u00e9verser un fichier spatial",
                accept = c(".shp", ".shx", ".dbf", ".prj", ".geojson"),
                multiple = TRUE
              ),
              helpText(
                HTML("
                  <strong>Formats support\u00e9s :</strong><br/>
                  \u2022 <strong>GeoJSON :</strong> Fichier unique .geojson<br/>
                  \u2022 <strong>Shapefile :</strong> S\u00e9lectionnez TOUS les fichiers (.shp, .shx, .dbf, .prj)
                ")
              )
            )
          )
        )
      ),
      
      div(
        class = "col-lg-8",
        bslib::card(
          bslib::card_header(h5("Carte interactive")),
          bslib::card_body(
            class = "p-0",
            leaflet::leafletOutput(ns("area_map"), height = "70vh")
          )
        )
      )
    )
  )
}

#' target_area Server Functions
#'
#' @noRd 
mod_target_area_server <- function(id, app_values){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      target_area = NULL,
      area_locked = FALSE,
      area_source = NULL,
      filtered_data = NULL
    )
    
    # Initialize map
    output$area_map <- leaflet::renderLeaflet({
      cli::cli_alert_info("Initializing target area selection map")

      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::setView(lng = -69.53, lat =  47.83, zoom = 8) |>
        leaflet.extras::addDrawToolbar(
          targetGroup = "drawn",
          polylineOptions = FALSE,
          circleOptions = FALSE,
          rectangleOptions = TRUE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()
        )
    })
    
    # Handle polygon drawing
    observeEvent(input$area_map_draw_new_feature, {
      cli::cli_alert_info("New polygon drawn on map")
      
      feature <- input$area_map_draw_new_feature
      
      if (feature$properties$feature_type %in% c("polygon", "rectangle")) {
        # Convert leaflet coordinates to sf object
        coords <- feature$geometry$coordinates[[1]]
        
        # Create matrix of coordinates
        coord_matrix <- do.call(rbind, lapply(coords, function(x) c(x[[1]], x[[2]])))
        
        # Ensure polygon is closed
        if (!identical(coord_matrix[1, ], coord_matrix[nrow(coord_matrix), ])) {
          coord_matrix <- rbind(coord_matrix, coord_matrix[1, ])
        }
        
        # Create sf polygon
        poly <- sf::st_polygon(list(coord_matrix))
        values$target_area <- sf::st_sfc(poly, crs = 4326)
        values$area_source <- "drawn"
        
        # Enable lock button
        shinyjs::enable("lock_area")
        
        cli::cli_alert_success("Polygon successfully captured")
      }
    })
    
    # Handle polygon editing
    observeEvent(input$area_map_draw_edited_features, {
      cli::cli_alert_info("Polygon edited on map")
      
      # Update the stored polygon with edited version
      feature <- input$area_map_draw_edited_features$features[[1]]
      
      if (!is.null(feature)) {
        coords <- feature$geometry$coordinates[[1]]
        coord_matrix <- do.call(rbind, lapply(coords, function(x) c(x[[1]], x[[2]])))
        
        if (!identical(coord_matrix[1, ], coord_matrix[nrow(coord_matrix), ])) {
          coord_matrix <- rbind(coord_matrix, coord_matrix[1, ])
        }
        
        poly <- sf::st_polygon(list(coord_matrix))
        values$target_area <- sf::st_sfc(poly, crs = 4326)
        
        cli::cli_alert_success("Polygon successfully updated")
      }
    })
    
    # Handle polygon deletion
    observeEvent(input$area_map_draw_deleted_features, {
      cli::cli_alert_info("Polygon deleted from map")
      values$target_area <- NULL
      values$area_source <- NULL
      values$area_locked <- FALSE
      shinyjs::disable("lock_area")
    })
    
    # Handle file upload
    observeEvent(input$spatial_file, {
      req(input$spatial_file)

      uploaded_files <- input$spatial_file
      file_names <- uploaded_files$name
      file_exts <- tools::file_ext(file_names)

      cli::cli_alert_info("Processing uploaded spatial file(s): {paste(file_names, collapse = ', ')}")

      tryCatch({
        # Determine file type
        if ("geojson" %in% file_exts) {
          # GeoJSON - single file
          if (length(file_names) > 1) {
            showNotification(
              "Pour GeoJSON, veuillez t\u00e9l\u00e9verser un seul fichier.",
              type = "error"
            )
            return()
          }
          file_path <- uploaded_files$datapath[1]
          cli::cli_alert_info("Processing GeoJSON file: {file_names[1]}")
          spatial_data <- sf::st_read(file_path, quiet = TRUE)

        } else if ("shp" %in% file_exts) {
          # Shapefile - multiple files required
          required_exts <- c("shp", "shx", "dbf")
          missing_exts <- setdiff(required_exts, file_exts)

          if (length(missing_exts) > 0) {
            showNotification(
              paste0(
                "Fichiers shapefile manquants: .",
                paste(missing_exts, collapse = ", .")
              ),
              type = "error"
            )
            return()
          }

          # Create temp directory for shapefile components
          temp_dir <- tempfile()
          dir.create(temp_dir)

          # Get base name from .shp file
          shp_name <- file_names[file_exts == "shp"][1]
          base_name <- tools::file_path_sans_ext(shp_name)

          # Copy all uploaded files to temp directory with correct names
          for (i in seq_along(uploaded_files$name)) {
            ext <- tools::file_ext(uploaded_files$name[i])
            dest_file <- file.path(temp_dir, paste0(base_name, ".", ext))
            file.copy(uploaded_files$datapath[i], dest_file)
            cli::cli_alert_info("Copied {ext} component to temp directory")
          }

          # Read shapefile from temp directory
          shp_path <- file.path(temp_dir, shp_name)
          cli::cli_alert_info("Processing Shapefile: {shp_name}")
          spatial_data <- sf::st_read(shp_path, quiet = TRUE)

        } else {
          cli::cli_alert_danger("Unsupported file format")
          showNotification(
            "Format de fichier non pris en charge. Veuillez t\u00e9l\u00e9verser des fichiers .shp (avec .shx, .dbf) ou .geojson.",
            type = "error"
          )
          return()
        }

        # Transform to WGS84 if needed - with robust error handling
        tryCatch({
          current_crs <- sf::st_crs(spatial_data)

          # Safe CRS handling - check if CRS is valid
          has_valid_crs <- FALSE
          tryCatch({
            has_valid_crs <- !is.null(current_crs) && !is.na(current_crs$input) && current_crs$input != ""
          }, error = function(e) {
            has_valid_crs <- FALSE
          })

          if (!has_valid_crs) {
            cli::cli_alert_warning("No valid CRS defined, setting to WGS84 (EPSG:4326)")
            sf::st_crs(spatial_data) <- 4326
          } else {
            cli::cli_alert_info("Current CRS: {current_crs$input}")

            # Check if transformation is needed
            tryCatch({
              is_longlat <- sf::st_is_longlat(spatial_data)
              if (isTRUE(is_longlat)) {
                cli::cli_alert_info("Data already in geographic coordinates")
              } else {
                cli::cli_alert_info("Transforming from {current_crs$input} to WGS84")
                spatial_data <- sf::st_transform(spatial_data, 4326)
              }
            }, error = function(e) {
              cli::cli_alert_warning("CRS check failed: {e$message}. Setting to WGS84")
              sf::st_crs(spatial_data) <- 4326
            })
          }
        }, error = function(e) {
          cli::cli_alert_warning("CRS processing failed: {e$message}. Proceeding with original data")
        })

        # If multiple features, take the union
        n_features <- nrow(spatial_data)
        if (!is.na(n_features) && n_features > 1) {
          cli::cli_alert_info("Multiple polygon features found, creating union")
          tryCatch({
            spatial_data <- sf::st_union(spatial_data)
          }, error = function(e) {
            cli::cli_alert_warning("Union failed: {e$message}. Using first feature only.")
            spatial_data <- spatial_data[1, ]
          })
        }

        # Extract geometry
        values$target_area <- sf::st_geometry(spatial_data)
        values$area_source <- "uploaded"

        # Add to map
        bounds <- sf::st_bbox(values$target_area)

        leaflet::leafletProxy("area_map") |>
          leaflet::clearGroup("uploaded") |>
          leaflet::addPolygons(
            data = sf::st_as_sf(values$target_area),
            group = "uploaded",
            color = "blue",
            weight = 2,
            fillOpacity = 0.3
          ) |>
          leaflet::fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])

        # Enable lock button
        shinyjs::enable("lock_area")

        cli::cli_alert_success("Spatial file successfully loaded")
        
      }, error = function(e) {
        cli::cli_alert_danger("Error reading spatial file: {e$message}")
        showNotification(paste("Erreur de lecture du fichier :", e$message), type = "error")
      })
    })
    
    # Reset when method changes
    observeEvent(input$area_method, {
      cli::cli_alert_info("Area selection method changed to: {input$area_method}")
      
      values$target_area <- NULL
      values$area_source <- NULL
      values$area_locked <- FALSE
      shinyjs::disable("lock_area")
      
      # Clear map
      leaflet::leafletProxy("area_map") |>
        leaflet::clearGroup("drawn") |>
        leaflet::clearGroup("uploaded")
    })
    
    # Lock area and filter dataset
    observeEvent(input$lock_area, {
      req(values$target_area)

      cli::cli_alert_info("Locking target area and filtering dataset")

      # Show full page spinner
      shinycssloaders::showPageSpinner(caption = "Filtrage spatial des donn\u00e9es...")

      tryCatch({
        # Filter dataset by target area
        cli::cli_alert_info("Filtering dataset by target area")

        # Use pre-loaded datasets from app_values
        req(app_values$all_df)
        all_data <- app_values$all_df

        # Convert target area to WGS84 if needed
        target_area_wgs84 <- sf::st_transform(values$target_area, 4326)

        # Create spatial points from the dataset
        if ("longitude" %in% names(all_data) && "latitude" %in% names(all_data)) {
          # Remove records with missing coordinates
          complete_coords <- stats::complete.cases(all_data[c("longitude", "latitude")])
          data_with_coords <- all_data[complete_coords, ]

          # Create sf points
          data_sf <- sf::st_as_sf(
            data_with_coords,
            coords = c("longitude", "latitude"),
            crs = 4326
          )

          # Filter points within target area
          intersects <- sf::st_intersects(data_sf, target_area_wgs84, sparse = FALSE)
          filtered_indices <- which(intersects[, 1])

          # Update app_values with spatially filtered data
          app_values$spatially_filtered_data <- data_with_coords[filtered_indices, ]

          cli::cli_alert_success("Dataset filtered: {nrow(app_values$spatially_filtered_data)} records within target area (from {nrow(all_data)} total)")

        } else {
          cli::cli_alert_warning("Dataset does not contain longitude/latitude columns")
          app_values$spatially_filtered_data <- all_data
        }

        values$area_locked <- TRUE

        # Update app_values with target area information
        app_values$target_area_geometry <- values$target_area
        app_values$target_area_source <- values$area_source
        app_values$target_area_locked <- TRUE

        cli::cli_alert_success("Target area locked successfully")

        showNotification("Zone d'int\u00e9r\u00eat verrouill\u00e9e et jeu de donn\u00e9es filtr\u00e9 !", type = "message")

        # Signal to app_server to navigate to next tab
        app_values$navigate_to_tab <- "species_temporal"

        # Hide spinner after navigation signal
        shinycssloaders::hidePageSpinner()

      }, error = function(e) {
        # Hide spinner on error
        shinycssloaders::hidePageSpinner()

        cli::cli_alert_danger("Error filtering dataset: {e$message}")
        showNotification(paste("Erreur de filtrage du jeu de donn\u00e9es :", e$message), type = "error")
      })
    })
    
    # Output for conditional panel
    output$area_locked <- reactive({
      values$area_locked
    })
    outputOptions(output, "area_locked", suspendWhenHidden = FALSE)
  })
}
