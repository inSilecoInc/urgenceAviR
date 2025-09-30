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
  ns <- shiny::NS(id)
  shiny::div(
    class = "container-fluid",
    
    # Header
    shiny::div(
      class = "row mb-4",
      shiny::div(
        class = "col-12",
        shiny::h3("Étape 1 : Sélection de la zone d'étude", class = "text-primary"),
        shiny::p("Définissez votre zone d'étude en traçant un polygone sur la carte ou en téléchargeant un fichier spatial.", class = "lead"),
        shiny::hr()
      )
    ),
    
    # Main content
    shiny::div(
      class = "row",
      shiny::div(
        class = "col-lg-4",
        bslib::card(
          bslib::card_header("Méthode de sélection de la zone"),
          bslib::card_body(
            shiny::radioButtons(
              ns("area_method"),
              "Choisissez une méthode :",
              choices = list(
                "Dessiner un polygone sur la carte" = "draw",
                "Télécharger un fichier spatial" = "upload"
              ),
              selected = "draw"
            ),
            
            shiny::conditionalPanel(
              condition = "input.area_method == 'upload'",
              ns = ns,
              shiny::fileInput(
                ns("spatial_file"),
                "Télécharger un fichier spatial",
                accept = c(".shp", ".kml", ".kmz", ".gpx", ".geojson"),
                multiple = FALSE
              ),
              shiny::helpText(
                shiny::HTML("
                  <strong>Formats supportés :</strong><br/>
                  • <strong>KML/KMZ :</strong> Fichiers Google Earth<br/>
                  • <strong>GPX :</strong> Format d'échange GPS<br/>
                  • <strong>GeoJSON :</strong> Format spatial web<br/>
                  • <strong>Shapefile :</strong> Fichier .shp unique (sans composants)
                ")
              )
            ),
            
            shiny::div(
              class = "d-grid mt-3",
              shiny::actionButton(
                ns("lock_area"),
                "Verrouiller la zone d'étude",
                icon = shiny::icon("lock"),
                class = "btn-primary",
                disabled = TRUE
              )
            )
          )
        )
      ),
      
      shiny::div(
        class = "col-lg-8",
        bslib::card(
          bslib::card_header("Carte interactive"),
          bslib::card_body(
            leafletOutput(ns("area_map"), height = "80vh")
          )
        )
      )
    ),
    
    # Success message
    shiny::div(
      class = "row mt-4",
      shiny::div(
        class = "col-12",
        shiny::conditionalPanel(
          condition = "output.area_locked",
          ns = ns,
          bslib::card(
            class = "border-success",
            bslib::card_body(
              class = "text-success",
              shiny::div(
                class = "d-flex align-items-center",
                bsicons::bs_icon("lock-fill", class = "me-2"),
                shiny::strong("Zone d'étude verrouillée avec succès !"),
                shiny::span("Jeu de données filtré et prêt pour la sélection des espèces.", class = "ms-2")
              )
            )
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
    output$area_map <- renderLeaflet({
      cli::cli_alert_info("Initializing target area selection map")
      
      leaflet() |>
        addTiles() |>
        setView(lng = -74, lat = 46, zoom = 6) |>
        addDrawToolbar(
          targetGroup = "drawn",
          polylineOptions = FALSE,
          circleOptions = FALSE,
          rectangleOptions = TRUE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions()
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
      
      cli::cli_alert_info("Processing uploaded spatial file: {input$spatial_file$name}")
      
      tryCatch({
        file_path <- input$spatial_file$datapath
        file_ext <- tools::file_ext(input$spatial_file$name)
        
        # Handle different file types
        if (file_ext %in% c("shp", "kml", "kmz", "gpx", "geojson")) {
          
          cli::cli_alert_info("Processing {toupper(file_ext)} file: {input$spatial_file$name}")
          
          # Read spatial file directly (single layer, single file)
          spatial_data <- sf::st_read(file_path, quiet = TRUE)
          
          # Transform to WGS84 if needed - with robust error handling
          tryCatch({
            current_crs <- sf::st_crs(spatial_data)
            
            # Safe CRS handling
            if (is.null(current_crs) || is.na(current_crs)) {
              cli::cli_alert_warning("No CRS defined, setting to WGS84 (EPSG:4326)")
              sf::st_crs(spatial_data) <- 4326
            } else {
              crs_input <- current_crs$input
              if (is.null(crs_input) || is.na(crs_input) || crs_input == "") {
                cli::cli_alert_warning("Invalid CRS, setting to WGS84 (EPSG:4326)")
                sf::st_crs(spatial_data) <- 4326
              } else {
                cli::cli_alert_info("Current CRS: {crs_input}")
                
                # Check if transformation is needed
                tryCatch({
                  is_longlat <- sf::st_is_longlat(spatial_data)
                  if (is.na(is_longlat) || !is_longlat) {
                    cli::cli_alert_info("Transforming from {crs_input} to WGS84")
                    spatial_data <- sf::st_transform(spatial_data, 4326)
                  } else {
                    cli::cli_alert_info("Data already in geographic coordinates")
                  }
                }, error = function(e) {
                  cli::cli_alert_warning("CRS check failed: {e$message}. Setting to WGS84")
                  sf::st_crs(spatial_data) <- 4326
                })
              }
            }
          }, error = function(e) {
            cli::cli_alert_warning("CRS processing failed: {e$message}. Proceeding with original data")
          })
          
          # Log feature information and validate geometry
          tryCatch({
            cli::cli_alert_info("Loaded {nrow(spatial_data)} feature(s)")
            
            # Check geometry types with error handling
            geom_types <- sf::st_geometry_type(spatial_data)
            unique_types <- unique(as.character(geom_types))
            cli::cli_alert_info("Geometry types: {paste(unique_types, collapse = ', ')}")
            
            # Filter for polygon geometries only
            polygon_indices <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
            
            if (!any(polygon_indices)) {
              stop("No polygon geometries found in the file. Only polygon features can be used as target areas.")
            }
            
            if (sum(polygon_indices) < nrow(spatial_data)) {
              cli::cli_alert_warning("Filtering to {sum(polygon_indices)} polygon features out of {nrow(spatial_data)} total features")
              spatial_data <- spatial_data[polygon_indices, ]
            }
            
          }, error = function(e) {
            cli::cli_alert_warning("Geometry type checking failed: {e$message}. Proceeding with all features")
          })
          
          # If multiple features, take the union
          if (nrow(spatial_data) > 1) {
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
          
          # Validate the geometry
          if (length(values$target_area) == 0) {
            stop("No valid geometry extracted from the file")
          }
          
          # Add to map
          bounds <- sf::st_bbox(values$target_area)
          
          leafletProxy("area_map") |>
            clearGroup("uploaded") |>
            addPolygons(
              data = sf::st_as_sf(values$target_area),
              group = "uploaded",
              color = "red",
              weight = 2,
              fillOpacity = 0.3
            ) |>
            fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
          
          # Enable lock button
          shinyjs::enable("lock_area")
          
          cli::cli_alert_success("Spatial file successfully loaded")
          
        } else {
          cli::cli_alert_danger("Unsupported file format: {file_ext}")
          showNotification("Format de fichier non pris en charge. Veuillez télécharger des fichiers .shp, .kml, .kmz, .gpx ou .geojson.", type = "error")
        }
        
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
      leafletProxy("area_map") |>
        clearGroup("drawn") |>
        clearGroup("uploaded")
    })
    
    # Lock area and filter dataset
    observeEvent(input$lock_area, {
      req(values$target_area)
      
      cli::cli_alert_info("Locking target area and filtering dataset")
      
      # Perform area checks
      area_km2 <- as.numeric(sf::st_area(values$target_area)) / 1000000
      
      if (area_km2 < 0.1) {
        cli::cli_alert_warning("Area is very small ({round(area_km2, 3)} km²)")
        showNotification("Avertissement : La zone sélectionnée est très petite. Considérez sélectionner une zone plus grande.", type = "warning")
      } else if (area_km2 > 100000) {
        cli::cli_alert_warning("Area is very large ({round(area_km2, 0)} km²)")
        showNotification("Avertissement : La zone sélectionnée est très grande. Le traitement peut prendre beaucoup de temps.", type = "warning")
      }
      
      tryCatch({
        # Filter dataset by target area
        cli::cli_alert_info("Filtering dataset by target area")
        
        # Use pre-loaded datasets from app_values
        req(app_values$all_data)
        all_data <- app_values$all_data
        
        # Convert target area to WGS84 if needed
        target_area_wgs84 <- sf::st_transform(values$target_area, 4326)
        
        # Create spatial points from the dataset
        if ("longitude" %in% names(all_data) && "latitude" %in% names(all_data)) {
          # Remove records with missing coordinates
          complete_coords <- complete.cases(all_data[c("longitude", "latitude")])
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
          
          # Get filtered data back with coordinates
          values$filtered_data <- data_with_coords[filtered_indices, ]
          
          cli::cli_alert_success("Dataset filtered: {nrow(values$filtered_data)} records within target area (from {nrow(all_data)} total)")
          
        } else {
          cli::cli_alert_warning("Dataset does not contain longitude/latitude columns")
          values$filtered_data <- all_data
        }
        
        values$area_locked <- TRUE
        
        cli::cli_alert_success("Target area locked successfully")
        showNotification("Zone d'étude verrouillée et jeu de données filtré !", type = "message")
        
      }, error = function(e) {
        cli::cli_alert_danger("Error filtering dataset: {e$message}")
        showNotification(paste("Erreur de filtrage du jeu de données :", e$message), type = "error")
      })
    })
    
    # Output for conditional panel
    output$area_locked <- reactive({
      values$area_locked
    })
    outputOptions(output, "area_locked", suspendWhenHidden = FALSE)
    
    # Return the locked area and filtered data for use by other modules
    return(reactive({
      if (values$area_locked) {
        list(
          geometry = values$target_area,
          source = values$area_source,
          area_km2 = as.numeric(sf::st_area(values$target_area)) / 1000000,
          filtered_data = values$filtered_data
        )
      } else {
        NULL
      }
    }))
  })
}
