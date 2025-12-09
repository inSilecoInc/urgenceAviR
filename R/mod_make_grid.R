#' make_grid UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_make_grid_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 8,
        h3("\u00c9tape 3 : Visualisation spatiale des observations", class = "text-primary"),
        p("Configurez les param\u00e8tres de grille et la visualisation des donn\u00e9es.")
      ),
      column(
        width = 4,
        class = "d-flex align-items-center justify-content-end",
        div(
          style = "padding-top: 10px;",
          actionButton(
            ns("back_to_filters"),
            HTML("<i class='fa fa-arrow-left'></i> &nbsp;Filtrer sur les esp\u00e8ces et le temps"),
            class = "btn-primary me-2"
          ),
          actionButton(
            ns("export_grid"),
            HTML("<i class='fa fa-download'></i> &nbsp;Exporter la visualisation"),
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
          bslib::card_header(h5("Param\u00e8tres")),
          bslib::card_body(
            fillable = FALSE,
            fill= FALSE,
            h6("Configuration de la grille", class = "fw-bold"),
            numericInput(
              ns("grid_size"),
              "Taille des cellules (km) :",
              value = 1,
              min = 0.5,
              max = 10,
              step = 0.1
            ),
            selectizeInput(
              ns("grid_type"),
              "Type de grille :",
              choices = list(
                "Carr\u00e9" = "square",
                "Hexagonale" = "hexagonal"
              ),
              options = list(
                dropdownParent = "body"
              ),
              selected = "hexagonal"
            ),
            selectizeInput(
              ns("color_palette"),
              "Palette :",
              choices = list(
                "Jaune-Orange-Rouge" = "YlOrRd",
                "Jaune-Orange-Brun" = "YlOrBr",
                "Bleu-Vert" = "BuGn",
                "Bleu-Violet" = "BuPu",
                "Vert-Bleu" = "GnBu",
                "Orange-Rouge" = "OrRd",
                "Violet-Bleu" = "PuBu",
                "Violet-Bleu-Vert" = "PuBuGn",
                "Violet-Rouge" = "PuRd",
                "Rouge-Violet" = "RdPu",
                "Jaune-Vert-Bleu" = "YlGnBu",
                "Jaune-Vert" = "YlGn",
                "Rouge-Jaune-Bleu" = "RdYlBu",
                "Rouge-Jaune-Vert" = "RdYlGn",
                "Spectral" = "Spectral",
                "Viridis" = "viridis",
                "Magma" = "magma",
                "Inferno" = "inferno",
                "Plasma" = "plasma"
              ),
              selected = "YlOrRd",
              options = list(
                dropdownParent = "body"
              )
            ),
            checkboxInput(
              ns("mask_target_area"),
              "Masquer la zone d'int\u00e9r\u00eat",
              value = FALSE
            ),

            h6("Variable d'int\u00e9r\u00eat", class = "fw-bold"),

            radioButtons(
              ns("variable_type"),
              "Type de variable :",
              choices = list(
                "Nombre d'observations" = "n_obs",
                "Somme de l'abondance" = "sum_abundance"
              ),
              selected = "n_obs"
            ),

            checkboxInput(
              ns("transform_log10"),
              "Transformation log10",
              value = FALSE
            )
          )
        )
      ),

      div(
        class = "col-lg-8",
        bslib::card(
          bslib::card_header(h5("Aper\u00e7u des observations")),
          bslib::card_body(
            class = "p-0",
            div(
              style = "position: relative;",
              leaflet::leafletOutput(ns("grid_map"), height = "70vh"),
              div(
                id = ns("map_loading"),
                style = "display: none; position: absolute; top: 0; left: 0; width: 100%; height: 100%; background: rgba(255,255,255,0.8); z-index: 1000; align-items: center; justify-content: center;",
                div(
                  style = "text-align: center;",
                  tags$i(class = "fa fa-spinner fa-spin fa-3x", style = "color: #0d6efd;"),
                  tags$p("G\u00e9n\u00e9ration de la grille...", style = "margin-top: 10px; color: #0d6efd; font-weight: bold;")
                )
              )
            )
          )
        )
      )
    )
  )
}

#' make_grid Server Functions
#'
#' @noRd
mod_make_grid_server <- function(id, app_values){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    values <- reactiveValues(
      grid_with_data = NULL,
      variable_selected = NULL,
      transformed_grid = NULL,
      display_grid = NULL
    )

    # Initialize map with target area if available
    output$grid_map <- leaflet::renderLeaflet({
      cli::cli_alert_info("Initializing grid configuration map")

      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$OpenStreetMap,
          group = "OpenStreetMap"
        ) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satellite"
        ) |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.DarkMatter,
          group = "Dark"
        ) |>
        leaflet::addProviderTiles(
          leaflet::providers$OpenTopoMap,
          group = "Terrain"
        ) |>
        leaflet::addLayersControl(
          baseGroups = c("OpenStreetMap", "Satellite", "Dark", "Terrain"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::setView(lng = -69.53, lat = 47.83, zoom = 8)

      # Add target area if already locked
      if (!is.null(app_values$target_area_geometry)) {
        tryCatch({
          cli::cli_alert_info("Target area found, adding to initial map")

          target_sf <- sf::st_as_sf(app_values$target_area_geometry)
          bounds <- sf::st_bbox(target_sf)

          # Validate bounds
          if (all(is.finite(c(bounds[1], bounds[2], bounds[3], bounds[4])))) {
            map <- map |>
              leaflet::addPolygons(
                data = target_sf,
                group = "target_area",
                color = "blue",
                weight = 2,
                fillOpacity = 0
              ) |>
              leaflet::fitBounds(
                lng1 = as.numeric(bounds["xmin"]),
                lat1 = as.numeric(bounds["ymin"]),
                lng2 = as.numeric(bounds["xmax"]),
                lat2 = as.numeric(bounds["ymax"])
              )

            cli::cli_alert_success("Target area added to map")
          } else {
            cli::cli_alert_warning("Invalid bounds, skipping fitBounds")
          }
        }, error = function(e) {
          cli::cli_alert_warning("Could not add target area to map: {e$message}")
        })
      }

      map
    })

    # Step 1: Generate grid with data when grid parameters change
    observeEvent(list(input$grid_size, input$grid_type, app_values$target_area_geometry, app_values$filtered_df), {
      req(app_values$target_area_geometry)
      req(app_values$filtered_df)
      req(input$grid_size)
      req(input$grid_type)

      cli::cli_alert_info("Generating grid with data: size={input$grid_size}km, type={input$grid_type}")
      shinyjs::runjs(paste0("$('#", ns("map_loading"), "').css('display', 'flex');"))

      tryCatch({
        
        # Use filtered data from module 2 (species_temporal)
        filtered_data <- app_values$filtered_df

        # Get target area geometry
        target_geom <- app_values$target_area_geometry

        # Transform to projected CRS for accurate grid creation (using UTM zone 19N for Quebec)
        target_proj <- sf::st_transform(target_geom, 32619)

        # Create grid based on type
        if (input$grid_type == "square") {
          grid <- sf::st_make_grid(
            target_proj,
            cellsize = input$grid_size * 1000, # Convert km to meters
            square = TRUE
          )
        } else {
          grid <- sf::st_make_grid(
            target_proj,
            cellsize = input$grid_size * 1000, # Convert km to meters
            square = FALSE
          )
        }
                
        # Convert grid to sf object and add IDs
        grid_sf <- sf::st_sf(
          grid_id = seq_along(grid),
          geometry = grid
        )

        # Keep only cells that intersect with target area
        grid_sf <- grid_sf[sf::st_intersects(grid_sf, target_proj, sparse = FALSE)[, 1], ]

        # Transform back to WGS84 for display
        grid_wgs84 <- sf::st_transform(grid_sf, 4326)

        values$grid <- grid_wgs84

        # Count observations and sum abundance per grid cell
        if (nrow(filtered_data) > 0 && "longitude" %in% names(filtered_data) && "latitude" %in% names(filtered_data)) {
          # Create spatial points
          data_sf <- sf::st_as_sf(
            filtered_data,
            coords = c("longitude", "latitude"),
            crs = 4326
          )

          # Transform to same projection as grid
          data_proj <- sf::st_transform(data_sf, 32619)

          # Spatial join to aggregate observations (reversed to keep all grid cells)
          joined <- sf::st_join(grid_sf, data_proj)

          # Calculate both observation counts and abundance sums
          grid_wgs84 <- joined |>
            dplyr::group_by(.data$grid_id) |>
            dplyr::summarise(
              n_obs = sum(!is.na(.data$abondance)),
              sum_abundance = sum(.data$abondance, na.rm = TRUE),
              .groups = "drop"
            ) |>
            dplyr::filter(.data$n_obs > 0) |>
            sf::st_transform(4326)
        } 

        # Store grid with data
        values$grid_with_data <- grid_wgs84

        cli::cli_alert_success("Grid with data created: {nrow(grid_wgs84)} cells")
        # Hide loading overlay
        shinyjs::runjs(paste0("$('#", ns("map_loading"), "').css('display', 'none');"))

      }, error = function(e) {
        cli::cli_alert_danger("Error generating grid: {e$message}")
        showNotification(paste("Erreur de g\u00e9n\u00e9ration de la grille :", e$message), type = "error")
        # Hide loading overlay
        shinyjs::runjs(paste0("$('#", ns("map_loading"), "').css('display', 'none');"))
      })
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    # Step 2a: Select variable type
    observeEvent(list(values$grid_with_data, input$variable_type), {
      req(values$grid_with_data)

      cli::cli_alert_info("Selecting variable: {input$variable_type}")

      tryCatch({
        grid_wgs84 <- values$grid_with_data

        # Select variable to display based on user choice
        if (input$variable_type == "sum_abundance") {
          grid_wgs84$display_value <- grid_wgs84$sum_abundance
          var_label <- "Abondance totale"
        } else {
          grid_wgs84$display_value <- grid_wgs84$n_obs
          var_label <- "Nombre d'observations"
        }

        # Store variable-selected grid and label
        values$variable_selected <- grid_wgs84
        values$var_label <- var_label

        cli::cli_alert_success("Variable selected: {var_label}")

      }, error = function(e) {
        cli::cli_alert_danger("Error selecting variable: {e$message}")
      })
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    # Step 2b: Apply log transformation
    observeEvent(list(values$variable_selected, input$transform_log10), {
      req(values$variable_selected)

      cli::cli_alert_info("Applying transformation: log10={input$transform_log10}")

      tryCatch({
        grid_wgs84 <- values$variable_selected
        var_label <- values$var_label

        # Apply transformation
        if (input$transform_log10) {
          grid_wgs84$display_value <- log10(grid_wgs84$display_value)
          var_label <- paste0(var_label, " (log10)")
        }

        # Update label
        values$transformed_grid <- grid_wgs84
        values$var_label <- var_label

        cli::cli_alert_success("Transformation applied")

      }, error = function(e) {
        cli::cli_alert_danger("Error applying transformation: {e$message}")
      })
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    # Step 2c: Prepare labels for display
    observeEvent(values$transformed_grid, {
      req(values$transformed_grid)

      cli::cli_alert_info("Preparing labels for display")

      tryCatch({
        grid_wgs84 <- values$transformed_grid

        # Create label text
        if (input$variable_type == "sum_abundance") {
          if (input$transform_log10) {
            grid_wgs84$label_text <- paste0(
              "Abondance: ", round(grid_wgs84$sum_abundance, 0),
              " (log10: ", round(grid_wgs84$display_value, 2), ")"
            )
          } else {
            grid_wgs84$label_text <- paste0("Abondance: ", round(grid_wgs84$sum_abundance, 0))
          }
        } else {
          if (input$transform_log10) {
            grid_wgs84$label_text <- paste0(
              grid_wgs84$n_obs, " observations",
              " (log10: ", round(grid_wgs84$display_value, 2), ")"
            )
          } else {
            grid_wgs84$label_text <- paste0(grid_wgs84$n_obs, " observations")
          }
        }

        # Store display grid
        values$display_grid <- grid_wgs84

        cli::cli_alert_success("Display data prepared: {nrow(grid_wgs84)} cells")

      }, error = function(e) {
        cli::cli_alert_danger("Error preparing display: {e$message}")
      })
    }, ignoreInit = FALSE, ignoreNULL = TRUE)


    # Helper function to update visualization
    update_visualization <- function(show_loading = TRUE) {
      req(values$display_grid)
      req(values$var_label)

      if (show_loading) {
        shinyjs::runjs(paste0("$('#", ns("map_loading"), "').css('display', 'flex');"))
      }

      cli::cli_alert_info("Updating grid visualization")
      tryCatch({
        grid_wgs84_filtered <- values$display_grid
        var_label <- values$var_label

        # Create color palette
        pal <- leaflet::colorNumeric(
          palette = input$color_palette,
          domain = grid_wgs84_filtered$display_value,
          na.color = "#00000000"
        )

        # Update grid on map
        leaflet::leafletProxy("grid_map") |>
          leaflet::clearGroup("grid") |>
          leaflet::clearControls() |>
          leaflet::addPolygons(
            data = grid_wgs84_filtered,
            group = "grid",
            fillColor = ~pal(display_value),
            fillOpacity = 0.6,
            color = "white",
            weight = 1,
            label = ~label_text,
            highlightOptions = leaflet::highlightOptions(
              weight = 3,
              color = "black",
              fillOpacity = 0.8,
              bringToFront = TRUE
            )
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            pal = pal,
            values = grid_wgs84_filtered$display_value,
            title = var_label,
            group = "legend"
          )

        # Enable export button
        shinyjs::enable("export_grid")

        if (show_loading) {
          shinyjs::runjs(paste0("$('#", ns("map_loading"), "').css('display', 'none');"))
        }

      }, error = function(e) {
        cli::cli_alert_danger("Error updating visualization: {e$message}")
        if (show_loading) {
          shinyjs::runjs(paste0("$('#", ns("map_loading"), "').css('display', 'none');"))
        }
      })
    }

    # Step 3: Update grid visualization when data changes
    observeEvent(list(values$display_grid, values$var_label), {
      update_visualization(show_loading = TRUE)
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    # Update only colors when palette changes (no loading overlay)
    observeEvent(input$color_palette, {
      update_visualization(show_loading = FALSE)
    }, ignoreInit = TRUE)

    # Step 4: Toggle target area overlay
    observeEvent(input$mask_target_area, {
      req(app_values$target_area_geometry)

      cli::cli_alert_info("Toggling target area overlay")

      leaflet::leafletProxy("grid_map") |>
        leaflet::clearGroup("target_area") |>
        leaflet::clearGroup("target_area_overlay")

      # Add target area if not masked
      if (!input$mask_target_area) {
        target_sf <- sf::st_as_sf(app_values$target_area_geometry)
        leaflet::leafletProxy("grid_map") |>
          leaflet::addPolygons(
            data = target_sf,
            group = "target_area_overlay",
            color = "blue",
            weight = 2,
            fillOpacity = 0
          )
      }
    }, ignoreInit = FALSE)

    # Export grid
    observeEvent(input$export_grid, {

      # TODO: Add actual export functionality here (e.g., download as shapefile, GeoJSON, etc.)
    })

    # Handle back to filters button
    observeEvent(input$back_to_filters, {
      cli::cli_alert_info("Navigating back to species_temporal tab")
      app_values$navigate_to_tab <- "species_temporal"
    })

  })
}
