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
        p("Configurez les param\u00e8tres de grille pour l'analyse spatiale et l'agr\u00e9gation des donn\u00e9es.")
      ),
      column(
        width = 4,
        class = "d-flex align-items-center justify-content-end",
        div(
          style = "padding-top: 10px;",
          actionButton(
            ns("export_grid"),
            HTML("<i class='fa fa-download'></i> &nbsp;Exporter la visualisation"),
            class = "btn-primary",
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
          bslib::card_header(h5("Param\u00e8tres de la grille")),
          bslib::card_body(
            numericInput(
              ns("grid_size"),
              "Taille des cellules de grille (km) :",
              value = 0.5,
              min = 0.1,
              max = 10,
              step = 0.1
            ),

            selectInput(
              ns("grid_type"),
              "Type de grille :",
              choices = list(
                "Carr\u00e9" = "square",
                "Hexagonale" = "hexagonal"
              ),
              selected = "hexagonal"
            )
          )
        )
      ),

      div(
        class = "col-lg-8",
        bslib::card(
          bslib::card_header(h5("Carte interactive - Aper\u00e7u de la grille")),
          bslib::card_body(
            class = "p-0",
            leaflet::leafletOutput(ns("grid_map"), height = "70vh")
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
      preview_data = NULL,
      config_validated = FALSE,
      preview_loaded = FALSE,
      grid = NULL
    )

    # Initialize map
    output$grid_map <- leaflet::renderLeaflet({
      cli::cli_alert_info("Initializing grid configuration map")

      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::setView(lng = -69.53, lat = 47.83, zoom = 8)
    })

    # Add target area to map when available
    observe({
      req(app_values$target_area_geometry)

      cli::cli_alert_info("Adding target area to grid map")

      bounds <- sf::st_bbox(app_values$target_area_geometry)

      leaflet::leafletProxy("grid_map") |>
        leaflet::clearGroup("target_area") |>
        leaflet::addPolygons(
          data = sf::st_as_sf(app_values$target_area_geometry),
          group = "target_area",
          color = "blue",
          weight = 2,
          fillOpacity = 0.1,
          label = "Zone d'intérêt"
        ) |>
        leaflet::fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    })

    # Function to generate grid
    generate_grid <- function() {
      req(app_values$target_area_geometry)
      req(app_values$filtered_df)
      req(input$grid_size)
      req(input$grid_type)

      cli::cli_alert_info("Generating grid with size={input$grid_size}km, type={input$grid_type}")

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
            cellsize = input$grid_size * 1000,  # Convert km to meters
            square = TRUE
          )
        } else {
          grid <- sf::st_make_grid(
            target_proj,
            cellsize = input$grid_size * 1000,  # Convert km to meters
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

        # Count observations per grid cell
        if (nrow(filtered_data) > 0 && "longitude" %in% names(filtered_data) && "latitude" %in% names(filtered_data)) {
          # Create spatial points
          data_sf <- sf::st_as_sf(
            filtered_data,
            coords = c("longitude", "latitude"),
            crs = 4326
          )

          # Transform to same projection as grid
          data_proj <- sf::st_transform(data_sf, 32619)

          # Spatial join to count observations
          joined <- sf::st_join(grid_sf, data_proj)
          obs_counts <- as.data.frame(table(joined$grid_id))
          names(obs_counts) <- c("grid_id", "n_obs")
          obs_counts$grid_id <- as.integer(as.character(obs_counts$grid_id))

          # Add observation counts to grid
          grid_wgs84$n_obs <- 0
          grid_wgs84$n_obs[match(obs_counts$grid_id, grid_wgs84$grid_id)] <- obs_counts$n_obs
        } else {
          grid_wgs84$n_obs <- 0
        }

        # Create color palette based on observation counts
        pal <- leaflet::colorNumeric(
          palette = "YlOrRd",
          domain = grid_wgs84$n_obs,
          na.color = "#00000000"
        )

        # Add grid to map
        leaflet::leafletProxy("grid_map") |>
          leaflet::clearGroup("grid") |>
          leaflet::clearControls() |>
          leaflet::addPolygons(
            data = grid_wgs84,
            group = "grid",
            fillColor = ~pal(n_obs),
            fillOpacity = 0.6,
            color = "white",
            weight = 1,
            label = ~paste0("Cellule ", grid_id, ": ", n_obs, " observations"),
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
            values = grid_wgs84$n_obs,
            title = "Nombre d'observations",
            group = "legend"
          )

        values$preview_loaded <- TRUE

        # Enable export button
        shinyjs::enable("export_grid")

        cli::cli_alert_success("Grid generated: {nrow(grid_wgs84)} cells")

      }, error = function(e) {
        cli::cli_alert_danger("Error generating grid: {e$message}")
        showNotification(paste("Erreur de génération de la grille :", e$message), type = "error")
      })
    }

    # Automatically generate grid when parameters change or data is available
    observe({
      generate_grid()
    }) |> bindEvent(input$grid_size, input$grid_type, app_values$filtered_df, ignoreNULL = TRUE, ignoreInit = FALSE)

    # Export grid
    observeEvent(input$export_grid, {
      req(values$grid)

      cli::cli_alert_info("Exporting grid visualization")

      grid_data <- values$grid
      n_cells <- nrow(grid_data)
      n_cells_with_data <- sum(grid_data$n_obs > 0)
      total_obs <- sum(grid_data$n_obs)

      # Store grid configuration in app_values
      app_values$grid_config <- list(
        grid_size = input$grid_size,
        grid_type = input$grid_type,
        grid = values$grid,
        n_cells = n_cells,
        n_cells_with_data = n_cells_with_data,
        total_observations = total_obs
      )

      cli::cli_alert_success("Grid ready for export: {n_cells} cells, {total_obs} total observations")
      showNotification(
        "Grille prête pour l'exportation !",
        type = "message"
      )

      # TODO: Add actual export functionality here (e.g., download as shapefile, GeoJSON, etc.)
    })
  })
}
