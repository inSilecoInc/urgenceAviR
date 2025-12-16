#' Export Grid Data and Observations
#'
#' @description Exports grid data, observations, and target area to a ZIP file
#' containing CSV and shapefiles.
#'
#' @param grid_with_data sf object with grid cells and statistics
#' @param filtered_df Data frame with filtered observations
#' @param target_area_geometry sf geometry of the target area
#' @param grid_size Numeric grid cell size in kilometers
#' @param grid_type Character grid type ("square" or "hexagonal")
#' @param output_file Character path to output ZIP file
#'
#' @return Path to the created ZIP file
#'
#' @noRd
export_grid_data <- function(grid_with_data, filtered_df, target_area_geometry,
                              grid_size, grid_type, output_file) {

  cli::cli_alert_info("Starting grid export")

  # Create temporary directory for export files
  temp_dir <- tempfile()
  dir.create(temp_dir)

  tryCatch({
    # 1. Export observations as CSV with cell_id
    cli::cli_alert_info("Preparing observations CSV")

    # Create spatial points from filtered data
    if (nrow(filtered_df) > 0 &&
        "longitude" %in% names(filtered_df) &&
        "latitude" %in% names(filtered_df)) {

      data_sf <- sf::st_as_sf(
        filtered_df,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
      )

      # Transform grid to WGS84 for spatial join
      grid_wgs84 <- sf::st_transform(grid_with_data, 4326)

      # Spatial join to assign cell_id
      data_with_cell <- sf::st_join(data_sf, grid_wgs84["grid_id"])

      # Convert to data frame and prepare export
      export_df <- as.data.frame(data_with_cell)
      export_df$geometry <- NULL  # Remove geometry column

      # Rename grid_id to cell_id
      names(export_df)[names(export_df) == "grid_id"] <- "cell_id"

      # Write CSV
      csv_path <- file.path(temp_dir, "observations.csv")
      utils::write.csv(export_df, csv_path, row.names = FALSE)
      cli::cli_alert_success("Observations CSV created")
    }

    # 2. Export grid with statistics as shapefile in Lambert Conformal Conic
    cli::cli_alert_info("Preparing grid shapefile with statistics")

    # Prepare grid with statistics
    grid_stats <- grid_with_data
    names(grid_stats)[names(grid_stats) == "grid_id"] <- "cell_id"

    # Transform to Lambert Conformal Conic (EPSG:6622 - NAD83(CSRS) / Quebec Lambert)
    grid_lambert <- sf::st_transform(grid_stats, 6622)

    # Write shapefile
    grid_shp_path <- file.path(temp_dir, "grille_stats.shp")
    sf::st_write(grid_lambert, grid_shp_path, quiet = TRUE)
    cli::cli_alert_success("Grid statistics shapefile created")

    # 3. Export reference grid as shapefile in Lambert Conformal Conic
    cli::cli_alert_info("Preparing reference grid shapefile")

    # Create reference grid (all cells, not just those with data)
    target_geom <- target_area_geometry
    target_proj <- sf::st_transform(target_geom, 32619)

    # Create grid based on current settings
    if (grid_type == "square") {
      ref_grid <- sf::st_make_grid(
        target_proj,
        cellsize = grid_size * 1000,
        square = TRUE
      )
    } else {
      ref_grid <- sf::st_make_grid(
        target_proj,
        cellsize = grid_size * 1000,
        square = FALSE
      )
    }

    # Convert to sf object
    ref_grid_sf <- sf::st_sf(
      cell_id = seq_along(ref_grid),
      geometry = ref_grid
    )

    # Keep only cells that intersect with target area
    ref_grid_sf <- ref_grid_sf[
      sf::st_intersects(ref_grid_sf, target_proj, sparse = FALSE)[, 1],
    ]

    # Transform to Lambert Conformal Conic
    ref_grid_lambert <- sf::st_transform(ref_grid_sf, 6622)

    # Write shapefile
    ref_shp_path <- file.path(temp_dir, "grille_reference.shp")
    sf::st_write(ref_grid_lambert, ref_shp_path, quiet = TRUE)
    cli::cli_alert_success("Reference grid shapefile created")

    # 4. Export target area footprint as shapefile in Lambert Conformal Conic
    cli::cli_alert_info("Preparing target area footprint shapefile")

    # Convert target area to sf object
    footprint_sf <- sf::st_as_sf(target_geom)

    # Transform to Lambert Conformal Conic
    footprint_lambert <- sf::st_transform(footprint_sf, 6622)

    # Write shapefile
    footprint_shp_path <- file.path(temp_dir, "footprint_event.shp")
    sf::st_write(footprint_lambert, footprint_shp_path, quiet = TRUE)
    cli::cli_alert_success("Target area footprint shapefile created")

    # Create ZIP file
    cli::cli_alert_info("Creating ZIP archive")
    zip_files <- list.files(temp_dir, full.names = TRUE)
    utils::zip(output_file, files = zip_files)

    cli::cli_alert_success("Export completed successfully")

    return(output_file)

  }, error = function(e) {
    cli::cli_alert_danger("Error during export: {e$message}")
    stop("Export failed: ", e$message)
  })
}
