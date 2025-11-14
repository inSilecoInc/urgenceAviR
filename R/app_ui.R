#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @noRd
app_ui <- function(request) {
  
  # Define theme
  theme <- bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#000000", 
    primary = "#0d6efd",
    secondary = "#6c757d",
    success = "#198754",
    info = "#0dcaf0",
    warning = "#ffc107",
    danger = "#dc3545",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter", wght = "600"),
    code_font = bslib::font_google("JetBrains Mono")
  )
  
  # Main page
  bslib::page_navbar(
    title = "UrgenceAviR",
    theme = theme,
    id = "main_nav",
    
    # Include dependencies
    shinyjs::useShinyjs(),
    
    # Target Area Tab
    bslib::nav_panel(
      title = span(bsicons::bs_icon("geo-alt"), "1. Déterminer la zone d'intérêt"),
      value = "target_area",
      mod_target_area_ui("target_area")
    ),
    
    # Species & Time Tab
    bslib::nav_panel(
      title = span(bsicons::bs_icon("bug"), "2. Filtrer sur les espèces et le temps"),
      value = "species_temporal",
      id = "nav_species_temporal",
      mod_species_temporal_ui("species_temporal")
    ),

    # Grid Configuration Tab
    bslib::nav_panel(
      title = span(bsicons::bs_icon("grid"), "3. Configurer et obtenir la grille d'occurences"),
      value = "grid_config",
      id = "nav_grid_config",
      mod_grid_config_ui("grid_config")
    )
    
  )
}
