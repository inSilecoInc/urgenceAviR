#' taxonomy_info UI Function
#'
#' @description A shiny Module for displaying taxonomy information tables.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_taxonomy_info_ui <- function(id){
  ns <- NS(id)
  tagList(
    # This module doesn't have its own UI, it's called from other modules
  )
}

#' taxonomy_info Server Functions
#'
#' @noRd
mod_taxonomy_info_server <- function(id, taxo_vuln_data){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show functional group taxonomy table
    show_functional_group_table <- function() {
      taxo <- taxo_vuln_data()
      req(taxo)

      cli::cli_alert_info("Showing functional group taxonomy modal")

      # Prepare data grouped by functional group
      taxo_grouped <- taxo |>
        dplyr::select(groupe_fonctionnel, nom_francais, nom_latin, code_id) |>
        dplyr::arrange(groupe_fonctionnel, nom_francais)

      showModal(
        modalDialog(
          title = "Table taxonomique - Groupes fonctionnels",
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Fermer"),
          reactable::reactableOutput(ns("taxo_functional_table"))
        )
      )

      output$taxo_functional_table <- reactable::renderReactable({
        reactable::reactable(
          taxo_grouped,
          filterable = TRUE,
          searchable = TRUE,
          defaultPageSize = 15,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 15, 25, 50),
          striped = TRUE,
          highlight = TRUE,
          language = reactable::reactableLang(
            searchPlaceholder = "Rechercher...",
            searchLabel = "Rechercher",
            noData = "Aucune donn\u00e9e disponible",
            pageSizeOptions = "Afficher {rows}",
            pageInfo = "{rowStart} \u00e0 {rowEnd} sur {rows} entr\u00e9es",
            pagePrevious = "Pr\u00e9c\u00e9dent",
            pageNext = "Suivant",
            pagePreviousLabel = "Page pr\u00e9c\u00e9dente",
            pageNextLabel = "Page suivante"
          ),
          columns = list(
            groupe_fonctionnel = reactable::colDef(
              name = "Groupe fonctionnel",
              minWidth = 150,
              filterable = TRUE
            ),
            nom_francais = reactable::colDef(
              name = "Nom fran\u00e7ais",
              minWidth = 150
            ),
            nom_latin = reactable::colDef(
              name = "Nom latin",
              minWidth = 150,
              style = list(fontStyle = "italic")
            ),
            code_id = reactable::colDef(
              name = "Code",
              minWidth = 80
            )
          ),
          defaultColDef = reactable::colDef(
            align = "left",
            headerStyle = list(background = "#f7f7f8")
          )
        )
      })
    }

    # Show habitat taxonomy table
    show_habitat_table <- function() {
      taxo <- taxo_vuln_data()
      req(taxo)

      cli::cli_alert_info("Showing habitat taxonomy modal")

      # Prepare data grouped by habitat
      taxo_grouped <- taxo |>
        dplyr::select(milieu_marin, nom_francais, nom_latin, code_id) |>
        dplyr::arrange(milieu_marin, nom_francais)

      showModal(
        modalDialog(
          title = "Table taxonomique - Milieux",
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Fermer"),
          reactable::reactableOutput(ns("taxo_habitat_table"))
        )
      )

      output$taxo_habitat_table <- reactable::renderReactable({
        reactable::reactable(
          taxo_grouped,
          filterable = TRUE,
          searchable = TRUE,
          defaultPageSize = 15,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 15, 25, 50),
          striped = TRUE,
          highlight = TRUE,
          language = reactable::reactableLang(
            searchPlaceholder = "Rechercher...",
            searchLabel = "Rechercher",
            noData = "Aucune donn\u00e9e disponible",
            pageSizeOptions = "Afficher {rows}",
            pageInfo = "{rowStart} \u00e0 {rowEnd} sur {rows} entr\u00e9es",
            pagePrevious = "Pr\u00e9c\u00e9dent",
            pageNext = "Suivant",
            pagePreviousLabel = "Page pr\u00e9c\u00e9dente",
            pageNextLabel = "Page suivante"
          ),
          columns = list(
            milieu_marin = reactable::colDef(
              name = "Milieu marin",
              minWidth = 120,
              filterable = TRUE
            ),
            nom_francais = reactable::colDef(
              name = "Nom fran\u00e7ais",
              minWidth = 150
            ),
            nom_latin = reactable::colDef(
              name = "Nom latin",
              minWidth = 150,
              style = list(fontStyle = "italic")
            ),
            code_id = reactable::colDef(
              name = "Code",
              minWidth = 80
            )
          ),
          defaultColDef = reactable::colDef(
            align = "left",
            headerStyle = list(background = "#f7f7f8")
          )
        )
      })
    }

    # Return functions to be called from other modules
    return(list(
      show_functional_group_table = show_functional_group_table,
      show_habitat_table = show_habitat_table
    ))
  })
}
