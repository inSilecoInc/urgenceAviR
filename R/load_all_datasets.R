#' Load and process all datasets
#'
#' This function sequentially calls all the `load_*` dataset processing functions
#' and combines their outputs into a single `data.frame` or a list of `data.frames`.
#'
#' @param combine Logical, if TRUE (default), combines all datasets into a single `data.frame`.
#'                If FALSE, returns a named list of individual `data.frames`.
#' @return A combined `data.frame` or a list of `data.frames` containing all processed datasets.
#' @examples
#' \dontrun{
#' all_data <- load_all_datasets()
#' }
#' @export
load_all_datasets <- function(combine = TRUE) {
    
    # List of dataset functions
    dataset_functions <- list(
        canards_de_mer = load_canards,
        eider_hiver = load_eider_hiver,
        garrot = load_garrot,
        macreuse = load_macreuse,
        oies = load_oies,
        sauvagine_fleuve = load_sauvagine_fleuve
    )
    
    cli::cli_h1("Loading all datasets")
    
    # Initialize an empty list to store results
    datasets <- list()
    
    # Call each function and store its result
    for (name in names(dataset_functions)) {
        tryCatch(
            {
                datasets[[name]] <- dataset_functions[[name]]()
            },
            error = function(e) {
                cli::cli_alert_danger("Failed to load dataset: {name}. Error: {e$message}")
            }
        )
    }
    
    # Combine datasets if requested
    if (combine) {
        cli::cli_h1("Combining datasets")
        combined_data <- dplyr::bind_rows(datasets, .id = "dataset")
        cli::cli_alert_success("Successfully combined datasets")
        return(combined_data)
    }
    
    cli::cli_alert_success("Returning datasets as a list")
    return(datasets)
}
