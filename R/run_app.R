#' Run the Shiny Application
#'
#' @param onStart A function that will be called before the app is actually run.
#' This is only needed for `shinyAppObj`, otherwise this is passed to the
#' `options` argument.
#' @param options Named options that should be passed to the `runApp` call
#' (these can be any of the following: "port", "launch.browser", "host", "quiet",
#' "display.mode" and "test.mode"). You can also specify `width` and `height`
#' parameters which provide a hint to the embedding environment about the ideal
#' height/width for the app.
#' @param enableBookmarking Can be one of "url", "server", or "disable". This
#' is equivalent to calling the `enableBookmarking()` function just prior to
#' calling `shinyApp()`. With the default value, `NULL`, the app will respect
#' the setting from any previous calls to  `enableBookmarking()`.
#' @param uiPattern A regular expression that will be applied to each `GET`
#' request to determine whether the `ui` should be used to handle the
#' request. Note that the entire request path must match the regular
#' expression in order for the match to be considered successful.
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#'
#' @export
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = "url",
    uiPattern = "/",
    ...) {
    golem::with_golem_options(
      app = shinyApp(
        ui = app_ui,
        server = app_server,
        options = options,
        enableBookmarking = enableBookmarking,
        uiPattern = uiPattern
      ),
      golem_opts = list(...)
    )
}
