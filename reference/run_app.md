# Run the Shiny Application

Run the Shiny Application

## Usage

``` r
run_app(
  onStart = NULL,
  options = list(),
  enableBookmarking = "url",
  uiPattern = "/",
  ...
)
```

## Arguments

- onStart:

  A function that will be called before the app is actually run. This is
  only needed for `shinyAppObj`, otherwise this is passed to the
  `options` argument.

- options:

  Named options that should be passed to the `runApp` call (these can be
  any of the following: "port", "launch.browser", "host", "quiet",
  "display.mode" and "test.mode"). You can also specify `width` and
  `height` parameters which provide a hint to the embedding environment
  about the ideal height/width for the app.

- enableBookmarking:

  Can be one of "url", "server", or "disable". This is equivalent to
  calling the `enableBookmarking()` function just prior to calling
  `shinyApp()`. With the default value, `NULL`, the app will respect the
  setting from any previous calls to `enableBookmarking()`.

- uiPattern:

  A regular expression that will be applied to each `GET` request to
  determine whether the `ui` should be used to handle the request. Note
  that the entire request path must match the regular expression in
  order for the match to be considered successful.

- ...:

  arguments to pass to golem_opts. See
  [`?golem::get_golem_options`](https://thinkr-open.github.io/golem/reference/get_golem_options.html)
  for more details.
