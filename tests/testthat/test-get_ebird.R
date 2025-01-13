library(testthat)
library(terra)
library(glue)

# Mock data for testing
mock_path <- tempfile(fileext = ".gpkg")
mock_species <- c("Snow Goose", "Mallard")
mock_year <- 2012:2013
mock_month <- 4:5

# Mock GeoDatabase creation
longitude <- c(-116.7, -120.4, -116.7, -113.5)
latitude <- c(45.3, 42.6, 38.9, 42.1)
lonlat <- cbind(longitude, latitude)
mock_gdb <- vect(lonlat, crs = "+proj=longlat +datum=WGS84")
mock_gdb$OBSERVATION_DATE <- c("2012-04-01", "2012-05-01", "2013-04-01", "2013-05-01")
mock_gdb$COMMON_NAME <- c("Snow Goose", "Mallard", "Snow Goose", "Mallard")

suppressWarnings(
  writeVector(mock_gdb, mock_path, filetype = "GPKG", overwrite = TRUE)
)

### Tests for compose_query (internal function)
test_that("compose_query generates correct SQL query", {
  # Test with both year and month
  query <- compose_query(species = mock_species, year = mock_year, month = mock_month)
  expect_true(grepl("OBSERVATION_DATE LIKE '2012-04-%'", query))
  expect_true(grepl("COMMON_NAME IN \\('Snow Goose', 'Mallard'\\)", query))

  # Test with year only
  query_year <- compose_query(species = mock_species, year = mock_year, month = NULL)
  expect_true(grepl("OBSERVATION_DATE LIKE '2012-%'", query_year))

  # Test with species NULL
  query_no_species <- compose_query(species = NULL, year = mock_year, month = mock_month)
  expect_false(grepl("COMMON_NAME", query_no_species))
})

### Tests for get_ebird (exported function)
test_that("get_ebird retrieves filtered data from a GeoDatabase", {
  result <- get_ebird(path = mock_path, species = "Snow Goose", year = 2012:2013, month = 4)

  # Check result is a SpatVector
  expect_s4_class(result, "SpatVector")

  # Verify filtering worked correctly
  expect_equal(nrow(result), 2) # Two rows for Snow Goose in April
  expect_equal(unique(result$COMMON_NAME), "Snow Goose")

  # Test without species filter
  result_no_species <- get_ebird(path = mock_path, species = NULL, year = 2012:2013, month = 4)
  expect_equal(nrow(result_no_species), 2)
})
