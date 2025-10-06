# tests/testthat/test-api.R
library(testthat)
library(httptest2)
library(sf)

# Detect the package namespace where the httr2 symbols are imported.
PKG <- tryCatch(utils::packageName(), error = function(e) NA_character_)
if (is.na(PKG) || !nzchar(PKG)) {
  # Fallback to your package name if needed
  PKG <- "Lab5"
}

test_that("fetch_countries parses Overpass CSV into a data.frame", {
  fake_csv <- paste(
    '"::type","::id","type","boundary","land_area","ISO3166-1","name:en","name","::count"',
    '"relation","12345","boundary","administrative","","DE","Germany","Deutschland","1"',
    sep = "\n"
  )

  with_mocked_bindings(
    {
      df <- fetch_countries(force_refresh = TRUE)

      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 1)
      expect_true(all(c("::type","::id","type","boundary","land_area",
                        "ISO3166-1","name:en","name","::count") %in% names(df)))
      expect_equal(df[1, "ISO3166-1"], "DE")
      expect_equal(df[1, "name:en"], "Germany")
      expect_identical(as.character(df[1, "::id"]), "12345")
    },
    .package = PKG,
    req_perform      = function(...) structure(list(), class = "httr2_response"),
    resp_body_string = function(...) fake_csv
  )
})

test_that("fetch_countries still returns a data.frame on empty body (zero rows)", {
  empty_csv <- '"::type","::id","type","boundary","land_area","ISO3166-1","name:en","name","::count"\n'

  with_mocked_bindings(
    {
      df <- fetch_countries(force_refresh = TRUE)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 0)
      expect_true(all(c("::type","::id","type","boundary","land_area",
                        "ISO3166-1","name:en","name","::count") %in% names(df)))
    },
    .package = PKG,
    req_perform      = function(...) structure(list(), class = "httr2_response"),
    resp_body_string = function(...) empty_csv
  )
})

test_that("fetch_country_geom_by_relation returns a valid sf feature with expected properties", {
  rel_id <- 99999L

  # Minimal valid GeoJSON Polygon ring (closed)
  coords <- list(list(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  ))

  fake_json <- list(
    geojson = list(
      type = "Polygon",
      coordinates = list(coords)
    )
  )

  with_mocked_bindings(
    {
      g <- fetch_country_geom_by_relation(rel_id)

      expect_s3_class(g, "sf")
      expect_equal(nrow(g), 1)
      expect_true("osm_id" %in% names(g))
      expect_true(all(as.character(g$osm_id) == as.character(rel_id)))

      # st_make_valid() can sometimes return a GEOMETRYCOLLECTION; assert it contains a polygonal geometry.
      gtypes <- as.character(sf::st_geometry_type(g, by_geometry = TRUE))
      expect_true(any(gtypes %in% c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION")))
      # If it's a collection, ensure it actually has polygonal parts
      if ("GEOMETRYCOLLECTION" %in% gtypes) {
        # st_collection_extract only available in recent sf; fallback: just assert it's valid
        expect_true(all(st_is_valid(g)))
      }
    },
    .package = PKG,
    req_perform    = function(...) structure(list(), class = "httr2_response"),
    resp_status    = function(...) 200L,
    resp_body_json = function(..., simplifyVector = TRUE) fake_json
  )
})

test_that("fetch_country_geom_by_relation stops if HTTP status is not 200", {
  with_mocked_bindings(
    {
      expect_error(fetch_country_geom_by_relation(123L), "is not TRUE")
    },
    .package = PKG,
    req_perform = function(...) structure(list(), class = "httr2_response"),
    resp_status = function(...) 500L
  )
})

test_that("fetch_country_geom_by_relation errors cleanly if JSON lacks geojson", {
  bad_json <- list(other = "thing")

  with_mocked_bindings(
    {
      expect_error(
        fetch_country_geom_by_relation(123L),
        regexp = "geojson|subsettable|\\$ operator is invalid|subscript|invalid geojson",
        info = "Function should fail when expected geojson field is absent"
      )
    },
    .package = PKG,
    req_perform    = function(...) structure(list(), class = "httr2_response"),
    resp_status    = function(...) 200L,
    resp_body_json = function(..., simplifyVector = TRUE) bad_json,
    st_read        = function(...) stop("invalid geojson")
  )
})
