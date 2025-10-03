library(testthat)
library(httptest2)
library(sf)

test_that("fetch_countries parses Overpass CSV into a data.frame", {
  fake_csv <- paste(
    '"::type","::id","type","boundary","land_area","ISO3166-1","name:en","name","::count"',
    '"relation","12345","boundary","administrative","","DE","Germany","Deutschland","1"',
    sep = "\n"
  )

  with_mocked_bindings(
    {
      df <- fetch_countries()

      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 1)
      expect_true(all(c("::type","::id","type","boundary","land_area",
                        "ISO3166-1","name:en","name","::count") %in% names(df)))
      expect_equal(df[1, "ISO3166-1"], "DE")
      expect_equal(df[1, "name:en"], "Germany")
      expect_identical(as.character(df[1, "::id"]), "12345")
    },
    .package = "httr2",
    req_perform      = function(...) structure(list(), class = "httr2_response"),
    resp_body_string = function(...) fake_csv
  )
})

test_that("fetch_countries still returns a data.frame on empty body (zero rows)", {
  empty_csv <- '"::type","::id","type","boundary","land_area","ISO3166-1","name:en","name","::count"\n'

  with_mocked_bindings(
    {
      df <- fetch_countries()
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 0)
      expect_true(all(c("::type","::id","type","boundary","land_area",
                        "ISO3166-1","name:en","name","::count") %in% names(df)))
    },
    .package = "httr2",
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
      # osm_id may come back as character or numeric depending on GDAL; be permissive
      expect_true(all(as.character(g$osm_id) == as.character(rel_id)))

      expect_true(all(st_is_valid(g)))
      expect_equal(as.character(sf::st_geometry_type(g))[1], "POLYGON")
    },
    .package = "httr2",
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
    .package = "httr2",
    req_perform = function(...) structure(list(), class = "httr2_response"),
    resp_status = function(...) 500L
  )
})

test_that("fetch_country_geom_by_relation errors cleanly if JSON lacks geojson", {
  bad_json <- list(other = "thing")

  # Outer: mock sf::st_read so that if it gets called, we force an error.
  with_mocked_bindings(
    {
      # Inner: mock httr2 bindings to supply the bad JSON and 200 status.
      with_mocked_bindings(
        {
          expect_error(
            fetch_country_geom_by_relation(123L),
            regexp = "invalid geojson|is not TRUE|geojson|subsettable|\\$ operator is invalid|subscript",
            info = "Function should fail when expected geojson field is absent"
          )
        },
        .package = "httr2",
        req_perform    = function(...) structure(list(), class = "httr2_response"),
        resp_status    = function(...) 200L,
        resp_body_json = function(..., simplifyVector = TRUE) bad_json
      )
    },
    .package = "sf",
    st_read = function(...) stop("invalid geojson")
  )
})
