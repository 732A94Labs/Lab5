library(httr2)
library(jsonlite)
library(sf)

#' Fetches a list of all available countries
#' @export
fetch_countries <- function() {
  overpass <- "https://overpass-api.de/api/interpreter"
  q <- '
  [out:csv(
  ::type, ::id, type, boundary, land_area, "ISO3166-1", "name:en", "name",
  ::count; true; ","
  )];
  (
  relation["type"="boundary"]["boundary"="administrative"]["admin_level"="2"];
  relation["type"="land_area"]["admin_level"="2"];
  );
  out;
  out count;
  '

  resp <- request(overpass) |>
    req_user_agent("YourAppName/1.0 (contact@example.com)") |>
    req_method("POST") |>
    req_body_raw(charToRaw(q)) |>
    req_timeout(seconds = 60) |>
    req_perform()

  txt <- resp_body_string(resp)

  df <- read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE)

  return(df)
}

#' Fetches a countrie's geometry by its id from the country list
#' @export
fetch_country_geom_by_relation <- function(rel_id) {
  req <- request("https://nominatim.openstreetmap.org/lookup") |>
    req_user_agent("YourAppName/1.0 (contact@example.com)") |>
    req_url_query(
      osm_ids = paste0("R", rel_id),
      format = "json",
      polygon_geojson = 1
    )

  resp <- req_perform(req)
  stopifnot(resp_status(resp) == 200)

  dat <- resp_body_json(resp, simplifyVector = TRUE)

  # geojson comes as a small data.frame with columns type + coordinates
  geom <- list(
    type = dat$geojson$type[1],
    coordinates = dat$geojson$coordinates[[1]]
  )

  # Wrap into a FeatureCollection, then read with sf
  feature <- list(type="Feature", properties=list(osm_id=rel_id), geometry=geom)
  fc <- list(type="FeatureCollection", features=list(feature))
  tmp <- tempfile(fileext = ".geojson")
  writeLines(toJSON(fc, auto_unbox = TRUE), tmp)
  st_read(tmp, quiet = TRUE) |> st_make_valid()
}
