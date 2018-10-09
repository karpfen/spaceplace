#' Download OSM data and return as sf object
#'
#' Download OSM data with specified parameters and return sf object.
#'
#' @param bbox \code{vector} of corner coordinates of the area to download.
#' @param crs output coordinate reference system.
#' @param geom_type OSM geometry type.
#' @param key OSM key to download
#' @param value OSM value to download
#'
#' @return \code{sf} object of downloaded data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # London
#' bb <- c (-0.90, 51.30, 0.0, 51.40)
#' crs <- 32630
#' geom_type <- "osm_points"
#' key <- "highway"
#' value <- "bus_stop"
#'
#' bus_stops <- osm_to_sf (bb, crs, geom_type, key, value)
#' }

osm_to_sf <- function (bbox, crs, geom_type, key, value)
{
  feat <- osmdata::opq (bbox = bbox)
  if (missing (value))
  {
    feat <- osmdata::add_osm_feature (feat, key = key)
  } else {
    feat <- osmdata::add_osm_feature (feat, key = key, value = value)
  }

  feat <- sf::st_transform (sf::st_as_sf (osmdata::osmdata_sf(feat) [[geom_type]]), crs)

  return (feat)
}
