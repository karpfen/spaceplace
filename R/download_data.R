#' Download OSM data and return as sf object
#'
#' Download OSM data with specified parameters and return sf object.
#'
#' @param bbox \code{vector} of corner coordinates of the area to download.
#' @param crs output coordinate reference system.
#' @param geom_type OSM geometry type.
#' @param key OSM key to download.
#' @param value OSM value to download.
#' @param timeout (optional) overpass timeout time in seconds.
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
osm_to_sf <- function (bbox, crs, geom_type, key, value, timeout = 25)
{
  feat <- osmdata::opq (bbox = bbox, timeout = timeout)
  if (missing (value))
  {
    feat <- osmdata::add_osm_feature (feat, key = key)
  } else {
    feat <- osmdata::add_osm_feature (feat, key = key, value = value)
  }

  feat <- sf::st_transform (sf::st_as_sf (osmdata::osmdata_sf(feat) [[geom_type]]), crs)

  return (feat)
}

#' Download OSM data, store it and and return as sf object
#'
#' Wrapper function for osm_to_sf that creates a unique file name from the
#' passed parameters, downloads the requested data if necessary and returns the
#' resulting sf object.
#'
#' @param bbox \code{vector} of corner coordinates of the area to download.
#' @param crs output coordinate reference system.
#' @param geom_type OSM geometry type.
#' @param key OSM key to download.
#' @param value OSM value to download.
#' @param clip_by_outline (optional) city name based on which the outline should be cut.
#' @param timeout (optional) overpass timeout time in seconds.
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
#' clip_by <- "London"
#'
#' bus_stops <- load_osm_data (bb, crs, geom_type, key, value, clip_by)
#' }
load_osm_data <- function (bbox, crs, geom_type, key, value, clip_by_outline,
                           timeout = 25)
{
  fname <- paste0 (paste (c (crs, geom_type, key, value), collapse = "-"),
                   ".gpkg")

  lyr <- paste (key, value, sep = " - ")

  dat <- NA
  if (!fname %in% dir())
  {
    dat <- osm_to_sf(bbox, crs, geom_type, key, value, timeout)
    dat <- dat [c ("name", "geometry")]
    sf::st_write (dat, layer = lyr, fname)
  }
  dat <- sf::read_sf(fname)

  if (exists(clip_by_outline))
  {
    aoi <- get_city_outline (clip_by_outline)
    keep <- sf::st_contains(sf::st_geometry(aoi), sf::st_geometry(dat)) [[1]]
    dat <- dat [keep, ]
  }
  return (dat)
}

#' Download city outline from OSM and return as sf object
#'
#' @param city_name Name of the city outline to download.
#'
#' @return \code{sf} object of downloaded data.
#'
#' @export
get_city_outline <- function (city_name)
{
  osm_l <- osmdata::getbb (city_name, format_out = "polygon") [[1]] [[1]]
  wkt <- paste0 ("SRID=4326;POLYGON((",
                 paste (paste (osm_l [, 1], osm_l [, 2]), collapse
                        = ","),
                 "))")
  aoi <- sf::st_set_crs (sf::st_as_sfc (wkt), 4326)
  return (aoi)
}
