#' Plot a country shape with .shp file.
#'
#' @param file The name of the file which the data are to be read from. The file should be in shp format.
#' @param tolerance The tolerance level to be used in thinning the origninal data. The default value is 0.1. The original data usually is too large to run. Larger tolerance value makes the data thinner.
#' @export
#' @return A list includes a vector of country name, a vector of the states name, and a data frame of the polygon longitudes and latitudes.
#' @examples
#' #Find the the path "fp" of your own shp file, then call team_1(fp) or team_(fp, 0.2)


team_1 <- function(file, tolerance = 0.1){
  library(class)
  file_format <- substr(file, nchar(file) - 2, nchar(file))
  assertthat::assert_that(file_format == "shp", msg = "The file is not .shp format!")
  ozbig <- sf::read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- sf::st_as_sf(oz_st)
  assertthat::assert_that(!is.null(oz), msg = "Fail to thinner the data!")
  assertthat::assert_that(!is.null(oz$NAME_0), msg = "The data does not include column 'NAME_0' which is the name of the country!")
  assertthat::assert_that(!is.null(oz$NAME_1), msg = "The data does not include column 'NAME_1' which is the name of the states!")
  assertthat::assert_that(is.list(oz$geometry), msg = "geometry is not a list!" )
  assertthat::assert_that(is.matrix(oz$geometry[[1]][[1]][[1]]), msg = "geometry is not a list (with depth = 3) of matrices!" )
  df.oz.purr <- oz$geometry %>%
    purrr::map_depth(3, data.frame) %>%
    purrr::flatten() %>%
    purrr::flatten() %>%
    dplyr::bind_rows(.id = "group") %>%
    dplyr::rename("lat" = y, "long" = x)
  assertthat::assert_that(!is.null(df.oz.purr), msg = "Fail to flatten list!")
  assertthat::assert_that(is.data.frame(df.oz.purr), msg = "Fail to generate the data frame!")
  assertthat::assert_that(!is.null(df.oz.purr$group), msg = "Fail to generate the column 'group'!")
  assertthat::assert_that(!is.null(df.oz.purr$lat), msg = "Fail to generate the column 'lat' (latitude)!")
  assertthat::assert_that(!is.null(df.oz.purr$lat), msg = "Fail to generate the column 'long' (longitude)!")
  list(country = oz$NAME_0[1], state = oz$NAME_1, polygon = df.oz.purr)
}
