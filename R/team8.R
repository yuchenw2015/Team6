
#' Plot a state shape in Australia. 
#'
#' @param file The name of the file includes the data (shp. format).
#' @param tolerance The tolerance level to thinen the origninal data. The default value is 0.1. The larger tolerance value, the thinner data.
#' @export
#' @return A list includes a vector of state name, and a data frame shows the polygon of longitudes and latitudes.
#' @examples
#' #Find the the path "fp" of your own shp file, then call team8(fp)


helper <- function(y) {
  data <- data.frame(long = y[, 1],
                     lat  = y[, 2],
                     groups = rep(rnorm(1),nrow(y)),
                     order = 1:nrow(y))
  data
  assertthat::assert_that(is.data.frame(data), msg = "Fail to return the order!")
  assertthat::assert_that(is.null(data$order), msg = "Fail to generate the data frame!")
  assertthat::assert_that(!is.null(data$groups), msg = "Fail to generate the column 'groups'!")
  assertthat::assert_that(!is.null(data$long), msg = "Fail to generate the column 'long' (longitude)!")
  assertthat::assert_that(!is.null(data$lat), msg = "Fail to generate the column 'lat' (latitude)!")
}


team_8 <- function(file, tolerance = 0.1){
  library(class)
  file_format <- substr(file, nchar(file) - 2, nchar(file))
  assertthat::assert_that(file_format == "shp", msg = "The file is not a .shp format!")
  ozbig <- sf::read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- sf::st_as_sf(oz_st)
  
  geometry <- oz$geometry
  matrix <- geometry %>% flatten() %>% flatten()
  ozplus <- matrix %>% purrr::map_df(.x = ., .id ="group", .f = helper)
  list(country = oz$NAME_1[10], polygon = ozplus)
  assertthat::assert_that(!is.null(geometry), msg = "Fail to flatten list of matrices!")
  assertthat::assert_that(is.data.frame(geometry), msg = "Fail to generate the data frame!")
  assertthat::assert_that(!is.null(oz$NAME_1), msg = "The data does not include column 'NAME_1' which is the name of the states!")
  assertthat::assert_that(is.list(oz$geometry), msg = "geometry is not a list!" )
}
