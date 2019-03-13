team_1 <- function(file, tolerance = 0.1){
  ozbig <- sf::read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- sf::st_as_sf(oz_st)
  df.oz.purr <- oz$geometry %>%
    purrr::map_depth(3, data.frame) %>%
    purrr::flatten() %>%
    purrr::flatten() %>%
    dplyr::bind_rows(.id = "group") %>%
    rename("lat" = y, "long" = x)
  list(country = oz$NAME_0[1], polygon = df.oz.purr)
}
