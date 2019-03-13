#####################################
## I'm using lab 2 team 3 solution ##
#####################################

team_3 <- function(file, tolerance = 0.1){

  con_big <- sf::read_sf(file)
  con_st <- maptools::thinnedSpatialPoly(as(con_big, "Spatial"), tolerance = tolerance,
                                         minarea = 0.001, topologyPreserve = TRUE)
  con <- sf::st_as_sf(con_st)
  con_plus <- con$geometry %>% flatten() %>% flatten() %>% lapply(., as.data.frame) %>% bind_rows() %>% rename(., c("x" = "long", "y" = "lat"))
  temp <- con$geometry %>% flatten() %>% flatten() %>% lapply(., nrow) %>% unlist
  con_plus$group <- rep(1 : length(temp), temp)
  list(country = con$NAME_0[1], polygon = con_plus)
  return(con_plus)
}
