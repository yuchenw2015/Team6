#####################################
## I'm using lab 2 team 3 solution ##
#####################################

team_3 <- function(file, tolerance){

  con_big <- read_sf(dat)
  con_st <- maptools::thinnedSpatialPoly(as(con_big, "Spatial"), tolerance = 0.1,
                                         minarea = 0.001, topologyPreserve = TRUE)
  con <- st_as_sf(con_st)
  con_plus <- con$geometry %>% flatten() %>% flatten() %>% lapply(., as.data.frame) %>% bind_rows() %>% rename(., c("x" = "long", "y" = "lat"))
  temp <- con$geometry %>% flatten() %>% flatten() %>% lapply(., nrow) %>% unlist
  con_plus$group <- rep(1 : length(temp), temp)

  return(con_plus)
}
