

team_8 <- function(file, tolerance = 0.1){
  geometry <- oz$geometry
  # In order to receive list of matrices, we used the flatten
  matrix <- geometry %>% flatten() %>% flatten()
  # By using  purrr functionality, we create the dataframe
  ozplus <- matrix %>% purrr::map_df(.x = ., .id ="group", .f = helper)
  list(country = oz$NAME_1[10])
}

