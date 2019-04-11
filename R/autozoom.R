## autozoom

## Automatically setting zoom size and center lon/lat


autozoom <- function(dataset.name){
  dat <- get(dataset.name)
  long <- select(dat, starts_with("lon")) %>% mutate_all(as.numeric)
  lat <- select(dat, starts_with("lat")) %>% mutate_all(as.numeric)
  center_long <- (max(long[,1]) + min(long[,1]))/2
  center_lat <- (max(lat[,1]) + min(lat[,1]))/2
  range_lat <- range(lat)
  range_lat_diff <- diff(range_lat)
  range_long <- range(long)
  range_long_diff <- diff(range_long)
  range_list <- c(range_lat_diff, range_long_diff)
  range_list_index <- which.max(range_list)
  range_list_key <- range_list[range_list_index]
  crit <- c(-Inf,12.8*2^(-15:0))
  zoom <- 17 - as.numeric(cut(range_list_key, crit))
  list(center = c(center_long, center_lat), zoom=zoom)
}
