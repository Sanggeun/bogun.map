library(tidyverse)
library(viridis)
library(rgeos)

data_to_map <- function(map, data, merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "dong"){
  map@data$id <- rownames(map@data)
  map_f <- fortify(map, region = "id")
  
  merge_data <- merge(map@data[, c(map_key, merge_key)], data, by.x=merge_key, by.y = data_key)
  
  map@data <- left_join(map@data, merge_data, by = map_key)
  map_f_d <- left_join(map_f, map@data, by = "id")
  return(map_f_d)
}


plot_map <- function(map , re_var, index_type, 
                     path_color = "white", legend_label, 
                     color_type = "blue_red", color_good = "blue", color_bad = "red", fixed_mid = NULL) {
  
  map$re_var <- map[[re_var]]
  
  color <- c(color_bad, color_good)
  midpoint<- median(map$re_var)
  range_rate <- range(map$re_var)
  
  if (index_type != 1) {
    color <- c(color_good, color_bad)
  }
  if(!is.null(fixed_mid)) {
    midpoint <- fixed_mid
  }
  
  return (
    ggplot(map) +
      aes(long, lat, group = group, fill = re_var) +
      geom_polygon() +
      geom_path(color = path_color) +
      coord_equal() +
      labs(x="", y="", fill = legend_label) +
      scale_x_continuous(labels=NULL) +
      scale_y_continuous(labels = NULL) +
      theme_bw() +
      if (color_type == "viridis") {
        scale_fill_viridis(direction = index_type) 
      } else if (color_type == "blue_red") {
        scale_fill_gradient2(low = color[1], mid ="white", midpoint = midpoint,
                             high = color[2], limits = c(range_rate[1], range_rate[2]))
      }
    
  )
  
}