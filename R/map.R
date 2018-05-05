# devtools::install_github("cardiomoon/Kormaps")
# install.packages("tmap")
# install.packages("cartogram")

library(tmap)
library(cartogram)

windowsFont()

source("./R/general_function.R")


map_dong <- readRDS("./data/dong_2014.rds")
map_bogun <- readRDS("./data/bogun_2014.rds")
map_dong_dg <- map_dong[substr(map_dong$adm_dr_cd, 1, 2) == "22", ]
map_dong_dg@data$adm_dr_nm <-
  trim(underbar_change(map_dong_dg@data$adm_dr_nm))

map_district_dg <- readRDS("./data/district_dg.rds")

# making map




# by district

# mapping data
library(viridis)

walking_dong <- read.csv("./data/걷기 실천율_동별.csv")



data_to_map <- function(map, data, merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "dong"){
  map@data$id <- rownames(map@data)
  map_f <- fortify(map, region = "id")
  
  merge_data <- merge(map@data[, c(map_key, merge_key)], data, by.x=merge_key, by.y = data_key)
  
  map@data <- left_join(map@data, merge_data, by = map_key)
  map_f_d <- join(map_f, map@data, by = "id")
  return(map_f_d)
}


plot_map <- function(map , re_var, index_type, 
                     path_color = "white", legend_label, 
                     color_type = "1", color_good = "blue", color_bad = "red", fixed_mid = NULL) {
  
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
    } else if (color_type == "1") {
      scale_fill_gradient2(low = muted(color[1]), mid ="white", midpoint = midpoint,
                           high = muted(color[2]), limits = c(range_rate[1], range_rate[2]))
    }
  
)

}

walking_map <- data_to_map(map_dong_dg, walking_dong, 
                           merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "dong")

a <- plot_map(walking_map, re_var = "c_rate", index_type = 1, legend_label = "조율(%)",
         path_color = "white", color_type = "1")



## submap


## points, line, 


