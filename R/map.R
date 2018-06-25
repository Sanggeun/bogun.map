# devtools::install_github("cardiomoon/Kormaps")
if (!"tmap" %in%  installed.packages()) install.packages("tmap")
if (!"cartogram" %in%  installed.packages()) install.packages("cartogram")

library(tidyverse)
library(tmap)
library(cartogram)

if (!require("extrafont")){install.packages("extrafont")}
library(extrafont)

windowsFont()
loadfonts()

source("R/general_function.R", encoding = "UTF-8")

map_dong <- readRDS("./data/dong_2014.rds")
map_bogun <- readRDS("./data/bogun_2014.rds")
map_dong_dg <- map_dong[substr(map_dong$adm_dr_cd, 1, 2) == "22", ]
map_dong_dg@data$adm_dr_nm <-
  trim(underbar_change(map_dong_dg@data$adm_dr_nm))

map_district_dg <- readRDS("./data/district_dg.rds")

map_dong_dg_010 <- map_dong_dg[substr(map_dong_dg$adm_dr_cd,1,5) == "22010", ]
map_dong_dg_020 <- map_dong_dg[substr(map_dong_dg$adm_dr_cd,1,5) == "22020", ]
map_dong_dg_030 <- map_dong_dg[substr(map_dong_dg$adm_dr_cd,1,5) == "22030", ]
map_dong_dg_040 <- map_dong_dg[substr(map_dong_dg$adm_dr_cd,1,5) == "22040", ]
map_dong_dg_050 <- map_dong_dg[substr(map_dong_dg$adm_dr_cd,1,5) == "22050", ]
map_dong_dg_060 <- map_dong_dg[substr(map_dong_dg$adm_dr_cd,1,5) == "22060", ]
map_dong_dg_070 <- map_dong_dg[substr(map_dong_dg$adm_dr_cd,1,5) == "22070", ]
map_dong_dg_310 <- map_dong_dg[substr(map_dong_dg$adm_dr_cd,1,5) == "22310", ]

saveRDS(map_dong_dg_010, "./data/dong_dg_010.rds")
saveRDS(map_dong_dg_020, "./data/dong_dg_020.rds")
saveRDS(map_dong_dg_030, "./data/dong_dg_030.rds")
saveRDS(map_dong_dg_040, "./data/dong_dg_040.rds")
saveRDS(map_dong_dg_050, "./data/dong_dg_050.rds")
saveRDS(map_dong_dg_060, "./data/dong_dg_060.rds")
saveRDS(map_dong_dg_070, "./data/dong_dg_070.rds")
saveRDS(map_dong_dg_310, "./data/dong_dg_310.rds")

map_bogun_dg<- map_bogun[map_bogun$bogun_cd %in% c("42","43","44","45","46","47","48","49"),]

saveRDS(map_bogun_dg, "./data/bogun_dg.rds")


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
  map_f_d <- left_join(map_f, map@data, by = "id")
  return(map_f_d)
}


plot_map <- function(map , re_var, index_type, 
                     path_color = "white", legend_label, 
                     color_type = "1", color_good = "blue", color_bad = "red", 
                     fixed_mid = NULL) {

  
  
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



return(
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
      scale_fill_gradient2(low = color[1], mid ="white", midpoint = midpoint,
                           high = color[2], limits = c(range_rate[1], range_rate[2]))
    } 
)

}



walking_map <- data_to_map(map_dong_dg, walking_dong, 
                           merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "dong")

plot_map(walking_map, re_var = "c_rate", index_type = 1, legend_label = "조율(%)",
         path_color = "white", color_type = "1")


## submap

a <- map_dong_dg[substr(map_dong_dg$adm_dr_cd,1,5) == "22010",]

walking_map_1 <- data_to_map(a, walking_dong, 
                             merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "dong")
plot_map(walking_map_1, re_var = "c_rate", index_type = 1, legend_label = "조율(%)",
         path_color = "white", color_type = "1")
## points, line, 


