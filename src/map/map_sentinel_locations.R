# 

gee_path = "/Users/gopal/Google Drive/_Research projects/ML/ndviML/ndviML_GEE"

library(tidyverse)
library(sf)
library(raster)


out_path <- ggp::fig_set_output("map_sentinel_locations")

files <- list.files(gee_path)

tif_files <- files[grepl('\\.tif',files)]
loc_all <- gsub(".*loc_([0-9]+)\\.tif","\\1",tif_files)

for (loc in loc_all) {
  # loc <- 1
  
  s1_filepath <- file.path(gee_path, paste0("s1_loc_",loc,"_ts.csv"))
  pts_filepath <- file.path(gee_path, paste0("s2_loc_",loc,"_pts.csv"))
  tif_filepath <- file.path(gee_path, paste0("sentinel_loc_",loc,".tif"))
  im_filename <- paste0("im_loc_",loc,"_s2.png")
  
  ras <- stack(tif_filepath)
  
  pts <- read_csv(pts_filepath) %>%
    mutate(x = as.numeric(gsub(".*\\[([0-9\\.]+),.*","\\1",.geo)),
           y = as.numeric(gsub(".*\\[[0-9\\.]+,([0-9\\.]+)\\].*","\\1",.geo))) %>%
    dplyr::select(-`.geo`)
  
  loc_fig <- ggplot() +
    RStoolbox::ggRGB(ras, r = 'B8', g = 'B4', b = 'B3', limits = c(1000, 3000), ggLayer = TRUE) +
    geom_point(data = pts, aes(x, y),shape = 1) + 
    coord_sf() +
    ggtitle(paste('location', loc))
  
  ggsave(im_filename, loc_fig, path = out_path, width = 5, height = 5)
}

