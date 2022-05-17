# 


library(tidyverse)
library(sf)
library(raster)
library(patchwork)

t_map <- ggp::t_manu() %+replace%
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


out_path <- ggp::fig_set_output("map_sentinel_locations")
gee_path = "/Users/gopal/Google Drive/_Research projects/ML/ndviML/ndviML_GEE"

files <- list.files(gee_path)

tif_files <- files[grepl('\\.tif',files)]
loc_all <- gsub(".*loc_([0-9]+)\\.tif","\\1",tif_files)

for (loc in loc_all) {
  # loc <- 1
  
  s1_filepath <- file.path(gee_path, paste0("s1_loc_",loc,"_ts.csv"))
  pts_filepath <- file.path(gee_path, paste0("s2_loc_",loc,"_pts.csv"))
  tif_filepath <- file.path(gee_path, paste0("sentinel_loc_",loc,".tif"))
  im_filename <- paste0("im_loc_",loc,"_s2.png")
  figs_filename <- paste0("im_loc_",loc,"_figs.png")
  
  ras <- stack(tif_filepath)
  
  any_clouds <- max(values(ras$cloudmask))>0
    
  ras$clouds[ras$cloudmask==0] <- NA
  ras$cloudmask[ras$cloudmask==0] <- NA
  ras$shadows[ras$cloudmask==0] <- NA
  
  pts <- read_csv(pts_filepath) %>%
    mutate(x = as.numeric(gsub(".*\\[([0-9\\.]+),.*","\\1",.geo)),
           y = as.numeric(gsub(".*\\[[0-9\\.]+,([0-9\\.]+)\\].*","\\1",.geo))) %>%
    dplyr::select(-`.geo`)
  
  loc_fig <- ggplot() +
    RStoolbox::ggRGB(ras, r = 'B8', g = 'B4', b = 'B3', limits = c(1000, 3000), ggLayer = TRUE) +
    geom_point(data = pts, aes(x, y),shape = 1) + 
    coord_sf() +
    ggtitle(paste('Location', loc,': Sentinel-2')) +
    t_map
  
  loc_fig_tcc <- ggplot() +
    RStoolbox::ggRGB(ras, r = 'B4', g = 'B3', b = 'B2', limits = c(1000, 3000), ggLayer = TRUE) +
    geom_point(data = pts, aes(x, y),shape = 1,alpha = 0.01) +
    coord_sf() +
    ggtitle(paste('True color composite')) +
    t_map
  
  s1_fig <- ggplot() +
    RStoolbox::ggRGB(ras, r = 'VH', g = 'VV', b = 'VV', limits = c(-150000, 50000), ggLayer = TRUE) +
    geom_point(data = pts, aes(x, y),shape = 1, color = 'white',alpha = 0.01) +
    coord_sf() +
    ggtitle(paste('Sent-1')) +
    t_map
  
  s1_fig_ep <- ggplot() +
    RStoolbox::ggRGB(ras, r = 'VH_ep', g = 'VV_ep', b = 'VV_ep', limits = c(-150000, 50000), ggLayer = TRUE) +
    geom_point(data = pts, aes(x, y),shape = 1, color = 'white',alpha = 0.01) +
    coord_sf() +
    ggtitle(paste('S1 conv')) +
    t_map
  
  
  if (any_clouds) {
    loc_fig_clouds <- ggplot() +
      RStoolbox::ggRGB(ras, r = 'B8', g = 'B4', b = 'B3', limits = c(1000, 3000), ggLayer = TRUE) +
      RStoolbox::ggRGB(ras, r = 'shadows', g = 'clouds', b = 'cloudmask', limits = c(0, 1), ggLayer = TRUE) +
      geom_point(data = pts, aes(x, y),shape = 1,alpha = 0.01) + 
      coord_sf() +
      ggtitle(paste('Loc', loc, 'clouds')) +
      t_map
  } else {
    loc_fig_clouds <- loc_fig
  }
  loc_figs <- (loc_fig_clouds / s1_fig) | (loc_fig_tcc / s1_fig_ep) 
  
  ggsave(im_filename, loc_fig, path = out_path, width = 5, height = 5)
  ggsave(figs_filename, loc_figs, path = out_path, width = 5, height = 5)
}

