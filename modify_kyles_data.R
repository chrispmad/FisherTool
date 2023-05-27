library(terra)
library(sf)
library(tidyverse)

den_r = rast('data/denning2021.tif')
open_r = rast('data/open2021.tif')
movement_r = rast('data/movement2021.tif')
rest_cwd_r = rast('data/rest_cwd2021.tif')
rest_cavity_r = rast('data/rest_cavity2021.tif')
rest_rust_r = rast('data/rest_rust2021.tif')
movp_r = rast('data/movp.tif')

# Combine rasters into 1 mega-raster.
megaraster = c(den_r,movement_r,rest_cwd_r,rest_rust_r,rest_cavity_r,open_r,movp_r)
names(megaraster) <- c('denning','mov','cwd','rust','cavity','opn','movp')

writeRaster(megaraster,
            'data/megaraster.tif',overwrite=TRUE)
