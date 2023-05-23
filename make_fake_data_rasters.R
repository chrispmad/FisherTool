library(bcmaps)
library(sf)
library(tidyverse)
library(bcdata)
library(leaflet)
library(terra)

# ------------------------------------------
# Load and Filter Data!

# Name of variables - this might need correcting.
variable_name_df  = tibble(
  real_name = c('den_perc','branch_perc',
                'cwd_perc','cav_perc',
                'active_perc','open_perc'),
  label_name = c('Denning','Branch Resting',
                 'CWD Resting','Cavity Resting',
                 'Active','Open (less than)'))

bc = bcmaps::bc_bound() |>
  mutate(province = 'bc') |>
  dplyr::select(province)

# Pull in BEC zones. We can use these to approximate data areas for now, until we get
# data from Kyle.
if(!file.exists('data/sub_boreal_moist.gpkg')){
beczones = bcmaps::bec()

# Filter BEC zones for just moist sub-boreal (for now!)
# subb_dry = beczones |>
#   filter(str_detect(ZONE_NAME,"Sub-Boreal") & str_detect(SUBZONE_NAME,'Dry'))

subb_moist = beczones |>
  filter(str_detect(ZONE_NAME,"Sub-Boreal") & str_detect(SUBZONE_NAME,'Moist'))

subb_dry = beczones |>
  filter(str_detect(ZONE_NAME,"Sub-Boreal") & str_detect(SUBZONE_NAME,'Dry'))

# boreal = beczones |>
#   filter(str_detect(ZONE_NAME,"^Boreal"))

# Summarise to single polygon.
subb_moist = subb_moist |>
  summarise(bec_zone = 'sub-boreal moist')

subb_dry = subb_dry |>
  summarise(bec_zone = 'sub-boreal dry')

# boreal_s = boreal |>
#   summarise(bec_zone = 'boreal')

# Simplify the subb_moist shapefile... might be dangerous for data fidelity to do this!
subb_moist = rmapshaper::ms_simplify(subb_moist)
subb_dry = rmapshaper::ms_simplify(subb_dry)
# boreal_s = rmapshaper::ms_simplify(boreal_s)

ggplot() +
  geom_sf(data = bc) +
  geom_sf(data = subb_moist, fill = 'lightblue') +
  geom_sf(data = subb_dry, fill = 'gold')# +
  # geom_sf(data = boreal_s, fill = 'lightgreen')

write_sf(subb_moist, 'data/sub_boreal_moist.gpkg')
write_sf(subb_dry, 'data/sub_boreal_dry.gpkg')
# write_sf(boreal_s, 'data/boreal.gpkg')
} else {
  subb_moist = read_sf('data/sub_boreal_moist.gpkg')
  subb_dry = read_sf('data/sub_boreal_dry.gpkg')
}

# Get TSAs
if(!file.exists('data/all_tsa_polygons.gpkg')){

  all_layers = bcdata::bcdc_list()

  tsa_names = all_layers[str_detect(all_layers, 'fadm-timber-supply')]

  tsa_polys = bcdc_query_geodata(tsa_names[1]) |>
    collect()

  write_sf(tsa_polys, 'data/all_tsa_polygons.gpkg')
} else {tsa_polys = read_sf('data/all_tsa_polygons.gpkg')}

if(!file.exists('data/tsa_polygons_in_subb_dry.gpkg')){
# Remove TSAs that have been retired before today's date.
tsa_polys = tsa_polys |> filter(is.na(RETIREMENT_DATE))

# Simplify TSA polys at this point?
tsa_polys = rmapshaper::ms_simplify(tsa_polys)

# Identify which TSAs are completely overlapped by other, larger TSAs; remove the former.
intersection_table = tsa_polys |>
  dplyr::select(OBJECTID, FEATURE_AREA_SQM) |>
  st_join(tsa_polys |> dplyr::select(overlapping_id = OBJECTID),
          st_contains) |>
  st_drop_geometry() |>
  filter(OBJECTID != overlapping_id)

tsa_polys = tsa_polys |>
  filter(!OBJECTID %in% intersection_table$overlapping_id)

# Remove TSAs that fall outside of our area of interest.
tsa_subb_moist = tsa_polys |>
  st_join(subb_moist,
          st_intersects) |>
  filter(!is.na(bec_zone))

tsa_subb_dry = tsa_polys |>
  st_join(subb_dry,
          st_intersects) |>
  filter(!is.na(bec_zone))

# tsa_boreal = tsa_polys |>
#   st_join(boreal_s,
#           st_intersects) |>
#   filter(!is.na(bec_zone))

ggplot() +
  geom_sf(data = subb_moist, fill = 'brown') +
  geom_sf(data = tsa_subb_moist, fill = 'lightblue', alpha = 0.5)

ggplot() +
  geom_sf(data = subb_dry, fill = 'brown') +
  geom_sf(data = tsa_subb_dry, fill = 'lightblue', alpha = 0.5)


tsa_subb_moist = st_transform(tsa_subb_moist, crs = 4326)
tsa_subb_dry = st_transform(tsa_subb_dry, crs = 4326)
# tsa_boreal = st_transform(tsa_boreal, crs = 4326)

tsa_subb_moist = tsa_subb_moist |>
  mutate(map_label = coalesce(TSB_NUMBER_DESCRIPTION,TSA_NUMBER_DESCRIPTION))

tsa_subb_dry = tsa_subb_dry |>
  mutate(map_label = coalesce(TSB_NUMBER_DESCRIPTION,TSA_NUMBER_DESCRIPTION))

# tsa_boreal = tsa_boreal |>
#   mutate(map_label = coalesce(TSB_NUMBER_DESCRIPTION,TSA_NUMBER_DESCRIPTION))

write_sf(tsa_subb_moist, 'data/tsa_polygons_in_subb_moist.gpkg')
write_sf(tsa_subb_dry, 'data/tsa_polygons_in_subb_dry.gpkg')
# write_sf(tsa_boreal, 'data/tsa_polygons_in_boreal.gpkg')

} else {
  tsa_subb_moist = read_sf('data/tsa_polygons_in_subb_moist.gpkg')
  tsa_subb_dry = read_sf('data/tsa_polygons_in_subb_dry.gpkg')
}

# Example cutblocks
cutblock = read_sf('data/cutblock.gpkg') |>
  st_transform(crs = 3005)

if(!file.exists('data/hexagons_subb_moist.gpkg')){

  hexagon_grid_subb_moist = st_make_grid(subb_moist, square = F, cellsize = units::set_units(3e07, "m2"))
  hexagon_grid_subb_dry = st_make_grid(subb_dry, square = F, cellsize = units::set_units(3e07, "m2"))
  # hexagon_grid_boreal = st_make_grid(boreal_s, square = F, cellsize = units::set_units(3e07, "m2"))

  hexagon_grid_subb_moist = st_set_geometry(tibble(placeholder = rep(1,length(hexagon_grid_subb_moist))), hexagon_grid_subb_moist)
  hexagon_grid_subb_dry = st_set_geometry(tibble(placeholder = rep(1,length(hexagon_grid_subb_dry))), hexagon_grid_subb_dry)
  # hexagon_grid_boreal = st_set_geometry(tibble(placeholder = rep(1,length(hexagon_grid_boreal))), hexagon_grid_boreal)

  hexagon_grid_subb_moist$X = st_coordinates(st_centroid(hexagon_grid_subb_moist))[,1]
  hexagon_grid_subb_moist$Y = st_coordinates(st_centroid(hexagon_grid_subb_moist))[,2]

  hexagon_grid_subb_dry$X = st_coordinates(st_centroid(hexagon_grid_subb_dry))[,1]
  hexagon_grid_subb_dry$Y = st_coordinates(st_centroid(hexagon_grid_subb_dry))[,2]

  # hexagon_grid_boreal$X = st_coordinates(st_centroid(hexagon_grid_boreal))[,1]
  # hexagon_grid_boreal$Y = st_coordinates(st_centroid(hexagon_grid_boreal))[,2]

  hexagon_grid_subb_moist = hexagon_grid_subb_moist |>
    arrange(Y,X) |>
    mutate(ID = row_number()) |>
    dplyr::select(-X,-Y)

  hexagon_grid_subb_dry = hexagon_grid_subb_dry |>
    arrange(Y,X) |>
    mutate(ID = row_number()) |>
    dplyr::select(-X,-Y)

  # hexagon_grid_boreal = hexagon_grid_boreal |>
  #   arrange(Y,X) |>
  #   mutate(ID = row_number()) |>
  #   dplyr::select(-X,-Y)

  make_fake_data = function(dat, var_name){

    dat = dat |>
      mutate(base_values = rnorm(n = nrow(dat), mean = 0.5, sd = 0.1))
    # Make values a bit more similar, based on nearest neighbours?
    dat |>
      mutate(!!sym(var_name) := case_when(
        sample(c(0,1),nrow(dat),replace = T) == 1 ~ (base_values + lead(base_values) + lag(base_values))/3,
        T ~ base_values)) |>
      mutate(!!sym(var_name) := ifelse(is.na(!!sym(var_name)),
                                       base_values,
                                       !!sym(var_name))) |>
      dplyr::select(-base_values)
  }

  # Trim for area of interest and
  # add a column indicating which TSAs these hexagons are in.
  hexagon_grid_subb_moist = hexagon_grid_subb_moist |>
    st_join(subb_moist, st_intersects) |>
    filter(!is.na(bec_zone))

  hexagon_grid_subb_moist = hexagon_grid_subb_moist |>
    st_join(tsa_subb_moist |> st_transform(crs = 3005) |> dplyr::select(tsa_obj_id = OBJECTID), st_intersects) |>
    filter(!is.na(tsa_obj_id))

  write_sf(hexagon_grid_subb_moist, 'data/hexagons_subb_moist.gpkg')

  hexagon_grid_subb_dry = hexagon_grid_subb_dry |>
    st_join(subb_dry_s, st_intersects) |>
    filter(!is.na(bec_zone)) |>
    dplyr::select(-placeholder)

  hexagon_grid_subb_dry = hexagon_grid_subb_dry |>
    st_join(tsa_subb_dry |> st_transform(crs = 3005) |> dplyr::select(tsa_obj_id = OBJECTID), st_intersects) |>
    filter(!is.na(tsa_obj_id)) |>
    dplyr::select(-placeholder)

  write_sf(hexagon_grid_subb_dry, 'data/hexagons_subb_dry.gpkg')

  # hexagon_grid_boreal = hexagon_grid_boreal |>
  #   st_join(boreal_s, st_intersects) |>
  #   filter(!is.na(bec_zone))
  #
  # hexagon_grid_boreal = hexagon_grid_boreal |>
  #   st_join(tsa_boreal |> st_transform(crs = 3005) |> dplyr::select(tsa_obj_id = OBJECTID), st_intersects) |>
  #   filter(!is.na(tsa_obj_id))
  #
  # write_sf(hexagon_grid_boreal, 'data/hexagons_boreal.gpkg')

} else {
  hexagon_grid_subb_moist = read_sf('data/hexagons_subb_moist.gpkg')
  hexagon_grid_subb_dry = read_sf('data/hexagons_subb_dry.gpkg')
  }

# Make 100-hectare squares for our areas of interest.
if(!file.exists('data/subb_moist_pixels.gpkg')){

subb_moist_pixels = st_make_grid(x = hexagon_grid_subb_moist |> filter(!duplicated(ID)), cellsize = units::set_units(1000000, "m2"))
subb_dry_pixels = st_make_grid(x = hexagon_grid_subb_dry |> filter(!duplicated(ID)), cellsize = units::set_units(1000000, "m2"))
# boreal_pixels = st_make_grid(x = hexagon_grid_boreal |> filter(!duplicated(ID)), cellsize = units::set_units(1000000, "m2"))

subb_moist_pixels = st_set_geometry(tibble(placeholder = rep(1,length(subb_moist_pixels))), subb_moist_pixels)
subb_dry_pixels = st_set_geometry(tibble(placeholder = rep(1,length(subb_dry_pixels))), subb_dry_pixels)
# boreal_pixels = st_set_geometry(tibble(placeholder = rep(1,length(boreal_pixels))), boreal_pixels)

# Add in an ID column for the subb_moist_pixels
subb_moist_pixels$X = st_coordinates(st_centroid(subb_moist_pixels))[,1]
subb_moist_pixels$Y = st_coordinates(st_centroid(subb_moist_pixels))[,2]

subb_dry_pixels$X = st_coordinates(st_centroid(subb_dry_pixels))[,1]
subb_dry_pixels$Y = st_coordinates(st_centroid(subb_dry_pixels))[,2]

# boreal_pixels$X = st_coordinates(st_centroid(boreal_pixels))[,1]
# boreal_pixels$Y = st_coordinates(st_centroid(boreal_pixels))[,2]

subb_moist_pixels = subb_moist_pixels |>
  arrange(Y,X) |>
  mutate(ID = row_number()) |>
  dplyr::select(-X, -Y)

subb_dry_pixels = subb_dry_pixels |>
  arrange(Y,X) |>
  mutate(ID = row_number()) |>
  dplyr::select(-X, -Y)

# boreal_pixels = boreal_pixels |>
#   arrange(Y,X) |>
#   mutate(ID = row_number()) |>
#   dplyr::select(-X, -Y)

subb_moist_pixels = subb_moist_pixels |>
  mutate(den = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         branch = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         cwd = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         cav = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         active = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         open = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
  ) |>
  dplyr::select(-placeholder)

subb_dry_pixels = subb_dry_pixels |>
  mutate(den = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
         branch = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
         cwd = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
         cav = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
         active = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
         open = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
  ) |>
  dplyr::select(-placeholder)

# Just keep pixels that are inside the hexagons overlaying the BEC zone of interest.
subb_moist_pixels = subb_moist_pixels |>
  st_join(hexagon_grid_subb_moist |> filter(!duplicated(ID)) |> dplyr::select(hex_ID = ID)) |>
  filter(!is.na(hex_ID))

subb_dry_pixels = subb_dry_pixels |>
  st_join(hexagon_grid_subb_dry |> filter(!duplicated(ID)) |> dplyr::select(hex_ID = ID)) |>
  filter(!is.na(hex_ID))

# And also strip away any pixels that fall outside of the BEC zones!
subb_moist_pixels = subb_moist_pixels |>
  st_join(subb_moist) |>
  filter(!is.na(bec_zone))

subb_dry_pixels = subb_dry_pixels |>
  st_join(subb_dry) |>
  filter(!is.na(bec_zone))

# boreal_pixels = boreal_pixels |>
#   st_join(hexagon_grid_boreal |> dplyr::select(hex_ID = ID)) |>
#   filter(!is.na(hex_ID))

# boreal_pixels = boreal_pixels |>
#   mutate(den = sample(c(0,1),nrow(boreal_pixels), replace = T),
#          branch = sample(c(0,1),nrow(boreal_pixels), replace = T),
#          cwd = sample(c(0,1),nrow(boreal_pixels), replace = T),
#          cav = sample(c(0,1),nrow(boreal_pixels), replace = T),
#          active = sample(c(0,1),nrow(boreal_pixels), replace = T),
#          open = sample(c(0,1),nrow(boreal_pixels), replace = T),
#   ) |>
#   dplyr::select(-placeholder)

write_sf(subb_moist_pixels, 'data/subb_moist_pixels.gpkg')
write_sf(subb_dry_pixels, 'data/subb_dry_pixels.gpkg')
# write_sf(boreal_pixels, 'data/boreal_pixels.gpkg')

} else {
  subb_moist_pixels = read_sf('data/subb_moist_pixels.gpkg')
  subb_dry_pixels = read_sf('data/subb_dry_pixels.gpkg')
  }





## Attempting to make the pixels above, but as rasters.

# Moist:

hex_moist = hexagon_grid_subb_moist |>
  filter(!duplicated(ID))

subb_moist_r = rast(
  stars::st_rasterize(
    hex_moist,
    dx = 100, dy = 100
    )
  )

names(subb_moist_r)[1:2] <- c("ID","tsa_obj_id")

subb_moist_r = terra::mask(subb_moist_r,
            rast(stars::st_rasterize(hex_moist,
                                     dx = 100, dy = 100)))

hex_moist = st_transform(hex_moist, 4326)

subb_moist_r = project(subb_moist_r, hex_moist)

# Lastly, make some random values
subb_moist_r$den = sample(c(0,1),terra::ncell(subb_moist_r), replace = T)
subb_moist_r$branch = sample(c(0,1),terra::ncell(subb_moist_r), replace = T)
subb_moist_r$cwd = sample(c(0,1),terra::ncell(subb_moist_r), replace = T)
subb_moist_r$cav = sample(c(0,1),terra::ncell(subb_moist_r), replace = T)
subb_moist_r$active = sample(c(0,1),terra::ncell(subb_moist_r), replace = T)
subb_moist_r$open = sample(c(0,1),terra::ncell(subb_moist_r), replace = T)

terra::writeRaster(subb_moist_r, 'data/subb_moist_pixels.tif',
                   overwrite = T)

# Dry

hex_dry = hexagon_grid_subb_dry |>
  filter(!duplicated(ID))

subb_dry_r = rast(
  stars::st_rasterize(
    hex_dry,
    dx = 100, dy = 100
  )
)

names(subb_dry_r)[1:2] <- c("ID","tsa_obj_id")

subb_dry_r = terra::mask(subb_dry_r,
                           rast(stars::st_rasterize(hex_dry,
                                                    dx = 100, dy = 100)))

hex_dry = st_transform(hex_dry, 4326)

subb_dry_r = project(subb_dry_r, hex_dry)

# Lastly, make some random values
subb_dry_r$den = sample(c(0,1),terra::ncell(subb_dry_r), replace = T)
subb_dry_r$branch = sample(c(0,1),terra::ncell(subb_dry_r), replace = T)
subb_dry_r$cwd = sample(c(0,1),terra::ncell(subb_dry_r), replace = T)
subb_dry_r$cav = sample(c(0,1),terra::ncell(subb_dry_r), replace = T)
subb_dry_r$active = sample(c(0,1),terra::ncell(subb_dry_r), replace = T)
subb_dry_r$open = sample(c(0,1),terra::ncell(subb_dry_r), replace = T)

terra::writeRaster(subb_dry_r, 'data/subb_dry_pixels.tif',
                   overwrite = T)

# leaflet() |>
#   addTiles() |>
#   addPolygons(
#     color = 'black',
#     weight = 1,
#     fillColor = 'transparent',
#     data = hex_moist |> filter(ID == 95)) |>
#     addRasterImage(raster::raster(subb_moist_r[subb_moist_r$ID==95]))


terra::writeRaster(subb_moist_r, 'data/raster_size_check.tif',
                   overwrite = T)


subb_moist_r[subb_moist_r$tsa_obj_id == 120301]
subb_moist_r = rast(x = hexagon_grid_subb_moist |> filter(!duplicated(ID)), resolution = 1000)
rasterize(hexagon_grid_subb_moist, subb_moist_r)
subb_dry_pixels = st_make_grid(x = hexagon_grid_subb_dry |> filter(!duplicated(ID)), cellsize = units::set_units(1000000, "m2"))
# boreal_pixels = st_make_grid(x = hexagon_grid_boreal |> filter(!duplicated(ID)), cellsize = units::set_units(1000000, "m2"))

subb_moist_pixels = st_set_geometry(tibble(placeholder = rep(1,length(subb_moist_pixels))), subb_moist_pixels)
subb_dry_pixels = st_set_geometry(tibble(placeholder = rep(1,length(subb_dry_pixels))), subb_dry_pixels)
# boreal_pixels = st_set_geometry(tibble(placeholder = rep(1,length(boreal_pixels))), boreal_pixels)

# Add in an ID column for the subb_moist_pixels
subb_moist_pixels$X = st_coordinates(st_centroid(subb_moist_pixels))[,1]
subb_moist_pixels$Y = st_coordinates(st_centroid(subb_moist_pixels))[,2]

subb_dry_pixels$X = st_coordinates(st_centroid(subb_dry_pixels))[,1]
subb_dry_pixels$Y = st_coordinates(st_centroid(subb_dry_pixels))[,2]

# boreal_pixels$X = st_coordinates(st_centroid(boreal_pixels))[,1]
# boreal_pixels$Y = st_coordinates(st_centroid(boreal_pixels))[,2]

subb_moist_pixels = subb_moist_pixels |>
  arrange(Y,X) |>
  mutate(ID = row_number()) |>
  dplyr::select(-X, -Y)

subb_dry_pixels = subb_dry_pixels |>
  arrange(Y,X) |>
  mutate(ID = row_number()) |>
  dplyr::select(-X, -Y)

# boreal_pixels = boreal_pixels |>
#   arrange(Y,X) |>
#   mutate(ID = row_number()) |>
#   dplyr::select(-X, -Y)

subb_moist_pixels = subb_moist_pixels |>
  mutate(den = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         branch = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         cwd = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         cav = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         active = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         open = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
  ) |>
  dplyr::select(-placeholder)

subb_dry_pixels = subb_dry_pixels |>
  mutate(den = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
         branch = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
         cwd = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
         cav = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
         active = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
         open = sample(c(0,1),nrow(subb_dry_pixels), replace = T),
  ) |>
  dplyr::select(-placeholder)

# Just keep pixels that are inside the hexagons overlaying the BEC zone of interest.
subb_moist_pixels = subb_moist_pixels |>
  st_join(hexagon_grid_subb_moist |> filter(!duplicated(ID)) |> dplyr::select(hex_ID = ID)) |>
  filter(!is.na(hex_ID))

subb_dry_pixels = subb_dry_pixels |>
  st_join(hexagon_grid_subb_dry |> filter(!duplicated(ID)) |> dplyr::select(hex_ID = ID)) |>
  filter(!is.na(hex_ID))

# And also strip away any pixels that fall outside of the BEC zones!
subb_moist_pixels = subb_moist_pixels |>
  st_join(subb_moist) |>
  filter(!is.na(bec_zone))

subb_dry_pixels = subb_dry_pixels |>
  st_join(subb_dry) |>
  filter(!is.na(bec_zone))






# Make empty polygon for leaflet map when BEC zones / TSAs are not selected.
empty_polygon = st_as_sf(
  tibble(TSA_NUMBER_DESCRIPTION = "",
         map_label = "",
         OBJECTID = 0,
         ID = 0,
         lat = c(0,1),
         lng = c(0,1)),
  coords = c('lng','lat'),
  crs = 4326)

empty_polygon = st_set_geometry(
  empty_polygon[1,],
  st_bbox(empty_polygon) |>
    st_as_sfc()
)

write_sf(empty_polygon,
         'data/empty_poly.gpkg')

# Testing how to summarise for each hexagon.
library(data.table)
subb_moist_pixels_dt = as.data.table(subb_moist_pixels |> st_drop_geometry())

subb_moist_pixels_dt = subb_moist_pixels_dt[,lapply(.SD, \(x) sum(x)/.N), by = hex_ID, .SDcols = names(subb_moist_pixels_dt)[1:6]]

ggplot() + geom_sf(data = hexagon_grid, aes(fill = den_perc))

hexagons_with_values = hexagon_grid |>
  dplyr::select(hex_ID,X,Y,bec_zone) |>
  left_join(subb_moist_pixels_dt)

ggplot() + geom_sf(data = hexagons_with_values, aes(fill = den))

## TESTING ##
# How big are the cutblocks? How many 1-hectare squares would fit inside?
# Let's make the 1-hectare squares to fill whichever hexagons fit onto these cutblocks.
pixels_for_cutblock = st_make_grid(x = cutblock, cellsize = units::set_units(10000, "m2"))

pixels_for_1_hex = st_make_grid(x = hex_for_cutblock[1],cellsize = units::set_units(10000, "m2"))
pixels_for_cutblock = st_set_geometry(tibble(placeholder = abs(rnorm(length(pixels_for_cutblock),
                                                                 mean = 0.5,
                                                                 sd = 0.1))), pixels_for_cutblock)

hex_for_cutblock = hexagon_grid = st_make_grid(cutblock, square = F, cellsize = units::set_units(3e07, "m2"))

ggplot() +
  geom_sf(data = pixels_for_cutblock, aes(fill = placeholder)) +
  geom_sf(data = cutblock, alpha = 0.6, fill = 'red') +
  geom_sf(data = hex_for_cutblock, col = 'blue', alpha = 0.5, fill = 'transparent') +
  scale_fill_distiller(palette = 'RdYlGn', direction = 1)





