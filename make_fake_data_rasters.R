library(bcmaps)
library(sf)
library(tidyverse)
library(bcdata)
library(leaflet)

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

# Summarise subb_moist to single polygon.
subb_moist_s = subb_moist |>
  summarise(bec_zone = 'sub-boreal moist')

# Simplify the subb_moist shapefile... might be dangerous for data fidelity to do this!
subb_moist_s = rmapshaper::ms_simplify(subb_moist_s)

ggplot() +
  geom_sf(data = bc) +
  geom_sf(data = subb_moist_s, fill = 'lightblue')

write_sf(subb_moist_s, 'data/sub_boreal_moist.gpkg')
} else {subb_moist = read_sf('data/sub_boreal_moist.gpkg')}

# Get TSAs
if(!file.exists('data/all_tsa_polygons.gpkg')){

  all_layers = bcdata::bcdc_list()

  tsa_names = all_layers[str_detect(all_layers, 'fadm-timber-supply')]

  tsa_polys = bcdc_query_geodata(tsa_names[1]) |>
    collect()

  write_sf(tsa_polys, 'data/all_tsa_polygons.gpkg')
} else {tsa_polys = read_sf('data/all_tsa_polygons.gpkg')}

if(!file.exists('data/tsa_polygons_in_zone_simplfied.gpkg')){
# Remove TSAs that have been retired before today's date.
tsa_polys = tsa_polys |> filter(is.na(RETIREMENT_DATE))

# Remove TSAs that fall outside of our area of interest.
tsa_polys = tsa_polys |>
  st_join(subb_moist_s,
          st_intersects) |>
  filter(!is.na(bec_zone))

ggplot() +
  geom_sf(data = subb_moist_s, fill = 'brown') +
  geom_sf(data = tsa_polys, fill = 'lightblue', alpha = 0.5)

# Simplify the geometries of the TSAs a little bit... this might not be okay to do!
tsa_polys = rmapshaper::ms_simplify(tsa_polys)

tsa_polys = st_transform(tsa_polys, crs = 4326)

tsa_polys = tsa_polys |>
  mutate(map_label = coalesce(TSB_NUMBER_DESCRIPTION,TSA_NUMBER_DESCRIPTION))

write_sf(tsa_polys, 'data/tsa_polygons_in_zone_simplfied.gpkg')
} else {tsa_polys = read_sf('data/tsa_polygons_in_zone_simplfied.gpkg')}

leaflet() |>
  addTiles() |>
  addPolygons(
    label = ~map_label,
    color = 'black',
    weight = 2,
    fillOpacity = 0.5,
    fillColor = 'darkgreen',
    data = tsa_polys
  )

# Example cutblocks
cutblock = read_sf('data/A20001_274_blocks.shp') |>
  st_transform(crs = 3005)

# Make raster of 1 hectare size over the moist sub-boreal area.
# TOO BIG #
# one_ha_pixels = st_make_grid(subb_moist, cellsize = units::set_units(100000, "m2"))

# one_ha_points = st_centroid(one_ha_pixels)

# write_sf(one_ha_pixels, 'www/one_hectare_size_test.gpkg')

# NOTE: I will need to change the size to 10 times smaller (i.e. 3e07)
#       for production.
if(!file.exists('data/hexagons_subb_moist.gpkg')){
hexagon_grid = st_make_grid(subb_moist_s, square = F, cellsize = units::set_units(3e07, "m2"))

hexagon_grid = st_set_geometry(tibble(placeholder = rep(1,length(hexagon_grid))), hexagon_grid)

# ggplot() +
#   geom_sf(data = hexagon_grid, aes(fill = placeholder)) +
#   scale_fill_fermenter(palette = 'RdYlGn')

# Add in an ID column for the hexagons.
hexagon_grid$ID = row_number(hexagon_grid)

hexagon_grid$X = st_coordinates(st_centroid(hexagon_grid))[,1]
hexagon_grid$Y = st_coordinates(st_centroid(hexagon_grid))[,2]

hexagon_grid = hexagon_grid |>
  arrange(Y,X) |>
  mutate(ID = row_number())

# ggplot() +
#   geom_sf(data = hexagon_grid, aes(fill = ID))

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

hexagon_grid = hexagon_grid |>
  make_fake_data(var_name = variable_name_df$real_name[1]) |>
  make_fake_data(var_name = variable_name_df$real_name[2]) |>
  make_fake_data(var_name = variable_name_df$real_name[3]) |>
  make_fake_data(var_name = variable_name_df$real_name[4]) |>
  make_fake_data(var_name = variable_name_df$real_name[5]) |>
  make_fake_data(var_name = variable_name_df$real_name[6]) |>
  dplyr::select(-placeholder)

# Trim for area of interest.
hexagon_grid = hexagon_grid |>
  st_join(subb_moist_s, st_intersects) |>
  filter(!is.na(bec_zone))

write_sf(hexagon_grid, 'data/hexagons_subb_moist.gpkg')
} else {hexagon_grid = read_sf('data/hexagons_subb_moist.gpkg')}

ggplot() +
  geom_sf(data = hexagon_grid, aes(fill = den_perc))

# Make 100-hectare squares for our area of interest.
if(!file.exists('data/subb_moist_pixels.gpkg')){
subb_moist_pixels = st_make_grid(x = hexagon_grid, cellsize = units::set_units(1000000, "m2"))

subb_moist_pixels = st_set_geometry(tibble(placeholder = rep(1,length(subb_moist_pixels))), subb_moist_pixels)

# Just keep pixels that are inside the hexagons overlaying the BEC zone of interest.
subb_moist_pixels = subb_moist_pixels |>
  st_join(hexagon_grid |> dplyr::select(hex_ID)) |>
  filter(!is.na(hex_ID))

# Add in an ID column for the subb_moist_pixels
subb_moist_pixels$X = st_coordinates(st_centroid(subb_moist_pixels))[,1]
subb_moist_pixels$Y = st_coordinates(st_centroid(subb_moist_pixels))[,2]

subb_moist_pixels = subb_moist_pixels |>
  arrange(Y,X) |>
  mutate(ID = row_number()) |>
  dplyr::select(-X, -Y)

subb_moist_pixels = subb_moist_pixels |>
  mutate(den = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         branch = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         cwd = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         cav = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         active = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         open = sample(c(0,1),nrow(subb_moist_pixels), replace = T),
         )

write_sf(subb_moist_pixels, 'data/subb_moist_pixels.gpkg')
} else {subb_moist_pixels = read_sf('data/subb_moist_pixels.gpkg')}

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
