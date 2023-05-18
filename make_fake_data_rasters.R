library(bcmaps)
library(sf)
library(tidyverse)
library(bcdata)

all_layers = bcdata::bcdc_list()
all_layers[str_detect(all_layers,"biogeoclimat")]

if(!file.exists('data/biogeoclimatic_zones_fisher.gpkg')){

}

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

beczones = bcmaps::bec()
# Make raster of 1 hectare size... very large.
one_ha_pixels = st_make_grid(bc, cellsize = units::set_units(10000, "m2"))

write_sf(one_ha_pixels, 'www/one_hectare_size_test.gpkg')

# NOTE: I will need to change the size to 10 times smaller (i.e. 3e07)
#       for production.
hexagon_grid = st_make_grid(bc, square = F, cellsize = units::set_units(3e08, "m2"))

hexagon_grid = st_set_geometry(tibble(placeholder = rep(1,length(hexagon_grid))), hexagon_grid)

# Trim for BC.
hexagon_grid = st_join(hexagon_grid, bc) |>
  filter(!is.na(province)) |>
  dplyr::select(-province)

ggplot() +
  geom_sf(data = hexagon_grid, aes(fill = placeholder)) +
  scale_fill_fermenter(palette = 'RdYlGn')

# Add in an ID column for the hexagons.
hexagon_grid$ID = row_number(hexagon_grid)

hexagon_grid$X = st_coordinates(st_centroid(hexagon_grid))[,1]
hexagon_grid$Y = st_coordinates(st_centroid(hexagon_grid))[,2]

hexagon_grid = hexagon_grid |>
  arrange(Y,X) |>
  mutate(ID = row_number())

ggplot() +
  geom_sf(data = hexagon_grid, aes(fill = ID))

# # Make values a bit more similar, based on nearest neighbours?
# hex_averaged = hexagon_grid |>
#   mutate(averaged_value = case_when(
#     sample(c(0,1),nrow(hexagon_grid),replace = T) == 1 ~ (value + lead(value) + lag(value))/3,
#     T ~ value)) |>
#   mutate(averaged_value = ifelse(is.na(averaged_value),value,averaged_value))
#
# ggplot() +
#   geom_sf(data = hex_averaged, aes(fill = averaged_value)) +
#   scale_fill_fermenter(palette = 'RdYlGn', direction = 1)

# hex_averaged

make_fake_data = function(dat, var_name){

  dat = dat |>
    mutate(base_values = rnorm(n = nrow(dat), mean = 0.5, sd = 0.1))
  # Make values a bit more similar, based on nearest neighbours?
  dat |>
    mutate(!!sym(var_name) := case_when(
      sample(c(0,1),nrow(hexagon_grid),replace = T) == 1 ~ (base_values + lead(base_values) + lag(base_values))/3,
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

ggplot() +
  geom_sf(data = hexagon_grid, aes(fill = den_perc))

write_sf(hexagon_grid, 'www/fake_hex_data.gpkg')
