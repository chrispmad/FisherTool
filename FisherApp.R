library(shiny)
# library(adehabitatHS)
library(sf)
library(bslib)
library(tidyverse)
library(leaflet)
library(data.table)
library(Ryacas) # for the TeXForm command


source('modules/habitat_slider_module.R')

# Habitat variable names as vector.
habitat_varnames = tibble(real_name = c('den','branch',
                                        'cwd','cav',
                                        'active','open'),
                          label_name = c('Denning','Branch Resting',
                                         'CWD Resting','Cavity Resting',
                                         'Active','Open (less than)'))


int_d2_slider_inputs = card(
  card_body(
    layout_column_wrap(
      width = 1/2,
      layout_column_wrap(
        width = 1/3,
        h5("Habitat"),
        h5("Hectares"),
        h5("Percent"),
        style = 'text-align:center;font-weight:bold;font-size:large;'
      ),
      NULL
    ),
    lapply(habitat_varnames$real_name, habitat_slider_module_ui)
  )
)

int_d2_sidebar = sidebar(
  width = '60%',
  h3("Per 3000 hectare hexagon", style = 'text-align:center;'),
  int_d2_slider_inputs
)

formula = withMathJax('$$D^2 = (x - \\mu)\\prime\\Sigma^{-1} (x - \\mu)$$')

main_content = div(
  div(
    textOutput('mean_D2'),
    style = 'font-size:xx-large'
  ),
  textOutput('our_value_matrix'),
  HTML("<br><br><br>"),
  h5("Mahalanobis Distance Formula"),
  formula
)

interactive_d2_nav = nav(
  title = 'Interactive D2 Demonstration',
  layout_sidebar(
    int_d2_sidebar,
    main_content
  )
)

# ---------------------------------------------
# UI section for Nav 2: Habitat Retention Tool

spat_ret_sidebar = sidebar(
  h5("Spatial Retention Tool"),
  fileInput(inputId = 'user_shapefile',
            label = 'Upload Proposed Cutblock Shapefile',
            accept = c(".zip",".gpkg")),
  selectInput(inputId = 'variable_for_leaflet',
              label = 'Habitat Type to Visualize',
              choices = c('Denning' = 'den',
                          'Branch Resting' = 'branch',
                          'CWD Resting' = 'cwd',
                          'Cavity Resting' = 'cav',
                          'Active' = 'active',
                          'Open (less than)' = 'open'),
              selectize = F
              )
)

spat_ret_nav = nav(title = 'Spatial Retention Tool',
    layout_sidebar(
      sidebar = spat_ret_sidebar,
      card(
        leafletOutput('myleaf', height = '700px')#,
        # DT::DTOutput('data_test')
      )
    ))

my_theme = bs_theme(bootswatch = 'flatly',
                    version = "5",
                    # danger = "#cc0000",
                    # primary = '#3399ff',
                    # "sidebar-bg" = '#ADD8E7',
                    font_scale = 0.75) %>%
  bs_add_rules(".irs-min, .irs-max, .irs-from, .irs-to { visibility:hidden !important; }
                .shiny-input-container {margin-bottom:0;}")


ui <- bslib::page_navbar(
  theme = my_theme,
  # withMathJax(),
  title = 'Fisher Tool',
  interactive_d2_nav,
  spat_ret_nav
)

server <- function(input, output, session) {

  # if(!str_detect(getwd(),'www$')) setwd(paste0(getwd(),'/www/'))

  # Load in generated data.
  bc = bcmaps::bc_bound() |> st_transform(crs = 4326)

  subb_moist = read_sf('data/sub_boreal_moist.gpkg') |>
    st_transform(crs = 4326)

  tsa_polys = read_sf('data/tsa_polygons_in_zone_simplfied.gpkg') |>
    st_transform(crs = 4326)

  hex_data = read_sf('data/hexagons_subb_moist.gpkg') |>
    st_transform(crs = 4326)

  subb_moist_pixels = reactive({
    read_sf('data/subb_moist_pixels.gpkg') |>
    st_transform(crs = 4326)
  })

  # Make a label/numeric input/slider combo for each variable name.
  variable_values = lapply(habitat_varnames$real_name, \(x) habitat_slider_module_server(x, habitat_varnames))

  require(graphics)

  value_matrix <- reactive({
    return(
    unlist(
      matrix(
        unlist(lapply(1:6, \(x) variable_values[[x]]())) / 3000,
      ncol = 1
      )
    )
    )
  })
  # stopifnot(mahalanobis(value_matrix(), 0, diag(ncol(value_matrix()))) == rowSums(x*x))
  ##- Here, D^2 = usual squared Euclidean distances

  D2 <- reactive({
    values = value_matrix()

    results = mahalanobis(
      values,
      colMeans(values),
      var(c(0.6,0.5,0.6,0.7,0.4,0.6,0.5))
    )

    return(results)
  })

  output$mean_D2 = renderText({
    d2_values = D2()
    return(
    paste0("D2 Result: ",round(mean(d2_values),3))
    )})

  # --------------------------------------------------------------
  # Nav 2: Habitat Retention Tool

  # Summarise percentages for each hexagon.
  hex_with_values = reactive({

    subb_moist_pixels_dt = as.data.table(subb_moist_pixels() |> st_drop_geometry())

    subb_moist_pixels_dt = subb_moist_pixels_dt[,lapply(.SD, \(x) sum(x)/.N), by = hex_ID, .SDcols = names(subb_moist_pixels_dt)[1:6]]

    hex_data = hex_data |>
      dplyr::select(hex_ID,X,Y,bec_zone) |>
      left_join(as.data.frame(subb_moist_pixels_dt))

    hex_data = st_set_geometry(hex_data, hex_data$geom)

  })

  # Which variable to see on the leaflet map?
  chosen_variable = reactive({
    input$variable_for_leaflet
  })

  hex_data_var_chosen = reactive({
    browser()
    hex_with_values() |>
      mutate(var_to_display = round(!!sym(chosen_variable()),3)) |>
      dplyr::select(hex_ID,var_to_display)
  })

  my_fill_pal = reactive({
    leaflet::colorNumeric(palette = 'RdYlGn',
                          domain = hex_data_var_chosen()$var_to_display)
  })

  output$myleaf = renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB) |>
      addPolygons(
        col = 'black',
        weight = 1,
        fillColor = 'transparent',
        data = bc) |>
      addPolygons(
        color = 'darkgreen',
        weight = 2,
        fillColor = 'darkgreen',
        opacity = 0.5,
        data = subb_moist
      )
      # addLegend()
  })

  observe({
    leafletProxy('myleaf') |>
      clearGroup(group = 'hexagons_with_values') |>
      addPolygons(
        fillColor = 'grey',#~ my_fill_pal()(var_to_display),
        fillOpacity = 0.8,
        col = 'black',
        weight = 3,
        data = hex_data_var_chosen(),
        group = 'hexagons_with_values'
      )
  })

  output$data_test = DT::renderDT({
    hex_data_var_chosen()
  })
  # bc_grid = sf::st_as_sf(
  #   sf::st_make_grid(x = bcmaps::bc_bound() |> st_transform(crs = 4326),
  #                    n = c(10,10))
  # ) |> terra::rast()
  #
  # bc_grid$D2 = D2
  #
  # bc_sf = bc_grid |>
  #   as.data.frame(xy = TRUE)
  #
  # ggplot() + geom_raster(data = bc_sf, aes(x = x, y = y, fill = D2))
}

shinyApp(ui, server)
