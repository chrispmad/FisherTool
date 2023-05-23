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

reset_selection_button = actionButton(
  inputId = 'reset_selection',
  label = 'Reset Selection'
)

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
  ),
  reset_selection_button
)

spat_ret_nav = nav(title = 'Spatial Retention Tool',
                   layout_sidebar(
                     sidebar = spat_ret_sidebar,
                     card(
                       leafletOutput('myleaf', height = '550px')#,
                       # DT::DTOutput('test_dt')
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
  selected = 'Spatial Retention Tool',
  theme = my_theme,
  # withMathJax(),
  title = 'Fisher Tool',
  interactive_d2_nav,
  spat_ret_nav
)

server <- function(input, output, session) {

  # if(!str_detect(getwd(),'www$')) setwd(paste0(getwd(),'/www/'))

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

  # Load in generated data.
  bc = bcmaps::bc_bound() |> st_transform(crs = 4326)

  # BEC Zones (n = 3) - first level of selection in leaflet map.
  subb_dry = read_sf('data/sub_boreal_dry.gpkg') |>
    st_transform(crs = 4326)

  subb_moist = read_sf('data/sub_boreal_moist.gpkg') |>
    st_transform(crs = 4326)

  # boreal = read_sf('data/boreal.gpkg') |>
  #   st_transform(crs = 4326)

  bec_zones = bind_rows(subb_dry, subb_moist) #|>
    # bind_rows(boreal)

  # Reactive: TSA polygons in selected BEC zone.
  tsa_polys = reactive({
    if(selected_bec() == 'nothing' | current_scale() == 'bec_zones'){
      dat = read_sf('data/empty_poly.gpkg')
    } else {
      if(selected_bec() == 'sub-boreal dry'){
        dat = read_sf('data/tsa_polygons_in_subb_dry.gpkg')
      }
      if(selected_bec() == 'sub-boreal moist'){
        dat = read_sf('data/tsa_polygons_in_subb_moist.gpkg')
      }
      if(selected_bec() == 'boreal'){
        dat = read_sf('data/tsa_polygons_in_boreal.gpkg')
      }
      if(current_scale() == 'hexagons'){
        dat = dat[dat$OBJECTID == selected_tsa(),]
      }
    }
    return(dat)
  })

  # Reactive: hexagons in selected TSA.
  hexagons_in_tsa = reactive({
    if(selected_tsa() == 'nothing' | current_scale() == 'bec_zones'){
      dat = read_sf('data/empty_poly.gpkg')
    } else {
      if(selected_tsa() != 'nothing' & selected_bec() == 'sub-boreal dry'){
        dat = read_sf('data/hexagons_subb_dry.gpkg') |>
          st_transform(crs = 4326) |>
          filter(tsa_obj_id %in% selected_tsa())
      }
      if(selected_tsa() != 'nothing' & selected_bec() == 'sub-boreal moist'){
        dat = read_sf('data/hexagons_subb_moist.gpkg') |>
          st_transform(crs = 4326) |>
          filter(tsa_obj_id %in% selected_tsa())
      }
      if(selected_tsa() != 'nothing' & selected_bec() == 'boreal'){
        dat = read_sf('data/hexagons_boreal.gpkg') |>
          st_transform(crs = 4326) |>
          filter(tsa_obj_id %in% selected_tsa())
      }
    }
    return(dat)
  })

  # Reactive: pixels in selected hexagon(s).
  pixels_in_hex = reactive({
    if(sum(str_detect(selected_hex(), 'nothing')) > 0 | current_scale() == 'bec_zones'){
      dat = read_sf('data/empty_poly.gpkg') |>
        mutate(hex_ID = 0, den = 0)
    } else {
      if(sum(str_detect(selected_hex(), 'nothing')) == 0 & selected_bec() == 'sub-boreal dry'){
        dat = read_sf('data/subb_dry_pixels.gpkg') |>
          st_transform(crs = 4326) |>
          filter(hex_ID %in% selected_hex())
      }
      if(sum(str_detect(selected_hex(), 'nothing')) == 0 & selected_bec() == 'sub-boreal moist'){
        dat = read_sf('data/subb_moist_pixels.gpkg') |>
          st_transform(crs = 4326) |>
          filter(hex_ID %in% selected_hex())
      }
      if(sum(str_detect(selected_hex(), 'nothing')) == 0 & selected_bec() == 'boreal'){
        dat = read_sf('data/boreal_pixels.gpkg') |>
          st_transform(crs = 4326) |>
          filter(hex_ID %in% selected_hex())
      }
    }
    return(dat)
  })

  # Reactive Values used in leaflet click selection events.
  selected_bec = reactiveVal('nothing')

  selected_tsa = reactiveVal('nothing')

  selected_hex = reactiveVal('nothing')

  current_scale = reactiveVal('bec_zones')

  # If user clicks on 'reset selection' button,
  # bring us back to BEC zone selection stage.
  observeEvent(input$reset_selection, {
    current_scale('bec_zones')
    selected_bec('nothing')
    selected_tsa('nothing')
    selected_hex('nothing')
  })

  current_zoom = reactive({
    if(current_scale() == 'bec_zones'){
      my_zoom = list(lng = -126, lat = 54.58419, zoom = 5)
    }
    if(current_scale() == 'TSA'){
      bec_zone_centroid = st_centroid(bec_zones[bec_zones$bec_zone == selected_bec(),])

      my_zoom = list(lng = bec_zone_centroid$geom[[1]][1],
                     lat = bec_zone_centroid$geom[[1]][2],
                     zoom = 6)
    }
    if(current_scale() == 'hexagons' & sum(str_detect(selected_hex(), 'nothing')) > 0){
      tsa_centroid = st_centroid(tsa_polys()[tsa_polys()$OBJECTID == selected_tsa(),])

      my_zoom = list(lng = tsa_centroid$geom[[1]][1],
                     lat = tsa_centroid$geom[[1]][2],
                     zoom = 7)
    }
    if(sum(str_detect(selected_hex(), 'nothing')) == 0){
      hex_centroid = st_centroid(hexagons_in_tsa()[hexagons_in_tsa()$ID %in% selected_hex(),])

      my_zoom = list(lng = hex_centroid$geom[[1]][1],
                     lat = hex_centroid$geom[[1]][2],
                     zoom = 10)
    }
    return(my_zoom)
  })

  observeEvent(input$myleaf_shape_click, {
    # Are we on to the hexagon selection scale? If so, grab shape id.
    if(current_scale() == 'hexagons'){
      # Have we already selected one hexagon? If so, add on the new ID.
      if(sum(str_detect(selected_hex(), 'nothing')) == 0){
        new_hex_selection = c(selected_hex(), input$myleaf_shape_click$id)
        selected_hex(unique(new_hex_selection[new_hex_selection != 'no_selection']))
      } else {
        selected_hex(input$myleaf_shape_click$id)
      }
      # current_scale('pixels')
    }
    # Are we on to the TSA selection scale? If so, grab shape id.
    if(current_scale() == 'TSA'){
      selected_tsa(input$myleaf_shape_click$id)
      current_scale('hexagons')
    }
    # Still on BEC zone selection? Grab id!
    if(input$myleaf_shape_click$id %in% c("boreal","sub-boreal dry","sub-boreal moist")){
      selected_bec(input$myleaf_shape_click$id)
      current_scale('TSA')
    }
  })

  # Which variable to see on the leaflet map?
  chosen_variable = reactive({
    input$variable_for_leaflet
  })

  bec_zone_pal = leaflet::colorFactor(
    palette = 'Dark2',
    domain = bec_zones$bec_zone)

  pixel_pal = leaflet::colorFactor(
    palette = 'Set1',
    domain = c(0,1),
    reverse = F
  )

  # output$test_dt = DT::renderDT({
  #   pixels_in_hex() |> filter(duplicated(ID))
  # })

  output$myleaf = renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB) |>
      addPolygons(layerId = ~bec_zone,
                  label = ~bec_zone,
                  color = 'black',
                  weight = 1,
                  fillColor = ~bec_zone_pal(bec_zone),
                  data = bec_zones) |>
      addLegend(pal = pixel_pal, values = c(0,1))
  })

  observe({
    leafletProxy('myleaf') |>
      clearGroup('selected_tsas') |>
      clearGroup('selected_hexagons') |>
      clearGroup('pixels_in_selection') |>
      addPolygons(
        layerId = ~OBJECTID,
        color = 'darkgreen',
        weight = 2,
        fillColor = 'darkgreen',
        opacity = 0.5,
        label = ~map_label,
        data = tsa_polys(),
        group = 'selected_tsas'
      ) |>
      addPolygons(
        layerId = ~ID,
        color = 'black',
        weight = 1,
        fillColor = 'purple',
        fillOpacity = 0.3,
        data = hexagons_in_tsa(),
        group = 'selected_hexagons'
      ) |>
      addPolygons(
        layerId = ~ID,
        color = 'black',
        weight = 1,
        fillColor = pixel_pal(pixels_in_hex()[[chosen_variable()]]),
        fillOpacity = 0.8,
        # label = ~ paste0(chosen_variable(),": ",pixels_in_hex()[[chosen_variable()]]),
        data = pixels_in_hex(),
        group = 'pixels_in_selection'
      ) |>
      setView(lng = current_zoom()$lng,
              lat = current_zoom()$lat,
              zoom = current_zoom()$zoom)
    #     # clearGroup(group = 'hexagons_with_values') |>
    #     # addPolygons(
    #     #   fillColor = 'grey',#~ my_fill_pal()(var_to_display),
    #     #   fillOpacity = 0.8,
    #     #   col = 'black',
    #     #   weight = 3,
    #     #   data = hex_data_var_chosen(),
    #     #   group = 'hexagons_with_values'
    #     # )
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
