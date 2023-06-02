library(shiny)
# library(adehabitatHS)
library(sf)
library(bslib)
library(tidyverse)
library(leaflet)
library(data.table)
# library(Ryacas) # for the TeXForm command
library(terra)
library(plotly)
# if(system.file(package='ggchicklet') == ''){
#   install.packages("ggchicklet",                    # Install & load ggchicklet package
#                    repos = "https://cinc.rud.is")
# }
# library(ggchicklet)
# if(system.file(package = 'bbplot') == ''){
#   devtools::install_github('bbc/bbplot')
# }
# library(bbplot)

source('modules/habitat_slider_module.R')

# Habitat variable names as vector.
habitat_varnames = tibble(real_name = c('denning','mov',
                                        'cwd','rust',
                                        'cavity','opn'),
                          label_name = c('Denning','Movement',
                                         'CWD Resting','Rust Resting',
                                         'Cavity Resting',
                                         'Open (less than)'))


int_d2_slider_inputs = card(
  card_body(
    layout_column_wrap(
      width = 1/2,
      layout_column_wrap(
        width = 1/3,
        h5("Habitat"),
        h5("Hectares"),
        h5('Percent of 3000 hectare hexagon'),
        style = 'text-align:center;font-weight:bold;font-size:large;'
      ),
      # layout_column_wrap(
      #   width = 1/2,
      # numericInput('movp_input','Movp Input',min = 1, max = 5, value = 1),
      radioButtons('population_input',h5('Population'),
                   choices = c("SBS-moist" = "1","SBS-dry" = "2","dry forest" = "3","boreal" = "5"),
                   inline = T,
                   selected = '1')
      # ),
    ),
    lapply(habitat_varnames$real_name, habitat_slider_module_ui)
  )
)

int_d2_sidebar = sidebar(
  width = '60%',
  int_d2_slider_inputs,
  style = 'text-align:center;'
  )

formula = withMathJax('$$D^2 = (x - \\mu)\\prime\\Sigma^{-1} (x - \\mu)$$')

main_content = card(
  card_body(
    h3(HTML('Mahalanobis D<sup>2</sup>'),
       style = 'text-align:center;'),
    div(
      plotOutput('mean_D2'),
      style = 'text-align:center;'
    )
  ),
  card_body(
    formula,
    style = 'font-size:x-large;'
  )
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
  width = '25%',
  h5("Home Range Visualization Tool"),
  fileInput(inputId = 'user_shapefile',
            label = HTML('Upload Potential Habitat Alteration<br>(.zip file of shapefile, or .gpkg)'),
            accept = c(".zip",".gpkg")),
  actionButton('reset', 'Clear Uploaded Shape(s)'),
  HTML("<br>"),
  reset_selection_button,
  HTML("<br>"),
  card(
    plotOutput('hex_plot')
  )
)

spat_ret_nav = nav(title = 'Home Range Visualization Tool',
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
  # selected = 'Home Range Visualization Tool',
  selected = 'Interactive D2 Demonstration',
  theme = my_theme,
  # withMathJax(),
  title = 'Fisher Tool',
  interactive_d2_nav,
  spat_ret_nav
)

server <- function(input, output, session) {

  # --------------------------------------------------------------
  # Nav 1: Interactive D2 Demonstration

  # if(!str_detect(getwd(),'www$')) setwd(paste0(getwd(),'/www/'))

  # Make a label/numeric input/slider combo for each variable name.
  variable_values = lapply(habitat_varnames$real_name, \(x) habitat_slider_module_server(x, habitat_varnames))

  # require(graphics)

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

  # default values that minimize D2 (from Scott Yaeger's powerpoint presentation)
  # Note: movp classifications - 1: SBS-moist, 2: SBS-dry, 3: dry forest, 5: boreal
  default_values = tibble(movp = c(5,3,2,1),
                          denning = c(702,69,66,114),
                          mov = c(1686,1743,1539,1845),
                          cwd = c(522,324,384,912),
                          rust = c(255,123,573,1086),
                          cav = c(0,0,15,30),
                          opn = c(936,468,1119,981))

  # # Respond to 'set default' button
  # observeEvent(input$set_ideal_values, {
  #   updateNumericInput(inputId = 'mov-mov', value = default_values[default_values$movp == as.numeric(input$population_input),]$mov)
  #   updateNumericInput(inputId = 'denning-denning', value = default_values[default_values$movp == as.numeric(input$population_input),]$denning)
  #   updateNumericInput(inputId = 'cwd-cwd', value = default_values[default_values$movp == as.numeric(input$population_input),]$cwd)
  #   updateNumericInput(inputId = 'rust-rust', value = default_values[default_values$movp == as.numeric(input$population_input),]$rust)
  #   updateNumericInput(inputId = 'cavity-cavity', value = default_values[default_values$movp == as.numeric(input$population_input),]$cav)
  #   updateNumericInput(inputId = 'opn-opn', value = default_values[default_values$movp == as.numeric(input$population_input),]$opn)
  # })

  # Have a reactive value that tells Shiny whether we are ready to render the
  # gauge plot yet or not (if we don't have this, it gets updated
  # while the inputs are updated, one by one, so we get a flickering effect)

  # Respond to population selection by updating the numbers.
  observeEvent(input$population_input, {

    updateNumericInput(inputId = 'mov-mov', value = default_values[default_values$movp == as.numeric(input$population_input),]$mov)
    updateNumericInput(inputId = 'denning-denning', value = default_values[default_values$movp == as.numeric(input$population_input),]$denning)
    updateNumericInput(inputId = 'cwd-cwd', value = default_values[default_values$movp == as.numeric(input$population_input),]$cwd)
    updateNumericInput(inputId = 'rust-rust', value = default_values[default_values$movp == as.numeric(input$population_input),]$rust)
    updateNumericInput(inputId = 'cavity-cavity', value = default_values[default_values$movp == as.numeric(input$population_input),]$cav)
    updateNumericInput(inputId = 'opn-opn', value = default_values[default_values$movp == as.numeric(input$population_input),]$opn)

  })

  # Set up the covariance matrix (these values come from Rich Weir, before he retired!)
  # Note that columns are: 'den','active','cwd','rust','cav', and 'open'
  # Note also that rows are: 1: SBS-moist, 2: SBS-dry, 3: dry forest, 5: boreal
  # Not sure why there is a 4th row... curious.
  fisher_covariance_matrix = list(matrix(c(0.536,	2.742,	0.603,	3.211,	-2.735,	1.816,	2.742,	82.721,	4.877,	83.281,	7.046,	-21.269,	0.603,	4.877,	0.872,	4.033,	-0.67,	-0.569,	3.211,	83.281,	4.033,	101.315,	-15.394,	-1.31,	-2.735,	7.046,	-0.67,	-15.394,	56.888,	-48.228,	1.816,	-21.269,	-0.569,	-1.31,	-48.228,	47.963), ncol =6, nrow =6),
                                  matrix(c(0.525,	-1.909,	-0.143,	2.826,	-6.891,	3.264,	-1.909,	96.766,	-0.715,	-39.021,	69.711,	-51.688,	-0.143,	-0.715,	0.209,	-0.267,	1.983,	-0.176,	2.826,	-39.021,	-0.267,	58.108,	-21.928,	22.234,	-6.891,	69.711,	1.983,	-21.928,	180.113,	-96.369,	3.264,	-51.688,	-0.176,	22.234,	-96.369,	68.499), ncol =6, nrow =6),
                                  matrix(c(2.905,	0.478,	4.04,	1.568,	-3.89,	0.478,	0.683,	6.131,	8.055,	-8.04,	4.04,	6.131,	62.64,	73.82,	-62.447,	1.568,	8.055,	73.82,	126.953,	-130.153,	-3.89,	-8.04,	-62.447,	-130.153,	197.783), ncol=5, nrow=5),
                                  matrix(c(193.235,	5.418,	42.139,	125.177,	-117.128,	5.418,	0.423,	2.926,	5.229,	-4.498,	42.139,	2.926,	36.03,	46.52,	-42.571,	125.177,	5.229,	46.52,	131.377,	-101.195,	-117.128,	-4.498,	-42.571,	-101.195,	105.054), ncol =5, nrow =5))


D2 = reactive({

    # Set up a data.table with values.

    values = as.data.frame(value_matrix()) |>
      mutate(names = c('denning','mov',
                       'cwd','rust',
                       'cavity','opn')) |>
      add_row(V1 = as.numeric(input$population_input), names = 'movp')

    values = pivot_wider(values, names_from = names, values_from = V1)
    values = as.data.table(values)

    values = values |>
      mutate(across(-'movp', \(x) 100*x))

    # First, make sure habitat types are as a percentage of the total possible hectares, i.e. 3000

    # Implement some adjustments based on a mystical column 'movp'. These are all based
    # on knowledge inside Rich Weir and others' heads. It is not for us to understand!!
    values[ movp == 1 & denning >= 0, denning:=log(denning + 1)][ movp == 1 & cavity >= 0, cavity:=log(cavity + 1)]
    values[ movp == 2 & denning >= 0, denning:=log(denning + 1)]
    values[ movp >= 3 & rust >= 0, rust:=log(rust + 1)]

    values[ movp == 1 & denning > 1.57 , denning := 1.57 ][ movp == 1 & rust > 36.2, rust :=36.2][ movp == 1 & cavity > 0.685 , cavity :=0.685][ movp == 1 & cwd > 30.38, cwd :=30.38][ movp == 1 & mov > 61.5, mov :=61.5][ movp == 1 & opn < 32.7, opn :=32.7]
    values[ movp == 2 & denning > 1.16, denning := 1.16][ movp == 2 & rust > 19.1, rust :=19.1][ movp == 2 & cavity > 0.45 , cavity :=0.45][ movp == 2 & cwd > 12.7, cwd :=12.7][movp == 2 & mov > 51.3, mov :=51.3][ movp == 2 & opn < 37.3, opn :=37.3]
    values[ movp == 3 & denning > 2.3, denning := 2.3][ movp == 3 & rust > 1.6, rust :=1.6][ movp == 3 & cwd > 10.8, cwd :=10.8][ movp == 3 & mov > 58.1, mov := 58.1][ movp == 3 & opn < 15.58, opn := 15.58]
    values[ movp == 5 & denning > 24 , denning:=24 ][ movp ==5 & rust > 2.2, rust :=2.2][ movp ==5 & cwd > 17.4 , cwd :=17.4][ movp ==5 & mov > 56.2, mov :=56.2][ movp == 5 & opn < 31.2, opn := 31.2]

    values[ movp == 1, d2:= mahalanobis(values[ movp == 1, c("denning", "rust", "cavity", "cwd", "mov", "opn")], c(1.57, 36.2, 0.68, 30.38, 61.5, 32.72), cov = fisher_covariance_matrix[[1]])]
    values[ movp == 2, d2:= mahalanobis(values[ movp == 2, c("denning", "rust", "cavity", "cwd", "mov", "opn")], c(1.16, 19.1, 0.4549, 12.76, 51.25, 37.27), cov = fisher_covariance_matrix[[2]])]
    values[ movp == 3, d2:= mahalanobis(values[ movp == 3, c("denning", "rust", "cwd", "mov", "opn")], c(2.31, 1.63, 10.8, 58.1, 15.58), cov = fisher_covariance_matrix[[3]])]
    values[ movp == 5, d2:= mahalanobis(values[ movp == 5, c("denning", "rust", "cwd", "mov", "opn")], c(23.98, 2.24, 17.4, 56.2, 31.2), cov = fisher_covariance_matrix[[4]])]
    values[ denning < 0.001, d2:= NA][mov < 0.001,  d2:= NA][cwd  < 0.001,  d2 := NA][rust  < 0.001,  d2 := NA]

    return(as.data.frame(values))
  })

  ###function for colour code
  colour_func_gauge <- function(x, value){
    case_when(
      x[[value]] < 7 ~ 'darkgreen',
      x[[value]] >= 7 & x[[value]] < 14 ~ 'darkorange',
      x[[value]] >= 14 ~ 'darkred'
    )
  }

  output$mean_D2 = renderPlot({

    d2_value = D2()$d2

    if(d2_value > 50) d2_value = 50

    max_d2 = 50

    df = tibble(max = max_d2,
                value = d2_value) |>
      mutate(value_half = 0.5*value)

    my_colwidth = 0.5
    my_colalpha = 0.5
    my_linewidth = 10

    df = df |>
      mutate(value_lab = case_when(
        value < 0.01 ~ '< 0.01',
        value == 50 ~ '> 50',
        T ~ as.character(round(value,2))
      ))

    ggplot(df, aes(x = 1, xend = 1)) +
      geom_col(
        aes(x = 1, y = 50),
        width = my_colwidth,
        alpha = my_colalpha,
        col = 'black',
        fill = 'darkred') +
      geom_col(
        aes(x = 1, y = 14),
        width = my_colwidth,
        alpha = my_colalpha,
        col = 'black',
        fill = 'darkorange') +
      geom_col(
        aes(x = 1, y = 7),
        width = my_colwidth,
        alpha = my_colalpha,
        col = 'black',
        fill = 'darkgreen') +
      geom_segment(lwd = my_linewidth, lineend = 'round',
                   aes(y = 0, yend = abs(value - 0.5)),
                   colour = colour_func_gauge(df, 'value')) +
      geom_text(aes(x = 0, y = 1, label = value_lab), size = 16) +
      scale_fill_manual(values = colour_func_gauge(df, 'value')) +
      xlab("") + ylab("") +
      theme_classic() +
      coord_polar(theta = "y",start=-pi/2) +
      ylim(0,100) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            legend.position = "top",
            legend.text.align = 0,
            legend.background = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size=18,
                                                color="#222222"),
            plot.title = ggtext::element_markdown())
  })

  # --------------------------------------------------------------
  # Nav 2: Habitat Retention Tool

  # Load in generated data.
  bc = bcmaps::bc_bound() |> st_transform(crs = 4326)

  # Probably unnecessary once the app is published...
  if(!str_detect(getwd(),".*www$")){
    setwd(paste0(getwd(),"/www"))
  }

  # BEC Zones (n = 2) - first level of selection in leaflet map.
  subb_dry = read_sf('sub_boreal_dry.gpkg') |>
    st_transform(crs = 4326)

  subb_moist = read_sf('sub_boreal_moist.gpkg') |>
    st_transform(crs = 4326)

  bec_zones = bind_rows(subb_dry, subb_moist)

  # TSAs that overlap with the BEC zones.
  tsa = read_sf('tsa_polygons_in_bec_zones.gpkg') |>
    st_transform(crs = 4326) |>
    mutate(map_label = TSA_NUMBER_DESCRIPTION,
           OBJECTID = TSA_NUMBER)

  # # Rasters of 1-hectare pixels.
  # subb_dry_r = rast('data/subb_dry_pixels.tif')
  # subb_moist_r = rast('data/subb_moist_pixels.tif')
  dat_r = rast('megaraster.tif')

  # Hexagons a la Kyle Lochhead
  hexagons = read_sf('feta_hex_simplified.gpkg')

  # Reactive: user's uploaded shapefile.
  user_file = reactiveValues()

  userpoly = reactive({

    user_file = input$user_shapefile
    if(is.null(user_file)) return(read_sf('empty_poly.gpkg'))

    #If it's a geopackage, read it in directly.
    if(str_detect(user_file$datapath, ".gpkg")){
      userpoly = read_sf(user_file$datapath) %>%
        st_transform(crs = 4326) |>
        summarise()

      userpoly
    }

    #If it's a zipped shapefile, unzip then read in.
    if(str_detect(user_file$datapath, ".zip")){

      filelist <- unzip(user_file$datapath)
      userpoly = read_sf(filelist[str_detect(filelist, ".shp")]) %>%
        st_transform(crs = 4326)

      userpoly
    }
    # Whenever a file is uploaded, I'd like to reset
    # the scope of the map to BEC zones.
    selected_bec('nothing')
    selected_tsa('nothing')
    selected_hex('nothing')
    current_scale('bec_zones')

    return(userpoly |> mutate(map_label = 'Uploaded Polygons'))
  })

  observe({
    user_file$userpoly = userpoly()
  })

  observeEvent(input$reset, {
    user_file$userpoly = read_sf('empty_poly.gpkg')
    selected_bec('nothing')
    selected_tsa('nothing')
    selected_hex('nothing')
    current_scale('bec_zones')
  })

  # If the user clicks on 'eliminate uploaded shape',
  # do that here.
  observeEvent(input$user_shapefile, ~ {
    shiny::file
  })

  # Reactive: TSA polygons in selected BEC zone.
  tsa_polys = reactive({
    if(selected_bec() == 'nothing'){ #| current_scale() != 'hexagons'){
      dat = read_sf('empty_poly.gpkg')
    } else {

      dat = tsa

      if(current_scale() == 'hexagons'){
        dat = dat[dat$TSA_NUMBER == selected_tsa(),]
      }
    }
    return(dat)
  })

  # Reactive: hexagons in selected TSA.
  # Note: if User has uploaded a spatial object, limit hexagons to
  # just those hexagons that spatially overlap with the uploaded spatial object.
  hexagons_in_tsa = reactive({

    if(selected_tsa() == 'nothing' | current_scale() == 'bec_zones'){
      dat = read_sf('empty_poly.gpkg')
    } else {
      if(selected_tsa() != 'nothing'){
        dat = hexagons |>
          st_transform(crs = 4326) |>
          st_join(tsa_polys() |>
                    filter(TSA_NUMBER == selected_tsa()) |>
                    dplyr::select(TSA_NUMBER),
                  st_intersects) |>
          filter(!is.na(TSA_NUMBER)) |>
          st_join(bec_zones) |>
          filter(!is.na(bec_zone)) |>
          dplyr::select(-bec_zone)
        # filter(tsa_obj_id %in% selected_tsa())
      }
      # if(selected_tsa() != 'nothing' & selected_bec() == 'sub-boreal moist'){
      #   dat = read_sf('data/hexagons_subb_moist.gpkg') |>
      #     st_transform(crs = 4326) |>
      #     filter(tsa_obj_id %in% selected_tsa())
      # }
    }

    #Has user uploaded file? If so, clip hexagons here.
    if(user_file$userpoly$map_label == 'Uploaded Polygons'){
      dat = dat |>
        st_join(user_file$userpoly |> dplyr::mutate(is_in_user_file = T), st_intersects) |>
        filter(!is.na(is_in_user_file) | ID == 0) |>
        dplyr::select(-is_in_user_file)
    }
    return(dat)
  })

  # Reactive summarized value for selected hexagons
  hexagons_with_values = reactive({

    # req(hexagons_in_tsa()$map_label != "")

    # Filter down object of all hexagons in TSA to just our selected list.
    choice_hexes = hexagons_in_tsa() |>
      filter(!duplicated(ID))

    hexes_to_summarise = choice_hexes$ID

    if('TSA_NUMBER' %in% names(choice_hexes)){
      # shiny Feedback!
      withProgress(message = 'Summarizing 1-hectare cell values...', {

        incProgress(0.2,
                    message = 'Please wait 30+ seconds, depending on size of selected TSA...')
        # Pull out raster pixel values for our list of chosen hexagons.
        extracted_data = terra::extract(dat_r,
                                        st_transform(choice_hexes,
                                                     crs = 3005)
        )
        # The extracted data needs to have an ID column that lines up with
        # the hexagon spatial object's ID column. Add that in here.
        extracted_data = extracted_data |>
          left_join(
            choice_hexes |>
              mutate(hex_ID = ID) |>
              mutate(ID = row_number()) |>
              dplyr::select(ID,hex_ID) |> st_drop_geometry()
          ) |>
          mutate(ID = hex_ID) |>
          dplyr::select(ID,denning:movp)

        # # We lose the geometry here, need to read it to the table as official geometry.
        # extracted_data = st_set_geometry(extracted_data, extracted_data$geom)

        # If the user has uploaded a spatial file,
        # update raster pixel values for those areas
        # (we assume these would reduce habitat quality from 1 to 0 for simplicity's sake.
        if(user_file$userpoly$map_label != ''){
          clipped_choice_hexes = st_intersection(choice_hexes,
                                                 st_transform(user_file$userpoly,
                                                              crs = 4326))

          pixel_value_updater = terra::extract(dat_r,
                                               st_transform(clipped_choice_hexes,
                                                            crs = 3005))

          pixel_value_updater = pixel_value_updater |>
            left_join(
              clipped_choice_hexes |>
                mutate(hex_ID = ID) |>
                mutate(ID = row_number()) |>
                dplyr::select(ID,hex_ID) |>
                st_drop_geometry()
            ) |>
            mutate(ID = hex_ID) |>
            dplyr::select(ID,denning:movp) |>
            mutate(across(denning:movp, ~ 0))

          # Overwrite a number of rows of the extracted data to 0 for habitat
          # types. The number of rows depends on the number of rows in
          # the 'pixel_value_updater' object.

          for(unique_id in unique(pixel_value_updater$ID)){
            hexagon_subset_extracted_data = extracted_data[extracted_data$ID == unique_id,]

            hexagon_subset_extracted_data[1:nrow(pixel_value_updater[pixel_value_updater$ID == unique_id,]),c(2:7)] = 0

            extracted_data = bind_rows(
              extracted_data[extracted_data$ID != unique_id,],
              hexagon_subset_extracted_data
            )
          }

        } else {
        }

        # colnames(extracted_data)[1] <- 'ID'

        # Drop pixels that fall outside the home-range of any of
        # the 4 fisher populations (these have a movp value of 0)
        # extracted_data = extracted_data |> filter(movp != 0)

        # If there are multiple population home-ranges in a hexagon,
        # change all non-0 populations to whatever the majority
        # is in that hexagon.
        hex_population_count = extracted_data |>
          count(ID,movp) |>
          group_by(ID) |>
          arrange(desc(n)) |>
          slice(1) |>
          ungroup() |>
          dplyr::rename(movp_corrector = movp) |>
          dplyr::select(ID,movp_corrector)

        extracted_data = extracted_data |>
          left_join(hex_population_count) |>
          mutate(movp = movp_corrector)

        extracted_data = as.data.table(extracted_data)

        # Get proportion of 3000 potential hectares for each
        # habitat type. Do this for each chosen hexagon ID separately,
        # and also for each different population (i.e. 'movp').

        extracted_data = extracted_data[,lapply(.SD, \(x) 100*sum(x,na.rm=T)/3000),
                                        by = .(ID,movp)]

        incProgress(1/2,
                    message = 'Finished extracting values.')

        #Calculate the Mahalanobis D2 for each hexagon.
        # Make sure variables are in this order:
        # 'denning','mov',
        # 'cwd','rust',
        # 'cavity','opn'

        # Implement some adjustments based on a mystical column 'movp'. These are all based
        # on knowledge inside Rich Weir and others' heads. It is not for us to understand!!
        extracted_data[ movp == 1 & denning >= 0, denning:=log(denning + 1)][ movp == 1 & cavity >= 0, cavity:=log(cavity + 1)]
        extracted_data[ movp == 2 & denning >= 0, denning:=log(denning + 1)]
        extracted_data[ movp >= 3 & rust >= 0, rust:=log(rust + 1)]

        extracted_data[ movp == 1 & denning > 1.57 , denning := 1.57 ][ movp == 1 & rust > 36.2, rust :=36.2][ movp == 1 & cavity > 0.685 , cavity :=0.685][ movp == 1 & cwd > 30.38, cwd :=30.38][ movp == 1 & mov > 61.5, mov :=61.5][ movp == 1 & opn < 32.7, opn :=32.7]
        extracted_data[ movp == 2 & denning > 1.16, denning := 1.16][ movp == 2 & rust > 19.1, rust :=19.1][ movp == 2 & cavity > 0.45 , cavity :=0.45][ movp == 2 & cwd > 12.7, cwd :=12.7][movp == 2 & mov > 51.3, mov :=51.3][ movp == 2 & opn < 37.3, opn :=37.3]
        extracted_data[ movp == 3 & denning > 2.3, denning := 2.3][ movp == 3 & rust > 1.6, rust :=1.6][ movp == 3 & cwd > 10.8, cwd :=10.8][ movp == 3 & mov > 58.1, mov := 58.1][ movp == 3 & opn < 15.58, opn := 15.58]
        extracted_data[ movp == 5 & denning > 24 , denning:=24 ][ movp ==5 & rust > 2.2, rust :=2.2][ movp ==5 & cwd > 17.4 , cwd :=17.4][ movp ==5 & mov > 56.2, mov :=56.2][ movp == 5 & opn < 31.2, opn := 31.2]

        extracted_data[ movp == 1, d2:= mahalanobis(extracted_data[ movp == 1, c("denning", "rust", "cavity", "cwd", "mov", "opn")], c(1.57, 36.2, 0.68, 30.38, 61.5, 32.72), cov = fisher_covariance_matrix[[1]])]
        extracted_data[ movp == 2, d2:= mahalanobis(extracted_data[ movp == 2, c("denning", "rust", "cavity", "cwd", "mov", "opn")], c(1.16, 19.1, 0.4549, 12.76, 51.25, 37.27), cov = fisher_covariance_matrix[[2]])]
        extracted_data[ movp == 3, d2:= mahalanobis(extracted_data[ movp == 3, c("denning", "rust", "cwd", "mov", "opn")], c(2.31, 1.63, 10.8, 58.1, 15.58), cov = fisher_covariance_matrix[[3]])]
        extracted_data[ movp == 5, d2:= mahalanobis(extracted_data[ movp == 5, c("denning", "rust", "cwd", "mov", "opn")], c(23.98, 2.24, 17.4, 56.2, 31.2), cov = fisher_covariance_matrix[[4]])]
        extracted_data[ denning < 0.001, d2:= NA][mov < 0.001,  d2:= NA][cwd  < 0.001,  d2 := NA][rust  < 0.001,  d2 := NA]

        hexagon_values = as.data.frame(extracted_data)

        # If a given hexagon has a portion that is 0 for movp (i.e.
        # there's no population homerange there), as well as another
        # portion that is non-0 for movp, drop the 0 part!
        hexagon_values = hexagon_values |>
          group_by(ID) |>
          mutate(result_rows = n()) |>
          ungroup() |>
          filter(movp != 0 | result_rows == 1)

        output = st_set_geometry(
          hexagon_values,
          st_geometry(choice_hexes)
        )
      })
    } else {
      output = st_set_geometry(
        tibble(a = 0,
               ID = 0,
               d2 = 1,
               d2_b = 1) |>
          mutate(hex_ID = 0),
        st_geometry(choice_hexes)
      )
    }

    # Optional addition: calculate new column that is binned values
    if(!'a' %in% names(output)){
      output = output |>
        mutate(d2_b = data.table::fcase(
          d2 < 7, '1',
          d2 >= 7 & d2 < 14, '2',
          d2 >= 14, '3'
        ))
    }

    return(output)
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
    if(user_file$userpoly$map_label != ''){
      my_zoom = list(lng = st_centroid(user_file$userpoly)$geom[[1]][1],
                     lat = st_centroid(user_file$userpoly)$geom[[1]][2],
                     zoom = 10)
    }
    if(current_scale() == 'bec_zones' & user_file$userpoly$map_label == ''){
      my_zoom = list(lng = -126, lat = 54.58419, zoom = 5)
    }
    if(current_scale() == 'TSA' & user_file$userpoly$map_label == ''){
      bec_zone_centroid = st_centroid(bec_zones[bec_zones$bec_zone == selected_bec(),])

      my_zoom = list(lng = bec_zone_centroid$geom[[1]][1],
                     lat = bec_zone_centroid$geom[[1]][2],
                     zoom = 6)
    }
    if(current_scale() == 'hexagons' & user_file$userpoly$map_label == ''){
      tsa_centroid = st_centroid(tsa_polys()[tsa_polys()$OBJECTID == selected_tsa(),])

      my_zoom = list(lng = tsa_centroid$geom[[1]][1],
                     lat = tsa_centroid$geom[[1]][2],
                     zoom = 7)
    }
    # if(){
    #   hex_centroid = st_centroid(hexagons_in_tsa()[hexagons_in_tsa()$ID %in% selected_hex(),])
    #
    #   my_zoom = list(lng = hex_centroid$geom[[1]][1],
    #                  lat = hex_centroid$geom[[1]][2],
    #                  zoom = 10)
    # }
    return(my_zoom)
  })

  observeEvent(input$myleaf_shape_click, {
    if(!is.null(input$myleaf_shape_click$id)){
      # Are we on to the hexagon selection scale? If so, grab shape id.
      if(current_scale() == 'hexagons'){
        selected_hex(input$myleaf_shape_click$id)
        # # Have we already selected one hexagon? If so, add on the new ID.
        # if(sum(str_detect(selected_hex(), 'nothing')) == 0){
        #   new_hex_selection = c(selected_hex(), input$myleaf_shape_click$id)
        #   selected_hex(unique(new_hex_selection[new_hex_selection != 'no_selection']))
        # } else {
        #   selected_hex(input$myleaf_shape_click$id)
        # }
        # # current_scale('pixels')
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
    }
  })

  # Render a plot for the selected hex (if any)
  hexagon_in_focus = reactive({
    if(selected_hex() == 'nothing') return(read_sf('empty_poly.gpkg') |>
                                             mutate(d2 = 0, d2_b = 0))

    return(
      hexagons_with_values() |>
        filter(ID == selected_hex())
    )
  })

  output$hex_plot = renderPlot({

    if(selected_hex() == 'nothing'){
      ggplot() +
        geom_text(aes(x = 1, y = 1),
                  label = "Please select a hexagon") +
        ggthemes::theme_map()
    } else {
    ggplot(hexagon_in_focus()) +
          geom_col(aes(x = 'Denning', y = denning)) +
          geom_col(aes(x = 'Movement', y = mov)) +
          geom_col(aes(x = 'CWD', y = cwd)) +
          geom_col(aes(x = 'Rust', y = rust)) +
          geom_col(aes(x = 'Cavity', y = cavity)) +
          geom_col(aes(x = 'Open', y = opn)) +
          labs(x = '', y = 'Percent (%)') +
          # scale_y_continuous(labels = scales::percent_format()) +
          theme_minimal() +
        coord_flip() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14))
          #theme(panel.grid = element_blank(),
           #     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    }
  })

  bec_zone_pal = leaflet::colorFactor(
    palette = 'Dark2',
    domain = bec_zones$bec_zone)

  # hex_pal = leaflet::colorNumeric(
  #   palette = 'Spectral',
  #   domain = c(0, 1000),
  #   reverse = T,
  #   na.color = 'grey'
  # )
  # hex_pal = leaflet::colorBin(
  #   palette = 'RdYlGn',
  #   domain = c(0, 1000),
  #   bins = c(0,7,14,50),
  #   reverse = T,
  #   na.color = 'grey'
  # )
  hex_pal = leaflet::colorFactor(
    palette = c('darkgreen','darkorange','darkred'),
    levels = c('1','2','3'),
    na.color = 'transparent'
  )

  labels = c('0 - 7','7 - 14','14 - 50+')

  output$myleaf = renderLeaflet({
    leaflet() |>
      envreportutils::add_bc_home_button() |>
      addProviderTiles(providers$CartoDB) |>
      addPolygons(layerId = ~bec_zone,
                  label = ~bec_zone,
                  color = ~bec_zone_pal(bec_zone),
                  weight = 2,
                  opacity = 1,
                  fillColor = ~bec_zone_pal(bec_zone),
                  data = bec_zones) |>
      addLegend(pal = hex_pal,
                values = c('1','2','3'),
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(labels)
                }
      )
  })

  observe({
    leafletProxy('myleaf') |>
      clearGroup('bec_zones') |>
      clearGroup('selected_tsas') |>
      clearGroup('hexagons') |>
      clearGroup('hexagon_in_focus') |>
      clearGroup('user_shapes') |>
      addPolygons(layerId = ~bec_zone,
                  label = ~bec_zone,
                  color = ~bec_zone_pal(bec_zone),
                  weight = 2,
                  opacity = ifelse(current_scale() == 'bec_zones', 0.5,0.05),
                  fillColor = ~bec_zone_pal(bec_zone),
                  data = bec_zones,
                  group = 'bec_zones') |>
      addPolygons(
        layerId = ~OBJECTID,
        color = 'darkgreen',
        weight = 2,
        fillColor = 'darkgreen',
        opacity = ifelse(current_scale() == 'TSA', 0.5,0.05),
        label = ~map_label,
        data = tsa_polys(),
        group = 'selected_tsas'
      ) |>
      addPolygons(
        layerId = ~ID,
        color = 'black',
        weight = 1,
        label = round(hexagons_with_values()$d2,3),
        fillColor = ~hex_pal(hexagons_with_values()$d2_b),
        fillOpacity = 0.5,
        data = hexagons_with_values(),
        group = 'hexagons'
      ) |>
      addPolygons(
        layerId = ~ID,
        color = 'orange',
        weight = 2,
        label = round(hexagon_in_focus()$d2,3),
        fillColor = ~hex_pal(hexagon_in_focus()$d2_b),
        fillOpacity = 0.5,
        data = hexagon_in_focus(),
        group = 'hexagon_in_focus'
      ) |>
      addPolygons(
        group = 'user_shapes',
        color = 'orange',
        weight = 1,
        fillColor = 'red',
        fillOpacity = 0.5,
        data = reactive(user_file$userpoly)()
      ) |>
      setView(lng = current_zoom()$lng,
              lat = current_zoom()$lat,
              zoom = current_zoom()$zoom)
  })

  output$data_test = DT::renderDT({
    hex_data_var_chosen()
  })

}

shinyApp(ui, server)
