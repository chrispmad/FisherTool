habitat_slider_module_ui <- function(id) {
  ns <- NS(id)
  # card(
  layout_column_wrap(
    width = 1/2,
    layout_column_wrap(width = 1/3,
                       div(textOutput(ns('this_variable')),
                           style = 'margin-top: 30px; text-align:center; font-size:medium;'),
                       div(uiOutput(ns('ha_value_ui')), style = 'margin-top: 10px; text-align:center;'),
                       div(textOutput(ns('percent_value')),
                           style = 'margin-top: 30px; text-align:center; font-size:large;')
    ),
    div(
      sliderInput(inputId = ns(id), label = '',
                  sep = '',
                  min = 0, max = 3000, value = 30*sample.int(n = 100, 1),
                  step = 100),
      style = '
      # irs-min{visibility:hidden; }
                span.irs-max{visibility:hidden;}
                .irs-from{visibility:hidden}
                . irs-to{visibility:hidden;}
                #shiny-input-container{margin-bottom:0rem;}'
    )
  )
}

habitat_slider_module_server <- function(id, name_df) {
  moduleServer(
    id,
    function(input, output, session) {

      # Render numeric input UI
      output$ha_value_ui = renderUI({
        ns = NS(id)
        numericInput(ns('ha_value'),label = '', value = 0)
      })

      # Hidden state
      state <- reactiveValues(chosen_value = NULL)

      slider_val = reactive(input[[id]])

      # UI elements update the hidden state
      observeEvent(input[[id]],  {
        if(!identical(state$chosen_value, input[[id]])){
          state$chosen_value <- input[[id]]
        }
      })
      observeEvent(input$ha_value,  {
        if(!identical(state$chosen_value, input$ha_value)){
          state$chosen_value <- input$ha_value
        }
      })

      # state updates the out of sync UI elements
      observeEvent(state$chosen_value, {
        # But only if state is not currently NULL, which indicates
        # we have just loaded the tab/page.
        if(!is.null(state$chosen_value)){
          if (!identical(input[[id]], state$chosen_value)) {
            updateNumericInput(session, id, value = state$chosen_value)
          }
          if (!identical(input$ha_value, state$chosen_value)) {
            updateSliderInput(session, "ha_value", value = state$chosen_value)
          }
        }
      })

      output$this_variable = renderText(name_df[name_df$real_name == id,]$label_name)

      output$percent_value = renderText(paste0(round(100*slider_val()/3000,1),"%"))

      output = eventReactive(state$chosen_value, {
        state$chosen_value
      })
      return(output)
    }
  )
}
