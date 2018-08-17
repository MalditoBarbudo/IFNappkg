#' @title mod_vizInput and mod_viz
#'
#' @description A shiny module to generate and populate the visualization inputs
#'
#' @param id shiny id
#'
#' @export
mod_vizInput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI
  shiny::tagList(

    # div and id is for later use of shinyjs. Inputs will be empty and
    # populated later on with the data in the server side
    shiny::div(
      id = 'vizInputs',

      shiny::wellPanel(
        shiny::h3('Visualització'),
        shiny::selectInput(
          ns('color'), 'Color',
          dic_color_choices[['esp']][['scenario3']],
          width = '100%'
        ),
        shiny::checkboxInput(
          ns('inverse_pal'), 'Invertir colors', value = FALSE
        ),
        shinyjs::hidden(
          shiny::selectInput(
            ns('mida'), label_mida[['esp']],
            dic_mida_choices[['esp']][['scenario1']],
            width = '100%'
          )
        ),
        shinyWidgets::pickerInput(
          ns('tipo_grup_func'), label_tipo_grup_func[['esp']],
          choices = dic_tipo_grup_func_choices[['esp']],
          selected = 'cadesccon', width = '100%'
        ),
        shiny::selectInput(
          ns('grup_func'), label_grup_func[['esp']][['scenario3']][['especie']],
          choices = dic_grup_func_choices[['esp']][['scenario3']][['especie']],
          width = '100%'
        ),
        shiny::selectInput(
          ns('statistic'), label = label_statistic[['esp']],
          choices = dic_statistic_choices[['esp']],
          width = '100%'
        )
      )
    )
  )
}

#' mod_viz server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param mod_data reactive with the reactive data and the data inputs
#'
#' @export
#'
#' @rdname mod_vizUI
mod_viz <- function(
  input, output, session,
  mod_data
) {

  input_scenario <- shiny::reactive({
    get_scenario(mod_data$viz_shape, mod_data$agg_level)
  })

  ## Color input
  shiny::observeEvent(
    eventExpr = input_scenario(),
    handlerExpr = {
      # update the needed inputs
      shiny::updateSelectInput(
        session, 'color', label = 'Color',
        choices = dic_color_choices[["esp"]][[input_scenario()]]
      )
    }
  )

  ## Size input
  shiny::observeEvent(
    eventExpr = input_scenario(),
    handlerExpr = {
      # if scenario changes, reset the input
      # shinyjs::reset('mida')

      # browser()
      # scenarios 1 and 2 (parecelas con y sin desglose)
      if (input_scenario() %in% c('scenario1', 'scenario2')) {

        shiny::updateSelectInput(
          session, 'mida', label = label_mida[['esp']],
          choices = dic_mida_choices[["esp"]][[input_scenario()]]
        )

        # show and enable
        shinyjs::show('mida')

      } else {

        # hide and disable
        shinyjs::hide('mida')

      }
    }
  )

  ## Functional group class input
  shiny::observeEvent(
    eventExpr = input_scenario(),
    handlerExpr = {
      # solo aparece en scenarios 1 y 3
      if (input_scenario() %in% c('scenario1', 'scenario3')) {
        # show
        shinyjs::show('tipo_grup_func')
      } else {
        # hide
        shinyjs::hide('tipo_grup_func')
      }
    }
  )

  # Functional group values input
  shiny::observeEvent(
    eventExpr = {
      input$tipo_grup_func
      mod_data$agg_level
    },
    handlerExpr = {
      # este está presente en los cuatro scenarios, pero su valor depende de
      # de tipo_grup_funcional en 1 y 3 y de los datos en 2 y 4
      if (input_scenario() == 'scenario1') {

        shiny::updateSelectInput(
          session, 'grup_func',
          label = label_grup_func[['esp']][['scenario1']][[input$tipo_grup_func]],
          choices = dic_grup_func_choices[['esp']][['scenario1']][[input$tipo_grup_func]]
        )

      } else {
        if (input_scenario() %in% c('scenario2', 'scenario4')) {

          shiny::updateSelectInput(
            session, 'grup_func',
            label = label_grup_func[['esp']][[input_scenario()]][[mod_data$agg_level]],
            choices = dic_grup_func_choices[['esp']][[input_scenario()]][[mod_data$agg_level]]
          )

        } else {
          if (input_scenario() == 'scenario3') {

            shiny::updateSelectInput(
              session, 'grup_func',
              label = label_grup_func[['esp']][['scenario3']][[input$tipo_grup_func]],
              choices = dic_grup_func_choices[['esp']][['scenario3']][[input$tipo_grup_func]]
            )

          }
        }
      }
    }
  )

  # Statistic input
  shiny::observeEvent(
    eventExpr = input_scenario(),
    handlerExpr = {

      # scenarios 3 and 4 (poligonos con y sin desglose)
      if (input_scenario() %in% c('scenario3', 'scenario4')) {
        # show and enable
        shinyjs::show('statistic')

      } else {
        # hide and disable
        shinyjs::hide('statistic')
      }

    }
  )

  # reactive with the inputs values
  viz_reactives <- shiny::reactiveValues()

  shiny::observe({
    # inputs
    viz_reactives$color <- input$color
    viz_reactives$inverse_pal <- input$inverse_pal
    viz_reactives$mida <- input$mida
    viz_reactives$tipo_grup_func <- input$tipo_grup_func
    viz_reactives$grup_func <- input$grup_func
    viz_reactives$statistic <- input$statistic
  })

  return(viz_reactives)
}
