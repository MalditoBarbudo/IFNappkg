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
          ns('color'), 'Color', c(Cap = ''), width = '100%'
        ),
        shiny::checkboxInput(
          ns('inverse_pal'), 'Invertir colors', value = FALSE
        ),
        shinyjs::hidden(
          shiny::selectInput(
            ns('mida'), 'Mida', c(Cap = ''), width = '100%'
          )
        ),
        shiny::selectInput(
          ns('tipo_grup_func'), 'Tipus grup funcional',
          choices = c(
            'Espècie' = 'especie',
            'Espècie simplificat' = 'especiesimp',
            'Gènere' = 'genere',
            'Conífera/Caducifoli/Esclerofil·le' = 'cadesccon',
            'Conífera/Planifoli' = 'planifconif'
          ), width = '100%'
        ),
        shiny::selectInput(
          ns('grup_func'), 'Grup funcional', c(Cap = ''), width = '100%'
        ),
        shiny::selectInput(
          ns('statistic'), 'Mètrica',
          c(
            'Mitjana' = '_mean', 'Mediana' = '_median',
            'Desviació estàndard' = '_sd', 'Mìnim' = '_min', 'Màxim' = '_max',
            'Nombre parcel·les' = '_n', 'Quartil 95' = '_q95'
          ), width = '100%'
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
        choices = dic_color_choices[["cat"]][[input_scenario()]]
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
          session, 'mida', label = 'Mida',
          choices = vars_to_use, selected = ''
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
          label = glue('{input$tipo_grup_func} dominant per densitat'),
          choices = grup_func_choices, selected = 'Qualsevol'
        )

      } else {
        if (input_scenario() %in% c('scenario2', 'scenario4')) {

          shiny::updateSelectInput(
            session, 'grup_func', label = glue('{mod_data$agg_level}'),
            choices = grup_func_choices
          )

        } else {
          if (input_scenario() == 'scenario3') {

            shiny::updateSelectInput(
              session, 'grup_func',
              label = glue('{input$tipo_grup_func} dominant per densitat'),
              choices = grup_func_choices
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
