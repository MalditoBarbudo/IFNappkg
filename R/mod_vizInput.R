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
            'Espècie simplificat' = 'especiesimple',
            'Gènere' = 'genere',
            'Conífera/Caducifoli/Esclerofil·le' = 'caducesclerconif',
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

  # update inputs with variables present in data. We have four input scenarios
  # so we build a reactive to know which scenario we have using the get_scenario
  # function from global.R
  input_scenario <- reactive({
    get_scenario(mod_data$viz_shape, mod_data$agg_level)
  })

  # color input
  observeEvent(
    eventExpr = input_scenario(),
    handlerExpr = {
      # if scenario changes, reset the input
      shinyjs::reset('color')

      # scenarios 1 and 2 (parecelas con y sin desglose)
      if (input_scenario() %in% c('scenario1', 'scenario2')) {
        # data needed
        vars_clima <- names(mod_data$data_clima() %>% collect()) %>%
          stringr::str_sort()
        vars_viz <- names(mod_data$data_core()) %>%
          stringr::str_sort()

        vars_to_use <- list(
          "Variables parcel·la" = vars_viz,
          "Variables climàtiques" = vars_clima
        )

        # update the needed inputs
        updateSelectInput(
          session, 'color', label = 'Color',
          choices = vars_to_use, selected = vars_to_use[[1]][1]
        )
      } else {
        # scenarios 3 y 4
        # data needed
        vars_viz <- names(mod_data$data_core()) %>%
          stringr::str_sort() %>%
          stringr::str_remove(
            pattern = '_mean$|_sd$|_min$|_max$|_n$|_q95$|_median$'
          ) %>%
          unique()

        vars_to_use <- list(
          "Variables poligon" = vars_viz
        )

        # update the needed inputs
        updateSelectInput(
          session, 'color', label = 'Color',
          choices = vars_to_use, selected = vars_to_use[[1]][1]
        )
      }
    }
  )

  # mida input
  observeEvent(
    eventExpr = input_scenario(),
    handlerExpr = {
      # if scenario changes, reset the input
      # shinyjs::reset('mida')

      # browser()
      # scenarios 1 and 2 (parecelas con y sin desglose)
      if (input_scenario() %in% c('scenario1', 'scenario2')) {
        # data needed
        vars_clima <- names(mod_data$data_clima() %>% collect()) %>%
          stringr::str_sort()
        vars_viz <- names(mod_data$data_core()) %>%
          stringr::str_sort() %>%
          paste0('', .)

        vars_to_use <- list(
          "Variables parcel·la" = vars_viz,
          "Variables climàtiques" = vars_clima
        )

        # update the needed inputs
        updateSelectInput(
          session, 'mida', label = 'Mida',
          choices = vars_to_use, selected = ''
        )

        # show and enable
        shinyjs::show('mida')
        # shinyjs::enable('mida')

      } else {
        # hide and disable
        shinyjs::hide('mida')
        # shinyjs::disable('mida')
      }
    }
  )

  # tipo_grup_func input
  observeEvent(
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

  # grup_func input
  observeEvent(
    eventExpr = {
      input$tipo_grup_func
      mod_data$agg_level
    },
    handlerExpr = {
      # este está presente en los cuatro scenarios, pero su valor depende de
      # de tipo_grup_funcional en 1 y 3 y de los datos en 2 y 4
      if (input_scenario() == 'scenario1') {

        # choices
        grup_func_choices <- mod_data$data_core() %>%
          pull(!!sym(glue('{input$tipo_grup_func}dens'))) %>%
          stringr::str_sort() %>%
          c('Qualsevol', .)

        updateSelectInput(
          session, 'grup_func',
          label = glue('{input$tipo_grup_func} dominant per densitat'),
          choices = grup_func_choices, selected = 'Qualsevol'
        )
      } else {
        if (input_scenario() %in% c('scenario2', 'scenario4')) {

          # choices
          grup_func_var <- glue('id{mod_data$agg_level}')

          grup_func_choices <- mod_data$data_core() %>%
            pull(!!sym(grup_func_var)) %>%
            stringr::str_sort()

          updateSelectInput(
            session, 'grup_func', label = glue('{mod_data$agg_level}'),
            choices = grup_func_choices
          )
        } else {
          if (input_scenario() == 'scenario3') {

            # choices
            grup_func_choices <- data_generator(
              oracle_ifn, mod_data$ifn, 'parcela', 'parcela', NULL, FALSE,
              {mod_data$data_sig() %>% collect()}, NULL
            ) %>%
              pull(!!sym(glue('{input$tipo_grup_func}dens'))) %>%
              stringr::str_sort() %>%
              c('Qualsevol', .)


            updateSelectInput(
              session, 'grup_func',
              label = glue('{input$tipo_grup_func} dominant per densitat'),
              choices = grup_func_choices
            )
          }
        }
      }
    }
  )

  # statistic input
  observeEvent(
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
  viz_reactives <- reactiveValues()

  observe({
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
