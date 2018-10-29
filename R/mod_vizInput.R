#' @title mod_vizInput and mod_viz
#'
#' @description A shiny module to generate and populate the visualization inputs
#'
#' @param id shiny id
#' @param ifndb pool object to access ifn db
#'
#' @export
mod_vizInput <- function(id, ifndb) {

  # ns
  ns <- shiny::NS(id)

  # choices
  color_choices <- dplyr::tbl(ifndb, 'color_thesaurus') %>%
    dplyr::filter(scenario_id == 'scenario3') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'color_id'), magrittr::extract2(., 'esp')
      )
    }

  mida_choices <- dplyr::tbl(ifndb, 'mida_thesaurus') %>%
    dplyr::filter(scenario_id == 'scenario3') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'mida_id'), magrittr::extract2(., 'esp')
      )
    }

  statistic_choices <- dplyr::tbl(ifndb, 'statistic_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'statistic_id'), magrittr::extract2(., 'esp')
      )
    }

  tipo_grup_func_choices <- dplyr::tbl(ifndb, 'tipo_grup_func_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'tipo_grup_func_id'), magrittr::extract2(., 'esp')
      )
    }

  grup_func_choices <- dplyr::tbl(ifndb, 'grup_func_thesaurus') %>%
    dplyr::filter(scenario_id == 'scenario3', grup_func_id == 'cadesccon') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'grup_func_val'), magrittr::extract2(., 'esp')
      )
    }

  # UI
  shiny::absolutePanel(
    id = 'vizControls', class = 'panel panel-default', fixed = TRUE,
    draggable = TRUE, width = 380, height = 'auto',
    top = 400, right = 60, left = 'auto', bottom = 'auto',

    shiny::tagList(

      # shiny::wellPanel(
        shiny::h3(label_getter(ifndb, 'esp', 'vizControls_h3_label')),
        shinyWidgets::pickerInput(
          ns('color'), label_getter(ifndb, 'esp', 'color_label'),
          color_choices,
          width = '100%'
        ),
        shinyWidgets::awesomeCheckbox(
          ns('inverse_pal'), label_getter(ifndb, 'esp', 'inverse_pal_label'),
          value = FALSE, status = 'info'
        ),
        shinyjs::hidden(
          shinyWidgets::pickerInput(
            ns('mida'), label_getter(ifndb, 'esp', 'mida_label'),
            mida_choices,
            width = '100%'
          )
        ),
        shinyWidgets::pickerInput(
          ns('statistic'), label = label_getter(ifndb, 'esp', 'statistic_label'),
          choices = statistic_choices,
          width = '100%'
        ),
        shinyWidgets::pickerInput(
          ns('tipo_grup_func'), label_getter(ifndb, 'esp', 'tipo_grup_func_label'),
          choices = tipo_grup_func_choices,
          selected = 'cadesccon', width = '100%'
        ),
        shinyWidgets::pickerInput(
          ns('grup_func'),
          label_getter(ifndb, 'esp', 'grup_func_label', 'scenario3', 'especie'),
          choices = grup_func_choices,
          width = '100%'
        )
      # )
    )
  )
}

#' mod_viz server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param mod_data reactive with the reactive data and the data inputs
#' @param ifndb pool object to access ifn db
#'
#' @export
#'
#' @rdname mod_vizUI
mod_viz <- function(
  input, output, session,
  mod_data, ifndb
) {

  input_scenario <- shiny::reactive({
    get_scenario(mod_data$viz_shape, mod_data$agg_level)
  })

  ## Color input
  shiny::observeEvent(
    eventExpr = input_scenario(),
    handlerExpr = {

      color_choices <- dplyr::tbl(ifndb, 'color_thesaurus') %>%
        dplyr::filter(scenario_id == !!input_scenario()) %>%
        dplyr::collect() %>% {
          magrittr::set_names(
            magrittr::extract2(., 'color_id'), magrittr::extract2(., 'esp')
          )
        }

      # update the needed inputs
      shinyWidgets::updatePickerInput(
        session, 'color', label = label_getter(ifndb, 'esp', 'color_label'),
        choices = color_choices
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

        mida_choices <- dplyr::tbl(ifndb, 'mida_thesaurus') %>%
          dplyr::filter(scenario_id == !!input_scenario()) %>%
          dplyr::collect() %>% {
            magrittr::set_names(
              magrittr::extract2(., 'mida_id'), magrittr::extract2(., 'esp')
            )
          }

        shinyWidgets::updatePickerInput(
          session, 'mida', label = label_getter(ifndb, 'esp', 'mida_label'),
          choices = mida_choices
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
      if (input_scenario() %in% c('scenario1', 'scenario3')) {

        grup_func_choices <- dplyr::tbl(ifndb, 'grup_func_thesaurus') %>%
          dplyr::filter(scenario_id == !!input_scenario(), grup_func_id == input$tipo_grup_func) %>%
          dplyr::collect() %>% {
            magrittr::set_names(
              magrittr::extract2(., 'grup_func_val'), magrittr::extract2(., 'esp')
            )
          }

        shinyWidgets::updatePickerInput(
          session, 'grup_func',
          label = label_getter(ifndb, 'esp', 'grup_func_label', input_scenario(), input$tipo_grup_func),
          choices = grup_func_choices
        )

      } else {
        grup_func_choices <- dplyr::tbl(ifndb, 'grup_func_thesaurus') %>%
          dplyr::filter(scenario_id == !!input_scenario(), grup_func_id == mod_data$agg_level) %>%
          dplyr::collect() %>% {
            magrittr::set_names(
              magrittr::extract2(., 'grup_func_val'), magrittr::extract2(., 'esp')
            )
          }

        shinyWidgets::updatePickerInput(
          session, 'grup_func',
          label = label_getter(ifndb, 'esp', 'grup_func_label', input_scenario(), mod_data$agg_level),
          choices = grup_func_choices
        )
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
