#' @title mod_advancedFiltersUI and mod_advancedFilters
#'
#' @description A shiny module to process the advanced filters
#'
#' @param id shiny id
#'
#' @export
mod_advancedFiltersUI <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # ui
  shiny::tagList(

    shinyjs::hidden(
      shiny::absolutePanel(
        # panel settings
        id = ns('advancedFiltersControls'), class = 'panel panel-default',
        fixed = TRUE, draggable = FALSE, width = '50%', height = 'auto',
        top = 'auto', right = '25%', left = 'auto', bottom = '2px',

        shiny::div(
          id = 'advFil',

          shiny::fluidRow(
            shiny::column(
              4, shiny::h3('Filtros Avanzados')
            ),
            shiny::column(
              1, offset = 7,
              shinyWidgets::circleButton(
                ns('close_adv_fils'), icon = shiny::icon('times'),
                status = 'danger', size = 'sm'
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              # picker input to select the variables to filter
              shinyWidgets::pickerInput(
                ns('adv_fil_clima_vars'), 'Variables climáticas',
                choices = names(dic_adv_fil_clima_filters[['esp']]),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = 'None selected...',
                  `select-all-text` = 'All selected',
                  `selected-text-format` = 'count',
                  `count-selected-text` = "{0} variables selected (of {1})"
                )
              )
            ),
            shiny::column(
              6,
              # picker input to select the variables to filter
              shinyWidgets::pickerInput(
                ns('adv_fil_sig_vars'), 'Variables SIG',
                choices = names(dic_adv_fil_sig_filters[['esp']]),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = 'None selected...',
                  `select-all-text` = 'All selected',
                  `selected-text-format` = 'count',
                  `count-selected-text` = "{0} variables selected (of {1})"
                )
              )
            )
          ),
          shiny::uiOutput(ns('adv_fil_filters'))
        )
      )
    )
  )
}

#' mod_advancedFilters server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param mod_data mod_data reactives, to get when button to show advanced
#'   filters is pressed
#'
#' @export
#'
#' @importFrom dplyr between
#'
#' @rdname mod_advancedFiltersUI
mod_advancedFilters <- function(
  input, output, session,
  mod_data
) {

  # show the panel when the button is pressed
  shiny::observeEvent(
    eventExpr = mod_data$show_adv_fils,
    handlerExpr = {
      shinyjs::showElement(id = 'advancedFiltersControls')
    }
  )

  # toggle the panel when close button is pressed
  shiny::observeEvent(
    eventExpr = input$close_adv_fils,
    handlerExpr = {
      shinyjs::hideElement(id = 'advancedFiltersControls')
    }
  )

  # reactive to get the variables selected, but debounced 1 sec to avoid to much
  # refreshes
  adv_fil_clima_variables <- shiny::reactive({
    input$adv_fil_clima_vars
  }) %>%
    shiny::debounce(1000)

  adv_fil_sig_variables <- shiny::reactive({
    input$adv_fil_sig_vars
  }) %>%
    shiny::debounce(1000)

  # ui renderer to create the different filters
  output$adv_fil_filters <- shiny::renderUI({

    # get the session ns to be able to tag the inputs with correct id
    ns <- session$ns

    # we create the input list with lapply, easy peachy
    clima_inputs_list <- shiny::reactive({
      lapply(
        input$adv_fil_clima_vars, function(var) {
          # shinyWidgets::noUiSliderInput(
          #   ns(var), label = dic_adv_fil_clima_filters[['esp']][[var]][['label']],
          #   min = dic_adv_fil_clima_filters[['esp']][[var]][['min']],
          #   max = dic_adv_fil_clima_filters[['esp']][[var]][['max']],
          #   value = dic_adv_fil_clima_filters[['esp']][[var]][['value']],
          #   orientation = 'horizontal', direction = 'ltr',
          #   behaviour = c('drag', 'tap'),
          #   format = shinyWidgets::wNumbFormat(decimals = 1)
          # )
          shiny::sliderInput(
            ns(var), label = dic_adv_fil_clima_filters[['esp']][[var]][['label']],
            min = dic_adv_fil_clima_filters[['esp']][[var]][['min']],
            max = dic_adv_fil_clima_filters[['esp']][[var]][['max']],
            value = dic_adv_fil_clima_filters[['esp']][[var]][['value']],
            round = 1
          )
        }
      )
    })

    sig_inputs_list <- shiny::reactive({
      lapply(
        input$adv_fil_sig_vars, function(var) {
          # shinyWidgets::noUiSliderInput(
          #   ns(var), label = dic_adv_fil_sig_filters[['esp']][[var]][['label']],
          #   min = dic_adv_fil_sig_filters[['esp']][[var]][['min']],
          #   max = dic_adv_fil_sig_filters[['esp']][[var]][['max']],
          #   value = dic_adv_fil_sig_filters[['esp']][[var]][['value']],
          #   orientation = 'horizontal', direction = 'ltr',
          #   behaviour = c('drag', 'tap'),
          #   format = shinyWidgets::wNumbFormat(decimals = 1)
          # )
          shiny::sliderInput(
            ns(var), label = dic_adv_fil_sig_filters[['esp']][[var]][['label']],
            min = dic_adv_fil_sig_filters[['esp']][[var]][['min']],
            max = dic_adv_fil_sig_filters[['esp']][[var]][['max']],
            value = dic_adv_fil_sig_filters[['esp']][[var]][['value']],
            round = 1
          )
        }
      )
    })

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(6, clima_inputs_list()),
        shiny::column(6, sig_inputs_list())
      )
    )
  })

  # quo filter expression constructor
  adv_fil_clima_expressions <- shiny::reactive({

    # check if adv_fil_clima_variables is null or empty, to avoid problems in
    # data_scenario helper function
    if (is.null(adv_fil_clima_variables()) || adv_fil_clima_variables() == '') {
      return(rlang::quo(TRUE))
    }

    # get the vars
    vars <- adv_fil_clima_variables()
    # return the list of quos to supply to tidyIFN::data_clima
    lapply(
      vars,
      function(var) {
        rlang::quo(
          dplyr::between(!!rlang::sym(var), !!input[[var]][1], !!input[[var]][2])
        )
      }
    )
  })

  adv_fil_sig_expressions <- shiny::reactive({

    # check if adv_fil_clima_variables is null or empty, to avoid problems in
    # data_scenario helper function
    if (is.null(adv_fil_sig_variables()) || adv_fil_sig_variables() == '') {
      return(rlang::quo(TRUE))
    }

    # get the vars
    vars <- adv_fil_sig_variables()
    # return the list of quos to supply to tidyIFN::data_clima
    lapply(
      vars,
      function(var) {
        rlang::quo(
          between(!!sym(var), !!input[[var]][1], !!input[[var]][2])
        )
      }
    )
  })

  mod_advancedFilters_reactives <- shiny::reactiveValues()
  shiny::observe({
    mod_advancedFilters_reactives$adv_fil_clima_expressions <- adv_fil_clima_expressions
    mod_advancedFilters_reactives$adv_fil_sig_expressions <- adv_fil_sig_expressions
  })

  return(mod_advancedFilters_reactives)
}