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

    shiny::absolutePanel(
      # panel settings
      id = 'advancedFiltersControls', class = 'panel panel-default',
      fixed = TRUE, draggable = TRUE, width = '80%', height = 'auto',
      top = '10%', right = '10%', left = 'auto', bottom = 'auto',

      shiny::div(
        id = 'advFil',

        shiny::h3('Filtros Avanzados'),
        shiny::fluidRow(
          shiny::column(
            3,
            # picker input to select the variables to filter
            shinyWidgets::pickerInput(
              ns('adv_fil_vars'), 'Selecciona las variables',
              choices = names(dic_adv_fil_filters[['esp']]),
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
            9,
            # uiOutput, we need to create the filters on the fly based on the variables
            # selected in adv_fil_vars
            shiny::uiOutput(ns('adv_fil_filters'))
          )
        )
      )
    )
  )
}

#' mod_advancedFilters server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#'
#' @importFrom dplyr between
#'
#' @rdname mod_advancedFiltersUI
mod_advancedFilters <- function(
  input, output, session
) {

  # reactive to get the variables selected, but debounced 1 sec to avoid to much
  # refreshes
  adv_fil_variables <- shiny::reactive({
    input$adv_fil_vars
  }) %>%
    shiny::debounce(1000)

  # ui renderer to create the different filters
  output$adv_fil_filters <- shiny::renderUI({

    # get the session ns to be able to tag the inputs with correct id
    ns <- session$ns

    # we create the input list with lapply, easy peachy
    inputs_list <- shiny::reactive({
      lapply(
        input$adv_fil_vars, function(var) {
          shinyWidgets::noUiSliderInput(
            ns(var), label = dic_adv_fil_filters[['esp']][[var]][['label']],
            min = dic_adv_fil_filters[['esp']][[var]][['min']],
            max = dic_adv_fil_filters[['esp']][[var]][['max']],
            value = dic_adv_fil_filters[['esp']][[var]][['value']],
            orientation = 'horizontal', direction = 'ltr',
            behaviour = c('drag', 'tap'),
            format = shinyWidgets::wNumbFormat(decimals = 1),
            inline = TRUE
          )
        }
      )
    })

    shiny::tagList(inputs_list())
  })

  # quo filter expression constructor
  adv_fil_expressions <- reactive({

    # check if adv_fil_variables is null or empty, to avoid problems in
    # data_scenario helper function
    if(is.null(adv_fil_variables()) || adv_fil_variables() == '') {
      return(quo(TRUE))
    }

    # get the vars
    vars <- adv_fil_variables()
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
    mod_advancedFilters_reactives$adv_fil_expressions <- adv_fil_expressions
  })

  return(mod_advancedFilters_reactives)
}