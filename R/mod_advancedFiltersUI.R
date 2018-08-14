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
  # picker input to select the variables to filter
  shinyWidgets::pickerInput(
    ns('adv_fil_vars'), 'Selecciona las variables',
    choices = '', multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      `deselect-all-text` = 'None selected...',
      `select-all-text` = 'All selected',
      `selected-text-format` = 'count',
      `count-selected-text` = "{0} variables selected (of {1})"
    )
  )

  # uiOutput, we need to create the filters on the fly based on the variables
  # selected in adv_fil_vars
  shiny::uiOutput(ns('adv_fil_filters'))

}

#' mod_advancedFilters server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
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
    inputs_list <- reactive({
      lapply(
        input$adv_fil_vars, function(var) {
          noUiSliderInput(
            ns(var), label = dic_adv_fil_filters[[var]][['label']],
            min = dic_adv_fil_filters[[var]][['min']],
            max = dic_adv_fil_filters[[var]][['max']],
            value = dic_adv_fil_filters[[var]][['value']],
            orientation = 'vertical', direction = 'rtl',
            behaviour = 'drag-tap',
            format = shinyWIdgets::wNumbFormat(decimals = 1)
          )
        }
      )
    })

    # old
    inputs_list <- reactive({
      lapply(
        input$clima_var_sel, function(var) {
          sliderInput(
            ns(var), label = var,
            min = min(clima_data[[var]]), max = max(clima_data[[var]]),
            value = c(min(clima_data[[var]]), max = max(clima_data[[var]]))
          )
        }
      )
    })

  })


}