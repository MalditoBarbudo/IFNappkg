#' @title mod_advancedFiltersUI and mod_advancedFilters
#'
#' @description A shiny module to process the advanced filters
#'
#' @param id shiny id
#' @param ifndb pool obkect to access the ifn db
#'
#' @export
mod_advancedFiltersUI <- function(id, ifndb) {

  # ns
  ns <- shiny::NS(id)

  # choices
  adv_fil_clima_vars_choices <- dplyr::tbl(ifndb, 'adv_fil_clima_vars_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'var_id'), magrittr::extract2(., 'esp')
      )
    }

  adv_fil_sig_vars_choices <- dplyr::tbl(ifndb, 'adv_fil_sig_vars_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'var_id'), magrittr::extract2(., 'esp')
      )
    }

  # ui
  shiny::tagList(

    shiny::fluidRow(
      shiny::column(
        6,
        # picker input to select the variables to filter
        shinyWidgets::pickerInput(
          ns('adv_fil_clima_vars'), 'Variables climáticas',
          choices = adv_fil_clima_vars_choices,
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
          choices = adv_fil_sig_vars_choices,
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
}

#' mod_advancedFilters server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param mod_buttons mod_buttons reactives, to get when button to show advanced
#'   filters is pressed
#' @param ifndb pool object to access the ifn db
#'
#' @export
#'
#' @importFrom dplyr between
#'
#' @rdname mod_advancedFiltersUI
mod_advancedFilters <- function(
  input, output, session,
  mod_buttons, ifndb
) {

  # toggle the panel when close button is pressed
  # shiny::observeEvent(
  #   eventExpr = input$close_adv_fils,
  #   handlerExpr = {
  #     shinyjs::hideElement(id = 'advancedFiltersControls')
  #   }
  # )

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

    clima_slider_inputs_choices <- dplyr::tbl(ifndb, 'adv_fil_clima_vars_thesaurus') %>%
      dplyr::collect()

    # we create the input list with lapply, easy peachy
    clima_inputs_list <- shiny::reactive({
      lapply(
        input$adv_fil_clima_vars, function(var) {

          clima_slider_inputs_choices_var <- clima_slider_inputs_choices %>%
            dplyr::filter(var_id == var)


          shiny::sliderInput(
            ns(var), label = clima_slider_inputs_choices_var[['esp']],
            min = clima_slider_inputs_choices_var[['min']],
            max = clima_slider_inputs_choices_var[['max']],
            value = c(
              clima_slider_inputs_choices_var[['value_min']],
              clima_slider_inputs_choices_var[['value_max']]
            ),
            round = 1
          )
        }
      )
    })

    sig_slider_inputs_choices <- dplyr::tbl(ifndb, 'adv_fil_sig_vars_thesaurus') %>%
      dplyr::collect()

    sig_inputs_list <- shiny::reactive({
      lapply(
        input$adv_fil_sig_vars, function(var) {
          sig_slider_inputs_choices_var <- sig_slider_inputs_choices %>%
            dplyr::filter(var_id == var)


          shiny::sliderInput(
            ns(var), label = sig_slider_inputs_choices_var[['esp']],
            min = sig_slider_inputs_choices_var[['min']],
            max = sig_slider_inputs_choices_var[['max']],
            value = c(
              sig_slider_inputs_choices_var[['value_min']],
              sig_slider_inputs_choices_var[['value_max']]
            ),
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
          between(!!rlang::sym(var), !!input[[var]][1], !!input[[var]][2])
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
          between(!!rlang::sym(var), !!input[[var]][1], !!input[[var]][2])
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