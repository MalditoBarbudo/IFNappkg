#' @title mod_tableOutput and mod_table
#'
#' @description A shiny module to generate the base IFN plots table
#'
#' @param id shiny id
#'
#' @export
mod_tableOutput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI
  shiny::tagList(

    shiny::fluidRow(
      shiny::column(
        8,
        DT::DTOutput(ns('ifn_table')) %>%
          shinycssloaders::withSpinner(
            type = 4, color = '#D2527F'
          )
      ),
      shiny::column(
        4,
        shiny::fluidRow(
          shinyWidgets::downloadBttn(
            ns('dwl_csv_button'), 'Save csv',
            color = 'primary', size = 'sm', block = FALSE,
            style = 'minimal'
          ),
          shinyWidgets::downloadBttn(
            ns('dwl_xlsx_button'), 'Save xlsx',
            color = 'primary', size = 'sm', block = FALSE,
            style = 'minimal'
          )
        ),
        shiny::fluidRow(
          shinyWidgets::pickerInput(
            ns('col_vis_selector'), 'Show/Hide columns',
            choices = '', multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = 'None selected...',
              `select-all-text` = 'All selected',
              `selected-text-format` = 'count',
              `count-selected-text` = "{0} variables selected (of {1})"
            )
          )
        ),
        shiny::fluidRow(
          shinyWidgets::pickerInput(
            ns('col_filter_selector'), 'Select columns to filter by',
            choices = '', multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = 'None selected...',
              `select-all-text` = 'All selected',
              `selected-text-format` = 'count',
              `count-selected-text` = "{0} variables selected (of {1})"
            )
          ),
          shiny::uiOutput(ns('col_filter'))
        )
      )
    )
  )
}

#' mod_table server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param mod_data reactive with the reactive data and the data inputs
#' @param mod_advancedFilters reactive with the reactive values from
#'   advancedFilters module
#' @param ifndb db pool
#'
#' @export
#'
#' @rdname mod_tableOutput
mod_table <- function(
  input, output, session,
  mod_data, mod_advancedFilters, ifndb
) {

  scenario_reac <- shiny::reactive({
    get_scenario(mod_data$viz_shape, mod_data$agg_level)
  })

  table_data_gen <- shiny::reactive({

    data_scenario_table <- data_scenario(
      mod_data$admin_div,
      mod_data$admin_div_fil,
      mod_data$espai_tipus,
      mod_data$espai_tipus_fil,
      mod_data$ifn,
      ifndb,
      mod_data$agg_level,
      mod_data$diameter_classes,
      mod_advancedFilters$adv_fil_clima_expressions(),
      mod_advancedFilters$adv_fil_sig_expressions()
    )

    # check data integrity (zero rows)
    if (
      {
        data_scenario_table[['clima']] %>%
          dplyr::collect() %>%
          nrow()
      } < 1
    ) {

      shinyWidgets::sendSweetAlert(
        session, title = 'Sin datos',
        text = 'Con los filtros actuales activados no hay parcelas que cumplan los requisitos',
        type = 'warning'
      )

      return(NULL)

    } else {
      data_scenario_table %>%
        table_data_modificator(
          scenario_reac(),
          mod_data$admin_div, mod_data$agg_level, mod_data$diameter_classes
        )
    }
  })

  shiny::observe({
    cd <- ifelse(mod_data$diameter_classes, 'cd', 'nocd')

    shinyWidgets::updatePickerInput(
      session, 'col_vis_selector', 'Show/Hide columns',
      choices = dic_col_vis_input[['esp']][[cd]][[scenario_reac()]]
    )

    shinyWidgets::updatePickerInput(
      session, 'col_filter_selector', 'Select columns to filter by',
      choices = dic_col_vis_input[['esp']][[cd]][[scenario_reac()]]
    )
  })

  # # get the columns selected in the table , but for real (remember, DT start
  # # col indexes in zero)
  # col_selected_react <- shiny::reactive({
  #   input$ifn_table_columns_selected + 1
  # })

  output$col_filter <- shiny::renderUI({

    # get the session ns to be able to tag the inputs with correct id
    ns <- session$ns

    # diameter classes
    cd <- ifelse(mod_data$diameter_classes, 'cd', 'nocd')

    # we create the input list with lapply, easy peachy
    col_filter_inputs <- shiny::reactive({

      data_temp <- table_data_gen()

      lapply(
        input$col_filter_selector, function(var) {
          if (!is.numeric(data_temp[[var]])) {
            return()
          }

          min_var <- floor(min(data_temp[[var]], na.rm = TRUE))
          max_var <- floor(max(data_temp[[var]], na.rm = TRUE))

          shiny::sliderInput(
            ns(var), label = var,
            min = min_var,
            max = max_var,
            round = 1,
            value = c(min_var, max_var)
          )
        }
      )
    })

    # tag list to return for the UI
    shiny::tagList(
      col_filter_inputs(),
      shinyWidgets::actionBttn(
        ns('apply_table_filters'), 'Aplicar filtros',
        icon = shiny::icon('eye'),
        style = "material-flat",
        block = TRUE,
        size = 'sm'
      )
    )
  })

  # quo filter expression constructor
  col_filter_expressions <- shiny::eventReactive(
    ignoreInit = FALSE, ignoreNULL = FALSE,
    eventExpr = input$apply_table_filters,
    valueExpr = {

      # check if adv_fil_clima_variables is null or empty, to avoid problems in
      # data_scenario helper function
      if (is.null(input$col_filter_selector) || input$col_filter_selector == '') {
        return(rlang::quos())
      } else {
        data_temp <- table_data_gen()

        # return the list of quos to supply to tidyIFN::data_clima
        lapply(
          input$col_filter_selector,
          function(var) {
            rlang::quo(
              between(!!rlang::sym(var), !!input[[var]][1], !!input[[var]][2])
            )
          }
        )
      }
    }
  )

  output$ifn_table <- DT::renderDT(
    server = TRUE,
    expr = {

      shiny::validate(
        need(table_data_gen(), 'No hay datos')
      )

      if (is.null(input$col_vis_selector)) {
        data_table_temp <- table_data_gen() %>%
          dplyr::filter(!!! col_filter_expressions())

        shiny::validate(
          need(
            nrow(data_table_temp) > 0,
            'Con los filtros actuales no se pueden mostrar datos'
          )
        )
      } else {
        data_table_temp <- table_data_gen() %>%
          dplyr::filter(!!! col_filter_expressions()) %>%
          dplyr::select(dplyr::one_of(input$col_vis_selector))


        shiny::validate(
          need(
            nrow(data_table_temp) > 0,
            'Con los filtros actuales no se pueden mostrar datos'
          )
        )
      }

      data_table_temp %>%
        DT::datatable(
          # filter = list(position = 'top', clear = FALSE, plain = TRUE),
          selection = list(target = 'column'),
          style = 'default', rownames = FALSE,
          fillContainer = TRUE, autoHideNavigation = TRUE,
          extensions = c('Scroller'),
          options = list(
            autoWidth = TRUE,
            deferRender = TRUE, scrollY = '70vh', scroller = TRUE,
            dom = 'ti'
          )
        ) %>%
        DT::formatRound(
          columns = {
            data_table_temp %>%
              purrr::map(is.numeric) %>%
              purrr::flatten_lgl()
          },
          digits = 2
        )

    }
  )

  output$dwl_csv_button <- shiny::downloadHandler(
    filename = function() {
      'IFN_data.csv'
    },
    content = function(file) {
      if (is.null(input$col_vis_selector)) {
        data_res <- table_data_gen() %>%
          dplyr::filter(!!! col_filter_expressions())
      } else {
        data_res <- table_data_gen() %>%
          dplyr::filter(!!! col_filter_expressions()) %>%
          dplyr::select(dplyr::one_of(input$col_vis_selector))
      }

      readr::write_csv(data_res, file)
    }
  )

  output$dwl_xlsx_button <- shiny::downloadHandler(
    filename = function() {
      'IFN_data.xlsx'
    },
    content = function(file) {
      if (is.null(input$col_vis_selector)) {
        data_res <- table_data_gen() %>%
          dplyr::filter(!!! col_filter_expressions())
      } else {
        data_res <- table_data_gen() %>%
          dplyr::filter(!!! col_filter_expressions()) %>%
          dplyr::select(dplyr::one_of(input$col_vis_selector))
      }

      writexl::write_xlsx(data_res, file)
    }
  )

}