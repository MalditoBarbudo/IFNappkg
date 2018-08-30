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
        2,
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

        shiny::br(),
        shiny::br(),

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
          shiny::uiOutput(ns('col_filter')),
          shiny::br(),
          shiny::br(),
          shinyWidgets::actionBttn(
            ns('apply_table_filters'), 'Aplicar filtros',
            icon = shiny::icon('eye'),
            style = "material-flat",
            block = FALSE,
            size = 'sm'
          )
        )
      ),
      shiny::column(
        10,
        # DT::DTOutput(ns('ifn_table')) %>%
        formattable::formattableOutput(ns('ifn_table')) %>%
          shinycssloaders::withSpinner(
            type = 4, color = '#D2527F'
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
#' @param mod_map reactive with the mod_map inputs
#' @param ifndb db pool
#'
#' @export
#'
#' @rdname mod_tableOutput
mod_table <- function(
  input, output, session,
  mod_data, mod_advancedFilters, mod_map, ifndb
) {

  scenario_reac <- shiny::reactive({
    get_scenario(mod_data$viz_shape, mod_data$agg_level)
  })

  shiny::observe({

    cd <- ifelse(mod_data$diameter_classes, 'cd', 'nocd')

    shinyWidgets::updatePickerInput(
      session, 'col_vis_selector', 'Show/Hide columns',
      choices = dic_col_vis_input[['esp']][[cd]][[scenario_reac()]],
      selected = dic_col_vis_input[['esp']][[cd]][[scenario_reac()]][1:10]
    )

    shinyWidgets::updatePickerInput(
      session, 'col_filter_selector', 'Select columns to filter by',
      choices = dic_col_vis_input[['esp']][[cd]][[scenario_reac()]]
    )
  })

  # base data reactives
  base_data_reactives <- shiny::reactive({
    base_data_reactives <- list()
    base_data_reactives$admin_div <- mod_data$admin_div
    # base_data_reactives$admin_div_fil <- mod_data$admin_div_fil
    base_data_reactives$espai_tipus <- mod_data$espai_tipus
    # base_data_reactives$espai_tipus_fil <- mod_data$espai_tipus_fil
    base_data_reactives$ifn <- mod_data$ifn
    base_data_reactives$agg_level <- mod_data$agg_level
    base_data_reactives$apply_filters <- mod_data$apply_filters
    base_data_reactives$diameter_classes <- mod_data$diameter_classes
    base_data_reactives$map_draw_new_feature <- mod_map$map_draw_new_feature
    base_data_reactives$map_draw_deleted_features <- mod_map$map_draw_deleted_features

    return(base_data_reactives)
  }) %>%
    shiny::debounce(millis = 500)

  # base data from data_scenario
  table_base_data <- shiny::eventReactive(
    ignoreInit = FALSE,
    eventExpr = base_data_reactives(),
    valueExpr = {

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
        mod_advancedFilters$adv_fil_sig_expressions(),
        mod_map$custom_polygon()
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
    }
  )

  output$col_filter <- shiny::renderUI({

    # get the session ns to be able to tag the inputs with correct id
    ns <- session$ns

    # diameter classes
    cd <- ifelse(mod_data$diameter_classes, 'cd', 'nocd')

    # we create the input list with lapply, easy peachy
    col_filter_inputs <- shiny::reactive({

      data_temp <- table_base_data()

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
      col_filter_inputs()
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
        data_temp <- table_base_data()

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

  # base_data_modifs_reactives
  base_data_modifs_reactives <- shiny::reactive({
    base_data_modifs_reactives <- list()
    base_data_modifs_reactives$table_base_data <- table_base_data()
    base_data_modifs_reactives$col_vis_selector <- input$col_vis_selector
    base_data_modifs_reactives$apply_table_filters <- input$apply_table_filters
  })

  # table final modifications based on table filters and col visibilty
  table_base_data_modifs <- shiny::eventReactive(
    ignoreInit = FALSE, ignoreNULL = FALSE,
    eventExpr = base_data_modifs_reactives(),
    valueExpr = {
      shiny::validate(
        shiny::need(table_base_data(), 'No hay datos')
      )

      if (is.null(input$col_vis_selector)) {
        data_table_temp <- table_base_data() %>%
          dplyr::filter(!!! col_filter_expressions())

        shiny::validate(
          shiny::need(
            nrow(data_table_temp) > 0,
            'Con los filtros actuales no se pueden mostrar datos'
          )
        )
      } else {
        data_table_temp <- table_base_data() %>%
          dplyr::filter(!!! col_filter_expressions()) %>%
          dplyr::select(dplyr::one_of(input$col_vis_selector))


        shiny::validate(
          shiny::need(
            nrow(data_table_temp) > 0,
            'Con los filtros actuales no se pueden mostrar datos'
          )
        )
      }

      # formattable accepts the format in a list with column names as list
      # names. So we need to create this list outside the formattable function
      # because we don't know the column names or indexes a priori
      num_cols_names <- data_table_temp %>%
        dplyr::select_if(is.numeric) %>%
        names()

      formattable_options <- lapply(
        num_cols_names, function(x) {
          formattable::color_tile('#E4F1FE', '#4B77BE')
        }
      )

      names(formattable_options) <- num_cols_names

      # formattable
      return(
        data_table_temp %>%
          dplyr::mutate_if(is.numeric, round, 2) %>%
          formattable::formattable(formattable_options)
      )
    }
  )

  # output$ifn_table <- DT::renderDT(
  output$ifn_table <- formattable::renderFormattable(
    # server = TRUE,
    expr = {

      table_base_data_modifs()

      # data_table_temp %>%
      #   DT::datatable(
      #     # filter = list(position = 'top', clear = FALSE, plain = TRUE),
      #     selection = list(target = 'column'),
      #     style = 'default', rownames = FALSE,
      #     fillContainer = TRUE, autoHideNavigation = TRUE,
      #     extensions = c('Scroller'),
      #     options = list(
      #       autoWidth = TRUE,
      #       deferRender = TRUE, scrollY = '70vh', scroller = TRUE,
      #       dom = 'ti'
      #     )
      #   ) %>%
      #   DT::formatRound(
      #     columns = {
      #       data_table_temp %>%
      #         purrr::map(is.numeric) %>%
      #         purrr::flatten_lgl()
      #     },
      #     digits = 2
      #   )
    }
  )

  output$dwl_csv_button <- shiny::downloadHandler(
    filename = function() {
      'IFN_data.csv'
    },
    content = function(file) {
      table_base_data_modifs() %>%
        dplyr::as_data_frame() %>%
        readr::write_csv(file)
    }
  )

  output$dwl_xlsx_button <- shiny::downloadHandler(
    filename = function() {
      'IFN_data.xlsx'
    },
    content = function(file) {
      table_base_data_modifs() %>%
        dplyr::as_data_frame() %>%
        writexl::write_xlsx(file)
    }
  )

}