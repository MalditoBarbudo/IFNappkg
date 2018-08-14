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
        # shiny::actionButton(ns('col_vis_button'), 'Show/Hide Cols'),
        shiny::downloadButton(ns('dwl_csv_button'), 'Save csv'),
        shiny::downloadButton(ns('dwl_xlsx_button'), 'Save xlsx'),
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
#'
#' @export
#'
#' @rdname mod_tableOutput
mod_table <- function(
  input, output, session,
  mod_data, mod_advancedFilters
) {

  scenario_reac <- shiny::reactive({
    get_scenario(mod_data$viz_shape, mod_data$agg_level)
  })

  table_data_gen <- shiny::reactive({
    data_scenario(
      mod_data$admin_div,
      mod_data$admin_div_fil,
      mod_data$espai_tipus,
      mod_data$espai_tipus_fil,
      mod_data$ifn,
      ifndb,
      mod_data$agg_level,
      mod_data$diameter_classes,
      mod_advancedFilters$adv_fil_clima_expressions()
    ) %>%
      table_data_modificator(
        scenario_reac(),
        mod_data$admin_div, mod_data$agg_level, mod_data$diameter_classes
      )
  })

  shiny::observe({
    cd <- ifelse(mod_data$diameter_classes, 'cd', 'nocd')

    shinyWidgets::updatePickerInput(
      session, 'col_vis_selector', 'Show/Hide columns',
      choices = dic_col_vis_input[['esp']][[cd]][[scenario_reac()]]
    )
  })


  # shiny::observeEvent(
  #   eventExpr = input$col_vis_button,
  #   handlerExpr = {
  #
  #     dictionary <- dic_col_vis_input[['esp']][[scenario_reac()]]
  #
  #     shiny::showModal(col_vis_modal(ns = session$ns, dictionary = dictionary))
  #   }
  # )

  # col_vis_reactive <- shiny::reactiveValues(columns = NULL)

  # shiny::observeEvent(
  #   eventExpr = input$col_vis_apply,
  #   handlerExpr = {
  #
  #     if (is.null(input$col_vis_input) || input$col_vis_input == '') {
  #
  #       dictionary <- dic_col_vis_input[['esp']][[scenario_reac()]]
  #
  #       shiny::showModal(
  #         col_vis_modal(failed = TRUE, ns = session$ns, dictionary = dictionary)
  #       )
  #     } else {
  #       # select arguments are the variables selected
  #       col_vis_reactive$columns <-  rlang::quo(
  #         dplyr::one_of(!!input$col_vis_input)
  #       )
  #       shiny::removeModal()
  #     }
  #   }
  # )

  # shiny::observeEvent(
  #   eventExpr = input$col_vis_reset,
  #   handlerExpr = {
  #     shinyjs::reset('col_vis_input')
  #   }
  # )

  # input_reactives <- shiny::reactive({
  #   input_reactives <- list()
  #   input_reactives$admin_div <- mod_data$admin_div
  #   input_reactives$espai_tipus <- mod_data$espai_tipus
  #   input_reactives$ifn <- mod_data$ifn
  #   input_reactives$agg_level <- mod_data$agg_level
  #   input_reactives$viz_shape <- mod_data$viz_shape
  #   input_reactives$apply_filters <- mod_data$apply_filters
  #
  #   return(input_reactives)
  # }) %>%
  #   shiny::debounce(millis = 500)
  #
  # shiny::observeEvent(
  #   eventExpr = input_reactives(),
  #   handlerExpr = {
  #     shinyjs::reset('col_vis_input')
  #   }
  # )

  output$dwl_csv_button <- shiny::downloadHandler(
    filename = function() {
      'IFN_data.csv'
    },
    content = function(file) {
      if (is.null(input$col_vis_selector) || input$col_vis_selector == '') {
        data_res <- table_data_gen()
      } else {
        data_res <- table_data_gen() %>%
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
      if (is.null(input$col_vis_selector) || input$col_vis_selector == '') {
        data_res <- table_data_gen()
      } else {
        data_res <- table_data_gen() %>%
          dplyr::select(dplyr::one_of(input$col_vis_selector))
      }

      writexl::write_xlsx(data_res, file)
    }
  )

  output$ifn_table <- DT::renderDT(
    server = TRUE,
    expr = {

      if (is.null(input$col_vis_selector)) {
        data_table_temp <- table_data_gen()
      } else {
        data_table_temp <- table_data_gen() %>%
          dplyr::select(dplyr::one_of(input$col_vis_selector))
      }

      data_table_temp %>%
        datatable(
          filter = list(position = 'top', clear = TRUE, plain = FALSE),
          style = 'default', rownames = FALSE,
          fillContainer = TRUE, autoHideNavigation = TRUE,
          extensions = c('Scroller'),
          options = list(
            dom = 'ti',
            autoWidth = TRUE,
            deferRender = TRUE, scrollY = '70vh', scroller = TRUE
          )
        ) %>%
        formatRound(
          columns = {
            data_table_temp %>%
              purrr::map(is.numeric) %>%
              purrr::flatten_lgl()
          },
          digits = 2
        )

    }
  )

}