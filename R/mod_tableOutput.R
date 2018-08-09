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
        shiny::actionButton(ns('col_vis_button'), 'Show/Hide Cols')
      )
    )
  )
}

#' mod_table server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param mod_data reactive with the reactive data and the data inputs
#'
#' @export
#'
#' @rdname mod_tableOutput
mod_table <- function(
  input, output, session,
  mod_data
) {

  table_data_gen <- shiny::reactive({

    data_scenario(
      mod_data$admin_div,
      mod_data$admin_div_fil,
      mod_data$espai_tipus,
      mod_data$espai_tipus_fil,
      mod_data$ifn,
      ifndb,
      mod_data$agg_level,
      diameter_classes = FALSE
    )

  })

  shiny::observeEvent(
    eventExpr = input$col_vis_button,
    handlerExpr = {
      shiny::showModal(
        col_vis_modal(
          ns = session$ns,
          dictionary = dic_color_choices[['esp']][[get_scenario(
            mod_data$viz_shape, mod_data$agg_level
          )]]
        )
      )
    }
  )

  col_vis_reactive <- shiny::reactiveValues(columns = NULL)

  shiny::observeEvent(
    eventExpr = input$col_vis_apply,
    handlerExpr = {
      if (is.null(input$col_vis_input)) {
        shiny::showModal(
          col_vis_modal(
            failed = TRUE, ns = session$ns,
            dictionary = dic_color_choices[['esp']][[get_scenario(
              mod_data$viz_shape, mod_data$agg_level
            )]]
          )
        )
      } else {
        # select arguments are the variables selected
        col_vis_reactive$columns <-  rlang::quo(
          dplyr::one_of(!!input$col_vis_input)
        )
        shiny::removeModal()
      }
    }
  )

  output$ifn_table <- DT::renderDT(
    server = TRUE,
    expr = {

      if (is.null(col_vis_reactive$columns)) {
        data_table_temp <- table_data_gen()[['core']] %>%
          dplyr::collect()
      } else {
        data_table_temp <- table_data_gen()[['core']] %>%
          dplyr::select(!!! col_vis_reactive$columns) %>%
          dplyr::collect()
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