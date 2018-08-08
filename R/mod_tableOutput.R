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

  col_vis_reactive <- shiny::eventReactive(
    ignoreInit = FALSE, ignoreNULL = FALSE,
    eventExpr = input$col_vis_apply,
    valueExpr = {

      if (is.null(input$col_vis_input) || all(!nzchar(input$col_vis_input))) {
        # select argument is everything()
        return(rlang::quo(dplyr::everything()))
      } else {
        # select arguments are the variables selected
        res <- rlang::quos(
          !!rlang::syms(input$col_vis_input)
        )
        return(res)
      }
    }
  )

  output$ifn_table <- DT::renderDT(
    server = TRUE,
    expr = {
      browser()

      table_data_gen()[['core']] %>%
        dplyr::select(!!! col_vis_reactive()) %>%
        dplyr::collect() %>%
        datatable(
          filter = list(position = 'top', clear = TRUE, plain = FALSE),
          style = 'default', rownames = FALSE,
          fillContainer = TRUE, autoHideNavigation = TRUE,
          extensions = c('Buttons', 'Scroller'),
          options = list(
            dom = 'tBi',
            extend = 'collection',
            buttons = c('csv', 'colvis'),
            text = 'Descàrrega',
            autoWidth = TRUE,
            deferRender = TRUE, scrollY = '70vh', scroller = TRUE
          )
        )
    }
  )

  shiny::observeEvent(
    eventExpr = input$col_vis_button,
    handlerExpr = {
      shiny::showModal(
        col_vis_modal(
          ns = session$ns,
          dictionary = dic_color_choices[['esp']][[get_scenario(mod_data$viz_shape, mod_data$agg_level)]]
        )
      )
    }
  )

}