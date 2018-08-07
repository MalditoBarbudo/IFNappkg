#' @title mod_infopanelUI and mod_infopanel
#'
#' @description A shiny module to generate the data tables
#'
#' @param id shiny id
#'
#' @export
mod_infopanelUI <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI elements
  shiny::tagList(

    shinyjs::disabled(
      shinyjs::hidden(
        shiny::div(
          id = ns('hiddeable_pan'),
          shiny::absolutePanel(
            # panel settings
            id = 'infoPanel', class = 'panel panel-default', fixed = TRUE,
            draggable = TRUE, width = 640, height = 'auto',
            top = 'auto', left = 'auto', bottom = 0, right = 15,

            # panel contents
            shiny::tabsetPanel(
              id = 'infoPanel_tabs', type = 'pills',

              shiny::tabPanel(
                'Info',
                shiny::uiOutput(ns('shape_click_info')),
                'Aquí va la info de la parcela o del polígono clickado'
              ),

              shiny::tabPanel(
                label_tabpanel_visualization[['esp']],
                shiny::plotOutput(
                  ns('shape_click_plot'), width = 600, height = 350
                ) %>%
                  shinycssloaders::withSpinner(
                    type = 4, color = '#D2527F'
                  )
              )
            )
          )
        )
      )
    )
  )
}

#' mod_infopanel server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param mod_data reactive with the reactive data and the data inputs
#' @param mod_map reactive with the map events from the map module
#' @param ifndb pool with database connection object
#'
#' @export
#'
#' @rdname mod_infoPanelOutput
mod_infopanel <- function(
  input, output, session,
  mod_data, mod_map, ifndb
) {

  # hide infoPanel
  shiny::observeEvent(
    eventExpr = {
      # all the inputs
      # data inputs
      mod_data$ifn
      mod_data$viz_shape
      mod_data$admin_div
      mod_data$espai_tipus
      mod_data$apply_filters
      mod_data$agg_level
      mod_data$diameter_classes
      # viz inputs
      mod_data$color
      mod_data$mida
      mod_data$inverse_pal
      mod_data$tipo_grup_func
      mod_data$grup_func
      mod_data$statistic
    },
    handlerExpr = {
      shinyjs::disable('hiddeable_pan')
      shinyjs::hide('hiddeable_pan')
    }
  )

  # observeEvent to show the panel when a shape is clicked
  shiny::observeEvent(
    mod_map$map_shape_click,
    {
      shinyjs::enable('hiddeable_pan')
      shinyjs::show('hiddeable_pan')
    }
  )

  # data reactive
  data_infopanel <- shiny::eventReactive(
    eventExpr = mod_map$map_shape_click,
    valueExpr = {

      # needed values
      click <- mod_map$map_shape_click
      click_fil <- dplyr::quo(!!rlang::sym(click$group) == click$id)

     # data
      data_scenario(
        mod_data$admin_div,
        mod_data$admin_div_fil,
        mod_data$espai_tipus,
        mod_data$espai_tipus_fil,
        mod_data$ifn,
        ifndb,
        mod_data$agg_level,
        mod_data$diameter_classes
      )[['core']] %>%
        dplyr::filter(!!! click_fil) %>%
        dplyr::collect()

    }
  )

  # plot reactive
  plot_infopanel <- shiny::eventReactive(
    eventExpr = data_infopanel(),
    valueExpr = {

      # validate that plotting is possible
      shiny::validate(
        shiny::need(
          expr = !is.null(data_infopanel()[[mod_data$color]]),
          message = 'Selected color variable is not available in the data'
        )
      )

      # create the plot
      infopanel_plot_gen(
        data_infopanel, mod_map$map_shape_click, mod_data$color,
        mod_data$viz_shape, mod_data$agg_level, mod_data$diameter_classes,
        mod_data$tipo_grup_func
      )
    }
  )

  output$shape_click_plot <- shiny::renderPlot({
    plot_infopanel()
  })
}