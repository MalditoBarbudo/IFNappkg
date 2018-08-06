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
          shiny::plotOutput(ns('shape_click_plot'), width = 600, height = 350) %>%
            shinycssloaders::withSpinner(
              type = 4, color = '#D2527F'
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

  output$shape_click_plot <- shiny::renderPlot({

    shiny::validate(
      shiny::need(
        expr = !is.null(data_infopanel()[[mod_data$color]]),
        message = 'Selected color variable is not available in the data'
      )
    )

    infopanel_plot_gen(
      data_infopanel, mod_map$map_shape_click, mod_data$color,
      mod_data$viz_shape, mod_data$agg_level, mod_data$diameter_classes,
      mod_data$tipo_grup_func
    )

    # # click
    # click <- mod_map$map_shape_click
    #
    # # values
    # clima_vars <- c('radiacioanual',
    #                 "temperaturaminimaanual",
    #                 "temperaturamitjanaanual",
    #                 "temperaturamaximaanual",
    #                 "precipitacioanual",
    #                 "npp_s")
    #
    # if (mod_data$color %in% clima_vars) {
    #   color_val <- rlang::quo(!!rlang::sym(
    #     glue::glue('{mod_data$tipo_grup_func}_dom_percdens_val')
    #   ))
    # } else {
    #   color_val <- rlang::quo(!!rlang::sym(mod_data$color))
    # }
    #
    # scenario <- get_scenario(mod_data$viz_shape, mod_data$agg_level)
    #
    # # x var
    # if (isTRUE(mod_data$diameter_classes)) {
    #   x_var_plot <- rlang::quo(!!rlang::sym('idcd'))
    # } else {
    #   if (scenario %in% c('scenario1', 'scenario3')) {
    #     x_var_plot <- rlang::quo(!!rlang::sym(
    #       glue::glue('{mod_data$tipo_grup_func}_dom_percdens')
    #     ))
    #   } else {
    #     x_var_plot <- rlang::quo(!!rlang::sym(
    #       glue::glue('id{mod_data$agg_level}')
    #     ))
    #   }
    # }
    #
    # x_var_index <- as.character(rlang::get_expr(x_var_plot))
    # y_var_index <- as.character(rlang::get_expr(color_val))
    #
    # plot_y_lab <- label_infopanel_variables[['esp']][[y_var_index]]
    # plot_x_lab <- label_infopanel_variables[['esp']][[x_var_index]]
    #
    # browser()
    #
    # # click in plot
    # if (click$group == 'idparcela') {
    #
    #   plot_title <- glue::glue(
    #     label_infopanel_plot[['esp']][['parcela']][['title']]
    #   )
    #   plot_subt <- glue::glue(
    #     label_infopanel_plot[['esp']][['parcela']][['subtitle']]
    #   )
    #
    #   if (isTRUE(mod_data$diameter_classes)) {
    #     infopanel_plot <- data_infopanel() %>%
    #       ggplot2::ggplot(
    #         ggplot2::aes(x = !! x_var_plot, y = !! color_val, group = idcd)
    #       ) +
    #       ggplot2::geom_col() +
    #       ggplot2::labs(
    #         title = plot_title, subtitle = plot_subt,
    #         x = plot_x_lab, y = plot_y_lab
    #       )
    #   } else {
    #     infopanel_plot <- data_infopanel() %>%
    #       ggplot2::ggplot(ggplot2::aes(x = !! x_var_plot, y = !! color_val)) +
    #       ggplot2::geom_col() +
    #       ggplot2::labs(
    #         title = plot_title, subtitle = plot_subt,
    #         x = plot_x_lab, y = plot_y_lab
    #       )
    #   }
    #
    # } else {
    #
    #   plot_title <- glue::glue(
    #     label_infopanel_plot[['esp']][['polygon']][['title']]
    #   )
    #   plot_subt <- glue::glue(
    #     label_infopanel_plot[['esp']][['polygon']][['subtitle']]
    #   )
    #
    #   # click in polygon
    #   if (isTRUE(mod_data$diameter_classes)) {
    #     infopanel_plot <- data_infopanel() %>%
    #       ggplot2::ggplot(
    #         ggplot2::aes(x = !! x_var_plot, y = !! color_val, group = idcd)
    #       ) +
    #       ggplot2::geom_boxplot() +
    #       ggplot2::labs(
    #         title = plot_title, subtitle = plot_subt,
    #         x = plot_x_lab, y = plot_y_lab
    #       )
    #   } else {
    #     infopanel_plot <- data_infopanel() %>%
    #       ggplot2::ggplot(ggplot2::aes(x = !! x_var_plot, y = !! color_val)) +
    #       ggplot2::geom_boxplot() +
    #       ggplot2::labs(
    #         title = plot_title, subtitle = plot_subt,
    #         x = plot_x_lab, y = plot_y_lab
    #       )
    #   }
    # }
    #
    # return(infopanel_plot)
  })
}