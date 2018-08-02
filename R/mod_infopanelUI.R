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
          'Visualització',
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

    # values
    clima_vars <- c('radiacioanual',
                    "temperaturaminimaanual",
                    "temperaturamitjanaanual",
                    "temperaturamaximaanual",
                    "precipitacioanual",
                    "npp_s")

    if (mod_data$color %in% clima_vars) {
      color_val <- rlang::quo(!!rlang::sym(
        glue::glue('{mod_data$tipo_grup_func}_dom_percdens_val')
      ))
    } else {
      color_val <- rlang::quo(!!rlang::sym(mod_data$color))
    }

    scenario <- get_scenario(mod_data$viz_shape, mod_data$agg_level)

    if (scenario == 'scenario1') {

      x_var_plot <- rlang::quo(!!rlang::sym(
        glue::glue('{mod_data$tipo_grup_func}_dom_percdens')
      ))

      if (isTRUE(mod_data$diameter_classes)) {
        infopanel_plot <- data_infopanel() %>%
          ggplot2::ggplot(
            ggplot2::aes(x = !! x_var_plot, y = !! color_val, group = idcd)
          ) +
          ggplot2::geom_col() +
          ggplot2::labs(title = 'Title', subtitle = 'subtitle')
      } else {
        infopanel_plot <- data_infopanel() %>%
          ggplot2::ggplot(ggplot2::aes(x = !! x_var_plot, y = !! color_val)) +
          ggplot2::geom_col() +
          ggplot2::labs(title = 'Title', subtitle = 'subtitle')
      }
    }

    if (scenario == 'scenario2') {

      x_var_plot <- rlang::quo(!!rlang::sym(
        glue::glue('id{mod_data$tipo_grup_func}')
      ))

      if (isTRUE(mod_data$diameter_classes)) {
        infopanel_plot <- data_infopanel() %>%
          ggplot2::ggplot(ggplot2::aes(x = !! x_var_plot, y = !! color_val, group = idcd)) +
          ggplot2::geom_col() +
          ggplot2::labs(title = 'Title', subtitle = 'subtitle')
      } else {
        infopanel_plot <- data_infopanel() %>%
          ggplot2::ggplot(ggplot2::aes(x = !! x_var_plot, y = !! color_val)) +
          ggplot2::geom_col() +
          ggplot2::labs(title = 'Title', subtitle = 'subtitle')
      }
    }


    return(infopanel_plot)


  })



}