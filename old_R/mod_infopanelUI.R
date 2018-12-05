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
  return()
}

#' mod_infopanel server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param mod_data reactive with the reactive data and the data inputs
#' @param mod_map reactive with the map events from the map module
#' @param mod_advancedFilters reactive with the reactive values from
#'   advancedFilters module
#' @param ifndb pool with database connection object
#'
#' @export
#'
#' @rdname mod_infoPanelOutput
mod_infopanel <- function(
  input, output, session,
  mod_data, mod_map, mod_advancedFilters, ifndb
) {

  # data reactive
  data_infopanel <- shiny::eventReactive(
    eventExpr = mod_map$map_shape_click,
    valueExpr = {

      # needed values
      click <- mod_map$map_shape_click
      click_fil <- dplyr::quo(!!rlang::sym(click$group) == click$id)

     # data
      temp_res <- data_scenario(
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

      shiny::validate(
        shiny::need(
          {temp_res[['clima']] %>%
              dplyr::collect() %>%
              nrow()} > 0,
          'No hay datos'
        )
      )

      temp_res[['core']] <- temp_res[['core']] %>%
        dplyr::filter(!!! click_fil)

      temp_res[['sig']] <- temp_res[['sig']] %>%
        dplyr::filter(!!! click_fil)

      shiny::validate(
        shiny::need(
          {temp_res[['sig']] %>%
              dplyr::collect() %>%
              nrow()} > 0,
          'No hay datos'
        )
      )

      plots <- temp_res[['sig']] %>%
        dplyr::select(idparcela) %>%
        dplyr::collect() %>%
        purrr::flatten_chr()

      temp_res[['clima_nf']] <- temp_res[['clima']]

      temp_res[['clima']] <- temp_res[['clima']] %>%
        dplyr::filter(idparcela %in% plots)

      return(temp_res)
    }
  )

  # plot reactive
  plot_infopanel <- shiny::eventReactive(
    eventExpr = data_infopanel(),
    valueExpr = {

      # validate that plotting is possible
      core <- data_infopanel()[['core']] %>%
        dplyr::collect()
      clima <- data_infopanel()[['clima']] %>%
        dplyr::collect()

      # validate the reactive
      shiny::validate(
        shiny::need(
          expr = !all(
            is.null(core[[mod_data$color]]), is.null(clima[[mod_data$color]])
          ),
          message = 'Selected color variable is not available in the data'
        )
      )

      clima_vars <- c(
        'radiacioanual', "temperaturaminimaanual", "temperaturamitjanaanual",
        "temperaturamaximaanual", "precipitacioanual", "npp_s"
      )

      # create the plot
      if (mod_data$color %in% clima_vars) {
        # plot for climate
        infopanel_climaplot_gen(
          data_infopanel(), mod_data$color, mod_map$map_shape_click
        )
      } else {
        # plot for core vars
        infopanel_plot_gen(
          core, mod_map$map_shape_click, mod_data$color,
          mod_data$viz_shape, mod_data$agg_level, mod_data$diameter_classes,
          mod_data$tipo_grup_func, ifndb
        )
      }
    }
  )

  output$shape_click_plot <- shiny::renderPlot({
    plot_infopanel()
  })

  output$shape_click_info <- shiny::renderUI({

    click <- mod_map$map_shape_click

    if (click$group == 'idparcela') {

      sig <- data_infopanel()[['sig']] %>%
        dplyr::collect()
      clima <- data_infopanel()[['clima']] %>%
        dplyr::collect()

      shiny::tagList(
        # header
        shiny::h4(
          glue::glue(
            label_getter(ifndb, 'esp', 'shape_click_info_label', 'plot', 'header')
          )
        ),
        # muni
        glue::glue(
          label_getter(ifndb, 'esp', 'shape_click_info_label', 'plot', 'muni')
        ),
        # comarca
        glue::glue(
          label_getter(ifndb, 'esp', 'shape_click_info_label', 'plot', 'comarca')
        ),
        # province
        glue::glue(
          label_getter(ifndb, 'esp', 'shape_click_info_label', 'plot', 'province')
        ),

        shiny::br(),

        # altitude
        glue::glue(
          label_getter(ifndb, 'esp', 'shape_click_info_label', 'plot', 'altitude')
        ),
        # slope
        glue::glue(
          label_getter(ifndb, 'esp', 'shape_click_info_label', 'plot', 'slope')
        ),
        # an_rad
        glue::glue(
          label_getter(ifndb, 'esp', 'shape_click_info_label', 'plot', 'an_rad')
        ),
        # an_ave_temp
        glue::glue(
          label_getter(ifndb, 'esp', 'shape_click_info_label', 'plot', 'an_ave_temp')
        ),
        # an_prec
        glue::glue(
          label_getter(ifndb, 'esp', 'shape_click_info_label', 'plot', 'an_prec')
        )
      )


    } else {

      sig <- data_infopanel()[['sig']] %>%
        dplyr::collect()
      clima <- data_infopanel()[['clima']] %>%
        dplyr::collect()

      shiny::tagList(
        # header
        shiny::h4(
          glue::glue(
            label_getter(ifndb, 'esp', 'shape_click_info_label', 'polygon', 'header')
          )
        ),
         # explanation
        glue::glue(
          label_getter(ifndb, 'esp', 'shape_click_info_label', 'polygon', 'explanation')
        )
      )


    }

  })

  # shiny::observeEvent(
  #   eventExpr = mod_map$map_shape_click,
  #   handlerExpr = {
  #     # ns
  #     ns <- session$ns
  #
  #     # dummy modal function
  #     infoPlot_modal <- function(click) {
  #       shiny::modalDialog(
  #         shiny::tags$div(
  #           id = 'infoPlot_tmp',
  #           shiny::plotOutput(
  #             ns('shape_click_plot')
  #           ) %>%
  #             shinycssloaders::withSpinner(
  #               type = 4, color = '#D2527F'
  #             )
  #         ),
  #         title = label_getter(
  #           ifndb, 'esp', 'sweetalert_shape_click_info_label', 'title'
  #         ),
  #         footer = shiny::modalButton(
  #           label_getter(
  #             ifndb, 'esp', 'sweetalert_shape_click_info_label', 'btn_labels'
  #           )
  #         ),
  #         size = 'l',
  #         easyClose = TRUE
  #       )
  #     }
  #
  #     # modal
  #     shiny::showModal(
  #       infoPlot_modal(mod_map$map_shape_click),
  #       session = session
  #     )
  #   }
  # )

  # observer to create the sweet alert
  shiny::observeEvent(
    eventExpr = mod_map$map_shape_click,
    handlerExpr = {
      # ns
      ns <- session$ns

      # sweetalert
      shinyWidgets::sendSweetAlert(
        session = session,
        title = label_getter(ifndb, 'esp', 'sweetalert_shape_click_info_label', 'title'),

        # here in text we insert the UI again
        text = shiny::tags$div(
          id = 'infoPlot_tmp',
          shiny::plotOutput(
            ns('shape_click_plot')
          ) %>%
            shinycssloaders::withSpinner(
              type = 4, color = '#D2527F'
            )
        ),
        html = TRUE,
        btn_labels = label_getter(ifndb, 'esp', 'sweetalert_shape_click_info_label', 'btn_labels')
      )
    }
  )
}