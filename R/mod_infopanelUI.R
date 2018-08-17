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
        mod_advancedFilters$adv_fil_sig_expressions()
      )

      shiny::validate(
        shiny::need(
          {temp_res[['clima']] %>%
              collect() %>%
              nrow()} > 0,
          'No hay datos'
        )
      )

      temp_res[['core']] <- temp_res[['core']] %>%
        dplyr::filter(!!! click_fil)

      temp_res[['sig']] <- temp_res[['sig']] %>%
        dplyr::filter(!!! click_fil)

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
          mod_data$tipo_grup_func
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
          glue::glue(label_shape_click_info[['esp']][['plot']][['header']])
        ),
        # muni
        glue::glue(label_shape_click_info[['esp']][['plot']][['muni']]),
        # comarca
        glue::glue(label_shape_click_info[['esp']][['plot']][['comarca']]),
        # province
        glue::glue(label_shape_click_info[['esp']][['plot']][['province']]),

        shiny::br(),

        # altitude
        glue::glue(label_shape_click_info[['esp']][['plot']][['altitude']]),
        # slope
        glue::glue(label_shape_click_info[['esp']][['plot']][['slope']]),
        # an_rad
        glue::glue(label_shape_click_info[['esp']][['plot']][['an_rad']]),
        # an_ave_temp
        glue::glue(label_shape_click_info[['esp']][['plot']][['an_ave_temp']]),
        # an_prec
        glue::glue(label_shape_click_info[['esp']][['plot']][['an_prec']])
      )


    } else {

      sig <- data_infopanel()[['sig']] %>%
        dplyr::collect()
      clima <- data_infopanel()[['clima']] %>%
        dplyr::collect()

      shiny::tagList(
        # header
        shiny::h4(
          glue::glue(label_shape_click_info[['esp']][['polygon']][['header']])
        ),
         # explanation
        glue::glue(label_shape_click_info[['esp']][['polygon']][['explanation']])
      )


    }

  })

}