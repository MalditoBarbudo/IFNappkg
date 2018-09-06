#' @title mod_mapUI and mod_map
#'
#' @description A shiny module to generate the data tables
#'
#' @param id shiny id
#'
#' @export
mod_mapUI <- function(id) {

  # ns
  ns <- shiny::NS(id)

  leaflet::leafletOutput(ns('map'), width = '100%', height = '100%')

}

#' mod_map server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param mod_data reactive with the reactive data and the data inputs
#' @param mod_advancedFilters reactive with the reactive values from the
#'   advancedFilters module
#' @param ifndb pool with database connection object
#'
#' @export
#'
#' @rdname mod_mapUI
mod_map <- function(
  input, output, session,
  mod_data, mod_advancedFilters, ifndb
) {

  # noms division (we need for the polygon dictionary call later on)
  nom_provincies <- as.character(polygons_provincies@data$NOMPROV)
  nom_vegueries <- as.character(polygons_vegueries@data$NOMVEGUE)
  nom_comarques <- as.character(polygons_comarques@data$NOMCOMAR)
  nom_municipis <- as.character(polygons_municipis@data$NOMMUNI)
  # noms proteccions
  nom_enpe <- as.character(polygons_enpe@data$nom)
  nom_pein <- as.character(polygons_pein@data$nom)
  nom_xn2000 <- as.character(polygons_xn2000@data$nom_n2)

  # basic map
  # here we only set the view, zoom and the panes for managing the zIndex)
  base_map <- shiny::reactive({
    leaflet::leaflet() %>%
      leaflet::setView(0.8, 41.67, zoom = 8) %>%
      leaflet::addMapPane('admin_divs', zIndex = 410) %>%
      leaflet::addMapPane('proteccions', zIndex = 405) %>%
      leaflet::addMapPane('parceles', zIndex = 420) %>%
      # leaflet.extras r package plugins
      leaflet.extras::addDrawToolbar(
        targetGroup = 'custom_poly',
        position = 'topleft',
        polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE,
        markerOptions = FALSE, circleMarkerOptions = FALSE,
        polygonOptions = leaflet.extras::drawPolygonOptions(
          shapeOptions = leaflet.extras::drawShapeOptions()
        ),
        editOptions = leaflet.extras::editToolbarOptions(
          edit = TRUE, remove = TRUE
        ),
        singleFeature = TRUE
      ) %>%
      # raw easyPrint plugin
      htmlwidgets::onRender(
        "function(el, x) {
        L.easyPrint({
        title: '',
        sizeModes: ['A4Landscape', 'A4Portrait'],
        filename: 'IFNmap',
        exportOnly: true,
        hideControlContainer: false
        }).addTo(this);
        }"
      )
  })
  output$map <- leaflet::renderLeaflet({
    base_map()
  })

  # observer for admin divs polygons. We use this instead of add polygons
  # directly in the map and control them with the default addLayersControl
  # because some ids are identical between polygons layers (i.e. Barcelona in
  # provincies and comarques) which causes some polygons to dissapear after
  # drawing. Also, in this way the app load is faster, but the polygon layer is
  # slower, though. So we control the polygons drawing with a classic
  # input-observer pair, as we do with the parceles circles.
  shiny::observeEvent(
    eventExpr = mod_data$admin_div,
    handlerExpr = {
      admin_div <- mod_data$admin_div

      if (admin_div == '') {
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('vegueria') %>%
          leaflet::clearGroup('comarca') %>%
          leaflet::clearGroup('municipi') %>%
          leaflet::clearGroup('provincia')
      } else {
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('vegueria') %>%
          leaflet::clearGroup('comarca') %>%
          leaflet::clearGroup('municipi') %>%
          leaflet::clearGroup('provincia') %>%
          leaflet::addPolygons(
            data = rlang::eval_tidy(rlang::sym(polygons_dictionary[[admin_div]][['polygon']])),
            group = polygons_dictionary[[admin_div]][['group']],
            label = polygons_dictionary[[admin_div]][['label']],
            layerId = rlang::eval_tidy(rlang::sym(polygons_dictionary[[admin_div]][['layerId']])),
            weight = 1, smoothFactor = 1,
            opacity = 1.0, fill = TRUE,
            color = '#6C7A89FF', fillColor = "#CF000F00",
            highlightOptions = leaflet::highlightOptions(
              color = "#CF000F", weight = 2,
              bringToFront = FALSE,
              fill = TRUE, fillColor = "#CF000F00"
            ),
            options = leaflet::pathOptions(
              pane = 'admin_divs'
            )
          )
      }
    }

  )

  # observer for proteccions polygons, same as above
  shiny::observeEvent(
    eventExpr = mod_data$espai_tipus,
    handlerExpr = {

      espai_tipus <- mod_data$espai_tipus
      if (is.null(espai_tipus)) {
        return()
      }

      if (espai_tipus == 'proteccio') {
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('enpes') %>%
          leaflet::clearGroup('nomxarxa2000') %>%
          leaflet::clearGroup('nomein')
      } else {
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('enpes') %>%
          leaflet::clearGroup('nomxarxa2000') %>%
          leaflet::clearGroup('nomein') %>%
          leaflet::addPolygons(
            data = rlang::eval_tidy(rlang::sym(polygons_dictionary[[espai_tipus]][['polygon']])),
            group = polygons_dictionary[[espai_tipus]][['group']],
            label = polygons_dictionary[[espai_tipus]][['label']],
            layerId = rlang::eval_tidy(rlang::sym(polygons_dictionary[[espai_tipus]][['layerId']])),
            weight = 1, smoothFactor = 1,
            opacity = 1.0, fill = TRUE,
            color = '#6C7A89FF', fillColor = "#CF000F00",
            highlightOptions = leaflet::highlightOptions(
              color = "#CF000F", weight = 2,
              bringToFront = FALSE,
              fill = TRUE, fillColor = "#CF000F00"
            ),
            options = leaflet::pathOptions(
              pane = 'proteccions'
            )
          )
      }
    }
  )

  # capture the custom polygon (if any) to use it later
  custom_polygon <- shiny::reactive({

    # When removing the features (custom polygon) the input$map_draw_new_feature
    # is not cleared, so is always filtering the sites, even after removing. For
    # that we need to control when the removed feature equals the new, that's it,
    # when we removed the last one
    res <- input$map_draw_all_features
    if (is.null(res) || length(res[['features']]) == 0) {
      return(NULL)
    } else {
      return(res[['features']][[1]])
    }

    # res <- input$map_draw_new_feature
    # removed_id <- input$map_draw_deleted_features$features[[1]]$properties$`_leaflet_id`
    # res_id <- input$map_draw_new_feature$properties$`_leaflet_id`
    #
    # # some iferation
    # if (is.null(res)) {
    #   return(NULL)
    # } else {
    #   if (is.null(removed_id)) {
    #     return(res)
    #   } else {
    #     if (removed_id == res_id) {
    #       return(NULL)
    #     } else {
    #       return(res)
    #     }
    #   }
    # }
  })

  # update inputs with variables present in data. We have four input scenarios
  # so we build a reactive to know which scenario we have using the get_scenario
  # function from global.R
  input_scenario <- shiny::reactive({
    get_scenario(mod_data$viz_shape, mod_data$agg_level)
  })

  # inputs to monitorize
  # input_reactives <- shiny::reactive({
  #   input_reactives <- list()
  #   input_reactives$admin_div <- mod_data$admin_div
  #   # input_reactives$admin_div_fil <- mod_data$admin_div_fil
  #   input_reactives$espai_tipus <- mod_data$espai_tipus
  #   # input_reactives$espai_tipus_fil <- mod_data$espai_tipus_fil
  #   input_reactives$ifn <- mod_data$ifn
  #   input_reactives$inverse_pal <- mod_data$inverse_pal
  #   input_reactives$color <- mod_data$color
  #   input_reactives$mida <- mod_data$mida
  #   input_reactives$tipo_grup_func <- mod_data$tipo_grup_func
  #   input_reactives$grup_func <- mod_data$grup_func
  #   input_reactives$statistic <- mod_data$statistic
  #   input_reactives$agg_level <- mod_data$agg_level
  #   input_reactives$viz_shape <- mod_data$viz_shape
  #   input_reactives$apply_filters <- mod_data$apply_filters
  #   input_reactives$map_draw_new_feature <- input$map_draw_new_feature
  #   input_reactives$map_draw_deleted_features <- input$map_draw_deleted_features
  #
  #   return(input_reactives)
  # }) %>%
  #   shiny::debounce(millis = 500)

  base_data_reactives <- shiny::reactive({
    base_data_reactives <- list()
    base_data_reactives$admin_div <- mod_data$admin_div
    # base_data_reactives$admin_div_fil <- mod_data$admin_div_fil
    base_data_reactives$espai_tipus <- mod_data$espai_tipus
    # base_data_reactives$espai_tipus_fil <- mod_data$espai_tipus_fil
    base_data_reactives$ifn <- mod_data$ifn
    base_data_reactives$agg_level <- mod_data$agg_level
    base_data_reactives$apply_filters <- mod_data$apply_filters
    # base_data_reactives$map_draw_new_feature <- input$map_draw_new_feature
    # base_data_reactives$map_draw_deleted_features <- input$map_draw_deleted_features
    base_data_reactives$map_draw_all_features <- input$map_draw_all_features

    return(base_data_reactives)
  }) %>%
    shiny::debounce(millis = 500)

  base_data_modifs_reactives <- shiny::reactive({
    base_data_modifs_reactives <- list()
    base_data_modifs_reactives$ifn <- mod_data$ifn
    base_data_modifs_reactives$admin_div <- mod_data$admin_div
    base_data_modifs_reactives$inverse_pal <- mod_data$inverse_pal
    base_data_modifs_reactives$color <- mod_data$color
    base_data_modifs_reactives$mida <- mod_data$mida
    base_data_modifs_reactives$tipo_grup_func <- mod_data$tipo_grup_func
    base_data_modifs_reactives$grup_func <- mod_data$grup_func
    base_data_modifs_reactives$statistic <- mod_data$statistic
    base_data_modifs_reactives$agg_level <- mod_data$agg_level
    base_data_modifs_reactives$viz_shape <- mod_data$viz_shape
    base_data_modifs_reactives$apply_filters <- mod_data$apply_filters
    base_data_modifs_reactives$map_draw_all_features <- input$map_draw_all_features
    # base_data_modifs_reactives$map_draw_new_feature <- input$map_draw_new_feature
    # base_data_modifs_reactives$map_draw_deleted_features <- input$map_draw_deleted_features

    return(base_data_modifs_reactives)
  }) %>%
    shiny::debounce(millis = 500)

  map_base_data <- shiny::eventReactive(
    ignoreInit = FALSE,
    eventExpr = base_data_reactives(),
    valueExpr = {

      data_scenario_map <- data_scenario(
        mod_data$admin_div,
        mod_data$admin_div_fil,
        mod_data$espai_tipus,
        mod_data$espai_tipus_fil,
        mod_data$ifn,
        ifndb,
        mod_data$agg_level,
        diameter_classes = FALSE,
        mod_advancedFilters$adv_fil_clima_expressions(),
        mod_advancedFilters$adv_fil_sig_expressions(),
        custom_polygon()
      )

      # check data integrity (zero rows)
      if (
        {
          data_scenario_map[['clima']] %>%
            dplyr::collect() %>%
            nrow()
        } < 1
      ) {

        shinyWidgets::sendSweetAlert(
          session, title = 'Sin datos',
          text = 'Con los filtros actuales activados no hay parcelas que cumplan los requisitos',
          type = 'warning'
        )

        return()

      } else {
        return(data_scenario_map)
      }
    }
  )

  map_base_data_modifs <- shiny::eventReactive(
    ignoreInit = TRUE,
    eventExpr = base_data_modifs_reactives(),
    valueExpr = {

      # create a progress object to indicate the user this will take time
      progress <- shiny::Progress$new()
      progress$set(value = 0, message = 'Procesando el mapa...')
      on.exit(progress$close())

      updateProgress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }


      map_base_data() %>%
        map_modificator(
          input_scenario(),
          mod_data$ifn,
          mod_data$inverse_pal,
          mod_data$color,
          mod_data$mida,
          mod_data$tipo_grup_func,
          mod_data$grup_func,
          mod_data$statistic,
          mod_data$admin_div,
          mod_data$agg_level,
          updateProgress = updateProgress
        )
    }
  )

  shiny::observeEvent(
    ignoreInit = TRUE,
    eventExpr = map_base_data_modifs(),
    handlerExpr = {
      map_base_data_modifs()
    }
  )


  # input_map <- shiny::eventReactive(
  #   ignoreInit = TRUE,
  #   eventExpr = {
  #     input_reactives()
  #   },
  #   valueExpr = {
  #
  #     data_scenario_map <- data_scenario(
  #       mod_data$admin_div,
  #       mod_data$admin_div_fil,
  #       mod_data$espai_tipus,
  #       mod_data$espai_tipus_fil,
  #       mod_data$ifn,
  #       ifndb,
  #       mod_data$agg_level,
  #       diameter_classes = FALSE,
  #       mod_advancedFilters$adv_fil_clima_expressions(),
  #       mod_advancedFilters$adv_fil_sig_expressions(),
  #       custom_polygon()
  #     )
  #
  #     # check data integrity (zero rows)
  #     if (
  #       {
  #         data_scenario_map[['clima']] %>%
  #           dplyr::collect() %>%
  #           nrow()
  #       } < 1
  #     ) {
  #
  #       shinyWidgets::sendSweetAlert(
  #         session, title = 'Sin datos',
  #         text = 'Con los filtros actuales activados no hay parcelas que cumplan los requisitos',
  #         type = 'warning'
  #       )
  #
  #       return()
  #
  #     } else {
  #       data_scenario_map %>%
  #         map_modificator(
  #           input_scenario(),
  #           mod_data$ifn,
  #           mod_data$inverse_pal,
  #           mod_data$color,
  #           mod_data$mida,
  #           mod_data$tipo_grup_func,
  #           mod_data$grup_func,
  #           mod_data$statistic,
  #           mod_data$admin_div,
  #           mod_data$agg_level
  #         )
  #     }
  #   }
  # )



  # reactive with the map events
  map_reactives <- shiny::reactiveValues()

  shiny::observe({
    map_reactives$map_shape_click <- input$map_shape_click
    map_reactives$base_map <- base_map
    map_reactives$input_map <- map_base_data_modifs
    map_reactives$map_draw_start <- input$map_draw_start
    map_reactives$map_draw_stop <- input$map_draw_stop
    map_reactives$map_draw_new_feature <- input$map_draw_new_feature
    map_reactives$map_draw_edited_features <- input$map_draw_edited_features
    map_reactives$map_draw_deleted_features <- input$map_draw_deleted_features
    map_reactives$map_draw_all_features <- input$map_draw_all_features
    map_reactives$custom_polygon <- custom_polygon
    # map_reactives$shape_mouseover <- input$map_shape_mouseover
    # map_reactives$shape_mouseout <- input$map_shape_mouseout
    # map_reactives$map_click <- input$map_click
    # map_reactives$map_bounds <- input$map_bounds
    # map_reactives$map_zoom <- input$map_zoom
    # map_reactives$map_center <- input$map_center
  })

  return(map_reactives)

}