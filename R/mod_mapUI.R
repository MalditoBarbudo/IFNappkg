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
#' @param ifndb pool with database connection object
#'
#' @export
#'
#' @rdname mod_mapUI
mod_map <- function(
  input, output, session,
  mod_data, ifndb
) {

  # noms division
  nom_provincies <- as.character(polygons_provincies@data$NOM_PROV)
  nom_vegueries <- as.character(polygons_vegueries@data$NOMVEGUE)
  nom_comarques <- as.character(polygons_comarques@data$NOM_COMAR)
  nom_municipis <- as.character(polygons_municipis@data$NOM_MUNI)
  # noms proteccions
  nom_enpe <- as.character(polygons_enpe@data$nom)
  nom_pein <- as.character(polygons_pein@data$nom)
  nom_xn2000 <- as.character(polygons_xn2000@data$nom_n2)

  # basic map
  # here we only set the view, zoom and the panes for managing the zIndex)
  output$map <- leaflet::renderLeaflet({

    leaflet::leaflet() %>%
      leaflet::setView(0.8, 41.67, zoom = 8) %>%
      leaflet::addMapPane('admin_divs', zIndex = 410) %>%
      leaflet::addMapPane('proteccions', zIndex = 405) %>%
      leaflet::addMapPane('parceles', zIndex = 420)
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

  # update inputs with variables present in data. We have four input scenarios
  # so we build a reactive to know which scenario we have using the get_scenario
  # function from global.R
  input_scenario <- shiny::reactive({
    get_scenario(mod_data$viz_shape, mod_data$agg_level)
  })

  data_scenario <- shiny::reactive({

    ## SIG data
    # admin_div filter
    if (is.null(mod_data$admin_div_fil)) {
      filter_expr_admin <- rlang::quo(TRUE)
    } else {
      filter_expr_admin <- rlang::quo(
        !!rlang::sym(mod_data$admin_div) %in% !!mod_data$admin_div_fil
      )
    }

    # espai tipus filter
    if (is.null(mod_data$espai_tipus_fil) ||
          any(mod_data$espai_tipus_fil == 'Tots')) {
      filter_expr_espai <- rlang::quo(TRUE)
    } else {
      # here we need also to check for nomes protegits and sense proteccio
      # to be able to filter these cases
      if (any(mod_data$espai_tipus_fil %in% c(
        'Només protegits',
        "Només espais d'interès nacional",
        "Només espai de protecció especial",
        "Només en Xarxa Natura 2000"
      ))) {
        filter_expr_espai <- rlang::quo(
          !(!!rlang::sym(mod_data$espai_tipus) %in% c(
            "Sense Pein", "Sense protecció", "SenseXarxa"
          ))
        )
      } else {
        filter_expr_espai <- rlang::quo(
          !!rlang::sym(mod_data$espai_tipus) %in% !!mod_data$espai_tipus_fil
        )
      }
    }

    sig_filters <- list(filter_expr_admin, filter_expr_espai)

    sig <- tidyIFN::data_sig(mod_data$ifn, ifndb, !!! sig_filters)

    ## CLIMA data
    clima_filters <- quo(TRUE)
    clima <- tidyIFN::data_clima(sig, mod_data$ifn, ifndb, !!! clima_filters)

    ## CORE data
    core <- tidyIFN::data_core(
      sig, mod_data$ifn, mod_data$agg_level, ifndb, clima[['idparcela']]
    )

    res_list <- list(
      sig = sig, clima = clima, core = core
    )

    return(res_list)

  })

  shiny::observeEvent(
    eventExpr = {
      mod_data$inverse_pal
      mod_data$color
      mod_data$mida
      mod_data$tipo_grup_func
      mod_data$grup_func
    },

    handlerExpr = {

      scenario <- input_scenario()
      scenario_data <- data_scenario()

      if (scenario == 'scenario1') {

        # viz_inputs needed
        color_val <- mod_data$color
        mida_val <- mod_data$mida
        tipo_grup_func_val <- mod_data$tipo_grup_func
        grup_func_val <- mod_data$grup_func
        inverse_pal_val <- mod_data$inverse_pal
        admin_div_val <- mod_data$admin_div

        # browser()
        # vars to select
        vars_to_sel <- c(
          color_val, mida_val, 'latitude', 'longitude', 'idparcela',
          glue::glue('{tipo_grup_func_val}_dom_percdens')
        )

        core <- scenario_data[['core']] %>%
          dplyr::select(dplyr::one_of(vars_to_sel))
        sig <- scenario_data[['sig']] %>%
          dplyr::select(dplyr::one_of(vars_to_sel))
        clima <- scenario_data[['clima']] %>%
          dplyr::select(dplyr::one_of(vars_to_sel))
        data_map <- core %>%
          dplyr::left_join(sig, by = 'idparcela') %>%
          dplyr::left_join(clima, by = 'idparcela') %>%
          dplyr::collect()

        # color palette
        if (is.null(color_val) || color_val == '') {
          color_vector <- rep('parcel·la', nrow(data_map))
          pal <- leaflet::colorFactor('viridis', color_vector)
        } else {

          if (grup_func_val != 'Qualsevol') {
            if (is.numeric(data_map[[color_val]])) {
              na <- NA_real_
            } else {
              na <- NA_character_
            }

            data_map <- data_map %>%
              dplyr::mutate(
                !!color_val := dplyr::case_when(
                  !!rlang::sym(glue::glue('{tipo_grup_func_val}dens')) ==
                    grup_func_val ~ !!rlang::sym(color_val),
                  TRUE ~ na
                )
              )
          }

          # We must take into account if the variable is categorical or
          # numerical
          color_vector <- data_map[[color_val]]
          if (is.numeric(color_vector)) {
            pal <- leaflet::colorBin('viridis', color_vector, 9, reverse = inverse_pal_val)
          } else {
            pal <- leaflet::colorFactor('viridis', color_vector, reverse = inverse_pal_val)
          }
        }

        # mida vector
        # size vector
        if (is.null(mida_val) || mida_val == '') {
          mida_vector <- rep(750, nrow(data_map))
        } else {
          # We must take into account if the variable is categorical or
          # numerical
          mida_val_values <- data_map[[mida_val]]

          if (is.numeric(mida_val_values)) {
            mida_vector <- mida_val_values / max(mida_val_values, na.rm = TRUE) * 3000
          } else {
            mida_vector <- as.numeric(as.factor(mida_val_values)) /
              max(as.numeric(as.factor(mida_val_values)), na.rm = TRUE) * 3000
          }
        }

        # update map
        leaflet::leafletProxy('map', data = data_map) %>%
          leaflet::clearGroup('idparcela') %>%
          leaflet::addCircles(
            group = 'idparcela', lng = ~longitude, lat = ~latitude,
            label = ~idparcela, layerId = ~idparcela,
            stroke = FALSE, fillOpacity = 0.4, fillColor = pal(color_vector),
            radius = mida_vector,
            options = leaflet::pathOptions(pane = 'parceles')
          ) %>%
          leaflet::addLegend(
            position = 'topright', pal = pal, values = color_vector,
            title = color_val, layerId = 'color_legend'
          ) %>%
          leaflet::clearGroup('vegueria') %>%
          leaflet::clearGroup('comarca') %>%
          leaflet::clearGroup('municipi') %>%
          leaflet::clearGroup('provincia') %>%
          leaflet::addPolygons(
            data = rlang::eval_tidy(rlang::sym(polygons_dictionary[[admin_div_val]][['polygon']])),
            group = polygons_dictionary[[admin_div_val]][['group']],
            label = polygons_dictionary[[admin_div_val]][['label']],
            layerId = rlang::eval_tidy(rlang::sym(polygons_dictionary[[admin_div_val]][['layerId']])),
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

      if (scenario == 'scenario2') {

        # viz_inputs needed
        color_val <- mod_data$color
        mida_val <- mod_data$mida
        grup_func_val <- mod_data$grup_func
        inverse_pal_val <- mod_data$inverse_pal

        # vars to select
        vars_to_sel <- c(
          color_val, mida_val, 'latitude', 'longitude', 'idparcela',
          glue::glue('id{mod_data$agg_level}')
        )

        core <- scenario_data[['core']] %>%
          dplyr::select(dplyr::one_of(vars_to_sel))
        sig <- scenario_data[['sig']] %>%
          dplyr::select(dplyr::one_of(vars_to_sel))
        clima <- scenario_data[['clima']] %>%
          dplyr::select(dplyr::one_of(vars_to_sel))
        data_map <- core %>%
          dplyr::left_join(sig, by = 'idparcela') %>%
          dplyr::left_join(clima, by = 'idparcela') %>%
          dplyr::collect()


        if (is.numeric(data_map[[color_val]])) {
          na <- NA_real_
        } else {
          na <- NA_character_
        }

        data_map <- data_map %>%
          dplyr::mutate(
            !!color_val := dplyr::case_when(
              !!rlang::sym(glue::glue('id{mod_data$agg_level}')) ==
                grup_func_val ~ !!rlang::sym(color_val),
              TRUE ~ na
            )
          )

        # color palette
        if (is.null(color_val) || color_val == '') {
          color_vector <- rep('parcel·la', nrow(data_map))
          pal <- leaflet::colorFactor('viridis', color_vector)
        } else {

          # We must take into account if the variable is categorical or
          # numerical
          color_vector <- data_map[[color_val]]
          if (is.numeric(color_vector)) {
            pal <- leaflet::colorBin(
              'viridis', color_vector, 9, reverse = inverse_pal_val
            )
          } else {
            pal <- leaflet::colorFactor(
              'viridis', color_vector, reverse = inverse_pal_val
            )
          }
        }

        # mida vector
        # size vector
        if (is.null(mida_val) || mida_val == '') {
          mida_vector <- rep(750, nrow(data_map))
        } else {
          # We must take into account if the variable is categorical or
          # numerical
          mida_val_values <- data_map[[mida_val]]

          if (is.numeric(mida_val_values)) {
            mida_vector <- mida_val_values / max(mida_val_values, na.rm = TRUE) * 3000
          } else {
            mida_vector <- as.numeric(as.factor(mida_val_values)) /
              max(as.numeric(as.factor(mida_val_values)), na.rm = TRUE) * 3000
          }
        }

        # update map
        leaflet::leafletProxy('map', data = data_map) %>%
          leaflet::clearGroup('idparcela') %>%
          leaflet::addCircles(
            group = 'idparcela', lng = ~longitude, lat = ~latitude,
            label = ~idparcela, layerId = ~idparcela,
            stroke = FALSE, fillOpacity = 0.4, fillColor = pal(color_vector),
            radius = mida_vector,
            options = leaflet::pathOptions(pane = 'parceles')
          ) %>%
          leaflet::addLegend(
            position = 'topright', pal = pal, values = color_vector,
            title = color_val, layerId = 'color_legend'
          ) %>%
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

      if (scenario == 'scenario3') {

        # viz_inputs needed
        color_val <- mod_data$color
        tipo_grup_func_val <- mod_data$tipo_grup_func
        grup_func_val <- mod_data$grup_func
        statistic_val <- mod_data$statistic
        inverse_pal_val <- mod_data$inverse_pal
        admin_div_val <- mod_data$admin_div

        if (grup_func_val == 'Qualsevol') {

          data_map <- scenario_data[['sig']] %>%
            dplyr::select(idparcela, !!rlang::sym(admin_div_val)) %>%
            dplyr::inner_join(scenario_data[['core']], by = 'idparcela') %>%
            dplyr::collect() %>%
            tidyIFN::summarise_polygons(polygon_group = admin_div_val)


        } else {

          filter_arg_val <- rlang::quo(
            !!rlang::sym(glue::glue('{tipo_grup_func_val}_dom_percdens')) ==
              !!grup_func_val
          )

          data_map <- scenario_data[['sig']] %>%
            dplyr::select(idparcela, !!rlang::sym(admin_div_val)) %>%
            dplyr::inner_join(scenario_data[['core']], by = 'idparcela') %>%
            dplyr::collect() %>%
            tidyIFN::summarise_polygons(
              polygon_group = admin_div_val, func_group = grup_func_val,
              !!! filter_arg_val
            )
        }

        polygons_label_var <- polygons_dictionary[[admin_div_val]][['label_chr']]
        polygon_data <- rlang::eval_tidy(
          rlang::sym(polygons_dictionary[[admin_div_val]][['polygon']])
        )

        polygon_data@data <- polygon_data@data %>%
          dplyr::rename(!!rlang::sym(admin_div_val) := !!rlang::sym(polygons_label_var)) %>%
          dplyr::left_join(data_map, by = admin_div_val)

        # color palette
        if (is.null(color_val) || color_val == '') {
          color_variable_def <- 'Sense color'
          color_vector <- rep('parcel·la', nrow(polygon_data@data))
          pal <- leaflet::colorFactor('viridis', color_vector)
        } else {

          # We must take into account if the variable is categorical or
          # numerical
          color_variable_def <- paste0(color_val, statistic_val)
          color_vector <- polygon_data@data[[color_variable_def]]

          if (is.numeric(color_vector)) {
            pal <- leaflet::colorBin(
              'viridis', color_vector, 9, reverse = inverse_pal_val
            )
          } else {
            pal <- leaflet::colorFactor(
              'viridis', color_vector, reverse = inverse_pal_val
            )
          }
        }

        if (admin_div_val == '') {
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
            leaflet::clearGroup('idparcela') %>%
            leaflet::addPolygons(
              data = polygon_data,
              group = polygons_dictionary[[admin_div_val]][['group']],
              label = polygons_dictionary[[admin_div_val]][['label_new']],
              layerId = rlang::eval_tidy(rlang::sym(polygons_dictionary[[admin_div_val]][['layerId']])),
              weight = 1, smoothFactor = 1,
              opacity = 1.0, color = '#6C7A89FF',
              fill = TRUE, fillColor = pal(color_vector),
              fillOpacity = 1,
              highlightOptions = leaflet::highlightOptions(
                color = "#CF000F", weight = 2,
                bringToFront = FALSE
              ),
              options = leaflet::pathOptions(
                pane = 'admin_divs'
              )
            ) %>%
            leaflet::addLegend(
              position = 'topright', pal = pal, values = color_vector,
              title = color_variable_def, layerId = 'color_legend', opacity = 1
            )
        }
      }

      if (scenario == 'scenario4') {

        # viz_inputs needed
        color_val <- mod_data$color
        grup_func_val <- mod_data$grup_func
        statistic_val <- mod_data$statistic
        inverse_pal_val <- mod_data$inverse_pal
        admin_div_val <- mod_data$admin_div

        filter_arg <- rlang::quo(
          !!rlang::sym(glue::glue('id{mod_data$agg_level}')) == !!grup_func_val
        )

        data_map <- scenario_data[['sig']] %>%
          dplyr::select(idparcela, !!rlang::sym(admin_div_val)) %>%
          dplyr::inner_join(scenario_data[['core']], by = 'idparcela') %>%
          dplyr::collect() %>%
          tidyIFN::summarise_polygons(
            polygon_group = admin_div_val,
            func_group = glue::glue('id{mod_data$agg_level}'),
            !!! filter_arg_val
          )

        polygons_label_var <- polygons_dictionary[[admin_div_val]][['label_chr']]
        polygon_data <- rlang::eval_tidy(
          rlang::sym(polygons_dictionary[[admin_div_val]][['polygon']])
        )

        polygon_data@data <- polygon_data@data %>%
          dplyr::rename(!!rlang::sym(admin_div_val) := !!rlang::sym(polygons_label_var)) %>%
          dplyr::left_join(data_map, by = admin_div_val)

        # color palette
        if (is.null(color_val) || color_val == '') {
          color_variable_def <- 'Sense color'
          color_vector <- rep('parcel·la', nrow(polygon_data@data))
          pal <- leaflet::colorFactor('viridis', color_vector)
        } else {

          # We must take into account if the variable is categorical or
          # numerical
          color_variable_def <- paste0(color_val, statistic_val)
          color_vector <- polygon_data@data[[color_variable_def]]

          if (is.numeric(color_vector)) {
            pal <- leaflet::colorBin('viridis', color_vector, 9, reverse = inverse_pal_val)
          } else {
            pal <- leaflet::colorFactor('viridis', color_vector, reverse = inverse_pal_val)
          }
        }

        if (admin_div_val == '') {
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
            leaflet::clearGroup('idparcela') %>%
            leaflet::addPolygons(
              data = polygon_data,
              group = polygons_dictionary[[admin_div_val]][['group']],
              label = polygons_dictionary[[admin_div_val]][['label_new']],
              layerId = rlang::eval_tidy(rlang::sym(polygons_dictionary[[admin_div_val]][['layerId']])),
              weight = 1, smoothFactor = 1,
              opacity = 1.0, color = '#6C7A89FF',
              fill = TRUE, fillColor = pal(color_vector),
              fillOpacity = 1,
              highlightOptions = leaflet::highlightOptions(
                color = "#CF000F", weight = 2,
                bringToFront = FALSE
              ),
              options = leaflet::pathOptions(
                pane = 'admin_divs'
              )
            ) %>%
            leaflet::addLegend(
              position = 'topright', pal = pal, values = color_vector,
              title = color_variable_def, layerId = 'color_legend', opacity = 1
            )
        }
      }
    }
  )

  # reactive with the map events
  map_reactives <- reactiveValues()

  observe({
    map_reactives$map_shape_click <- input$map_shape_click
    # map_reactives$shape_mouseover <- input$map_shape_mouseover
    # map_reactives$shape_mouseout <- input$map_shape_mouseout
    # map_reactives$map_click <- input$map_click
    # map_reactives$map_bounds <- input$map_bounds
    # map_reactives$map_zoom <- input$map_zoom
    # map_reactives$map_center <- input$map_center
  })

  return(map_reactives)

}