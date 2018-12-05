## Helper functions

#' Get the current data scenario
#'
#' Return the current scenario as character
#'
#' @param viz_shape The shape to visualize, plots or polygons
#' @param agg_level The breakdown level, plot, species...
#'
#' @export
get_scenario <- function(viz_shape, agg_level) {

  if (viz_shape == 'parcela') {
    if (agg_level == 'parcela') {
      # parcelas y agregado por parcelas
      return('scenario1')
    } else {
      # parcelas y desglosado por tipo funcional
      return('scenario2')
    }
  } else {
    if (agg_level == 'parcela') {
      # poligonos agregados por parcelas
      return('scenario3')
    } else {
      # poligonos desglosados por tipo funcional
      return('scenario4')
    }
  }

}

#' Get a list woth sig, clima and core data
#'
#' sig, clima and core data for the corresponding agg level
#'
#' @param admin_div Input
#' @param admin_div_fil  Input
#' @param espai_tipus  Input
#' @param espai_tipus_fil  Input
#' @param ifn  Input
#' @param ifndb  Database pool object
#' @param agg_level  Input
#' @param diameter_classes  Input
#' @param clima_filters reactive from mod_advancedFilters with the clima filters
#'   quos
#' @param sig_extra_filters reactive from mod_advancedFilters with the extra
#'   sig filters quos
#' @param custom_polygon input from map module to capture the custom polygon
#'   if any
#'
#' @export
data_scenario <- function(
  admin_div,
  admin_div_fil,
  espai_tipus,
  espai_tipus_fil,
  ifn,
  ifndb,
  agg_level,
  diameter_classes,
  clima_filters,
  sig_extra_filters,
  custom_polygon = NULL,
  updateProgress = NULL
) {

  ## SIG data
  # updateProgress setup (only for table generation)
  if (is.function(updateProgress)) {
    updateProgress(
      value = 0.1,
      detail = label_getter(ifndb, 'esp', 'progress_data_scenario_label', 'detail', 'sig')
    )
  }
  # admin_div filter
  if (is.null(admin_div_fil) ||
      any(admin_div_fil == '')) {
    filter_expr_admin <- NULL
  } else {
    filter_expr_admin <- rlang::quo(
      !!rlang::sym(admin_div) %in% !!admin_div_fil
    )
  }

  # espai tipus filter
  if (is.null(espai_tipus_fil) ||
      any(espai_tipus_fil == '')) {
    filter_expr_espai <- NULL
  } else {
    # here we need also to check for nomes protegits and sense proteccio
    # to be able to filter these cases
    if (any(espai_tipus_fil == "only_protected")) {
      filter_expr_espai <- rlang::quo(
        !(!!rlang::sym(espai_tipus) %in% c(
          "Sense Pein", "Sense protecció", "SenseXarxa"
        ))
      )
    } else {
      filter_expr_espai <- rlang::quo(
        !!rlang::sym(espai_tipus) %in% !!espai_tipus_fil
      )
    }
  }

  sig <- tidyIFN::data_sig(
    ifn, ifndb,
    !!! filter_expr_admin, !!! filter_expr_espai, !!! sig_extra_filters
  )

  # is there any custom polygon?? beacuse if it is we need to change sig
  if (!is.null(custom_polygon)) {

    tmp <- custom_polygon[['geometry']][['coordinates']] %>%
      purrr::flatten() %>%
      purrr::set_names(nm = 1:length(.)) %>%
      purrr::modify_depth(1, purrr::set_names, nm = c('long', 'lat')) %>%
      dplyr::bind_rows() %>%
      sp::Polygon()

    tmp <- sp::SpatialPolygons(list(sp::Polygons(list(tmp), 'custom_polygon')))

    sig_collected <- sig %>%
      dplyr::collect()

    # is in poly
    is_in_poly_col <- sig_collected %>%
      dplyr::select(longitude, latitude) %>%
      sp::SpatialPoints() %>%
      sp::over(tmp) %>%
      as.vector()
    is_in_poly_col <- dplyr::case_when(
      is_in_poly_col == 1 ~ TRUE,
        TRUE ~ FALSE
      )

    plots_codes <- sig_collected[['idparcela']][is_in_poly_col]

    sig <- sig %>%
      dplyr::filter(idparcela %in% !! plots_codes)

  }


  ## CLIMA data
  # updateProgress setup (only for table generation)
  if (is.function(updateProgress)) {
    updateProgress(
      value = 0.2,
      detail = label_getter(ifndb, 'esp', 'progress_data_scenario_label', 'detail', 'clima')
    )
  }
  clima <- tidyIFN::data_clima(sig, ifn, ifndb, !!! clima_filters)
  clima_plots <- try(clima %>% dplyr::pull(idparcela))

  # if no clima data because of filters, pull is gonna fail. This way return
  # an empty character vector for clima plots, resulting in an empty core data
  # also, win-win
  if (class(clima_plots) == 'try-error') {
    clima_plots <- ''
  }

  ## CORE data
  # updateProgress setup (only for table generation)
  if (is.function(updateProgress)) {
    updateProgress(
      value = 0.3,
      detail = label_getter(ifndb, 'esp', 'progress_data_scenario_label', 'detail', 'core')
    )
  }
  core <- tidyIFN::data_core(
    sig, ifn, agg_level, ifndb, clima_plots, diameter_classes
  )

  res_list <- list(
    sig = sig, clima = clima, core = core
  )

  return(res_list)

}

#' modifcate the map based on vis inputs
#'
#' this returns the leaflet proxy object to modify the map
#'
#' @param data_scenario tbl_sql object
#' @param scenario character indicating the scenario
#' @param ifn Input
#' @param inverse_pal Input
#' @param color Input
#' @param mida Input
#' @param tipo_grup_func Input
#' @param grup_func Input
#' @param statistic Input
#' @param ifndb pool object to access the ifn db
#' @param updateProgress progress function to monitorize the map drawing
#'
#' @importFrom dplyr left_join
#'
#' @export
map_modificator <- function(
  data_scenario,
  scenario,
  ifn,
  inverse_pal,
  color,
  mida,
  tipo_grup_func,
  grup_func,
  statistic,
  admin_div,
  agg_level,
  ifndb,
  updateProgress = NULL
) {

  # noms division
  nom_cas <- as.character(polygons_catalunya@data$NOM_CA)
  nom_provincies <- as.character(polygons_provincies@data$NOMPROV)
  nom_vegueries <- as.character(polygons_vegueries@data$NOMVEGUE)
  nom_comarques <- as.character(polygons_comarques@data$NOMCOMAR)
  nom_municipis <- as.character(polygons_municipis@data$NOMMUNI)
  nom_delegacions <- as.character(polygons_delegacions@data$comarcas_d)
  nom_comunidades <- as.character(polygons_catalunya@data$NOM_CA)
  # noms proteccions
  nom_enpe <- as.character(polygons_enpe@data$nom)
  nom_pein <- as.character(polygons_pein@data$nom)
  nom_xn2000 <- as.character(polygons_xn2000@data$nom_n2)

  # scenario 1, plots no breakdown
  if (scenario == 'scenario1') {

    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.33,
        detail = label_getter(ifndb, 'esp', 'progress_map_modificator_label', 'detail', 'first')
      )
    }

    # viz_inputs needed
    color_val <- color
    mida_val <- mida
    tipo_grup_func_val <- tipo_grup_func
    grup_func_val <- grup_func
    inverse_pal_val <- inverse_pal
    admin_div_val <- admin_div



    # vars to select
    vars_to_sel <- c(
      color_val, mida_val, 'latitude', 'longitude', 'idparcela',
      glue::glue('{tipo_grup_func_val}_dom_percdens')
    )

    core <- data_scenario[['core']] %>%
      dplyr::select(dplyr::one_of(vars_to_sel))
    sig <- data_scenario[['sig']] %>%
      dplyr::select(dplyr::one_of(vars_to_sel))
    clima <- data_scenario[['clima']] %>%
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

      if (grup_func_val != '') {
        if (is.double(data_map[[color_val]])) {
          na <- NA_real_
        } else {
          if (is.integer(data_map[[color_val]])) {
            na <- NA_integer_
          } else {
            na <- NA_character_
          }
        }

        data_map <- data_map %>%
          dplyr::mutate(
            !!color_val := dplyr::case_when(
              !!rlang::sym(glue::glue('{tipo_grup_func_val}_dom_percdens')) ==
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

    mida_vector[is.na(color_vector)] <- 250

    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.72,
        detail = label_getter(ifndb, 'esp', 'progress_map_modificator_label', 'detail', 'second')
      )
    }

    # update map
    return({
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
        leaflet::clearGroup('delegacio') %>%
        leaflet::clearGroup('comunidad') %>%
        leaflet::addPolygons(
          data = rlang::eval_tidy(
            rlang::sym(polygons_dictionary[[admin_div_val]][['polygon']])
          ),
          group = polygons_dictionary[[admin_div_val]][['group']],
          label = polygons_dictionary[[admin_div_val]][['label']],
          layerId = rlang::eval_tidy(
            rlang::sym(polygons_dictionary[[admin_div_val]][['layerId']])
          ),
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
    })

  }

  if (scenario == 'scenario2') {

    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.33,
        detail = label_getter(ifndb, 'esp', 'progress_map_modificator_label', 'detail', 'first')
      )
    }

    # viz_inputs needed
    color_val <- color
    mida_val <- mida
    grup_func_val <- grup_func
    inverse_pal_val <- inverse_pal
    admin_div_val <- admin_div

    if (grup_func_val == '') {
      return()
    }

    # vars to select
    vars_to_sel <- c(
      color_val, mida_val, 'latitude', 'longitude', 'idparcela',
      glue::glue('id{agg_level}')
    )

    core <- data_scenario[['core']] %>%
      dplyr::select(dplyr::one_of(vars_to_sel))
    sig <- data_scenario[['sig']] %>%
      dplyr::select(dplyr::one_of(vars_to_sel))
    clima <- data_scenario[['clima']] %>%
      dplyr::select(dplyr::one_of(vars_to_sel))
    data_map <- core %>%
      dplyr::left_join(sig, by = 'idparcela') %>%
      dplyr::left_join(clima, by = 'idparcela') %>%
      dplyr::collect()


    if (is.double(data_map[[color_val]])) {
      na <- NA_real_
    } else {
      if (is.integer(data_map[[color_val]])) {
        na <- NA_integer_
      } else {
        na <- NA_character_
      }
    }

    # debug
    # browser()

    data_map <- data_map %>%
      dplyr::mutate(
        !!color_val := dplyr::case_when(
          !!rlang::sym(glue::glue('id{agg_level}')) ==
            grup_func_val ~ !!rlang::sym(color_val),
          TRUE ~ na
        )
      ) %>%
      dplyr::arrange(!!rlang::sym(color_val)) %>%
      dplyr::group_by(idparcela) %>%
      dplyr::slice(1)

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

    mida_vector[is.na(color_vector)] <- 250

    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.72,
        detail = label_getter(ifndb, 'esp', 'progress_map_modificator_label', 'detail', 'second')
      )
    }

    # update map
    return({
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
        leaflet::clearGroup('delegacio') %>%
        leaflet::clearGroup('comunidad') %>%
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
    })
  }

  if (scenario == 'scenario3') {

    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.33,
        detail = label_getter(ifndb, 'esp', 'progress_map_modificator_label', 'detail', 'first')
      )
    }

    # viz_inputs needed
    color_val <- color
    tipo_grup_func_val <- tipo_grup_func
    grup_func_val <- grup_func
    statistic_val <- statistic
    inverse_pal_val <- inverse_pal
    admin_div_val <- admin_div

    if (grup_func_val == '') {

      data_map <- data_scenario %>%
        purrr::reduce(left_join) %>%
        dplyr::collect() %>%
        tidyIFN::summarise_polygons(
          polygon_group = admin_div_val,
          .funs = dplyr::funs(
            mean = mean(., na.rm = TRUE),
            sd = stats::sd(., na.rm = TRUE),
            # min = min(., na.rm = TRUE),
            # max = max(., na.rm = TRUE),
            median = stats::median(., na.rm = TRUE),
            # mad = stats::mad(., na.rm = TRUE),
            # q95 = stats::quantile(., probs = 0.95, na.rm = TRUE),
            n = dplyr::n()
          )
        )


    } else {

      filter_arg_val <- rlang::quo(
        !!rlang::sym(glue::glue('{tipo_grup_func_val}_dom_percdens')) ==
          !!grup_func_val
      )

      data_map <- data_scenario %>%
        purrr::reduce(left_join) %>%
        dplyr::collect() %>%
        tidyIFN::summarise_polygons(
          filter_arg_val,
          polygon_group = admin_div_val,
          func_group = glue::glue('{tipo_grup_func_val}_dom_percdens'),
          .funs = dplyr::funs(
            mean = mean(., na.rm = TRUE),
            sd = stats::sd(., na.rm = TRUE),
            # min = min(., na.rm = TRUE),
            # max = max(., na.rm = TRUE),
            median = stats::median(., na.rm = TRUE),
            # mad = stats::mad(., na.rm = TRUE),
            # q95 = stats::quantile(., probs = 0.95, na.rm = TRUE),
            n = dplyr::n()
          )
        )
    }

    polygons_label_var <- polygons_dictionary[[admin_div_val]][['label_chr']]
    polygon_data <- rlang::eval_tidy(
      rlang::sym(polygons_dictionary[[admin_div_val]][['polygon']])
    )

    polygon_data@data <- polygon_data@data %>%
      dplyr::rename(
        !!rlang::sym(admin_div_val) := !!rlang::sym(polygons_label_var)
      ) %>%
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

    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.72,
        detail = label_getter(ifndb, 'esp', 'progress_map_modificator_label', 'detail', 'second')
      )
    }

    if (admin_div_val == '') {
      return({
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('vegueria') %>%
          leaflet::clearGroup('comarca') %>%
          leaflet::clearGroup('municipi') %>%
          leaflet::clearGroup('delegacio') %>%
          leaflet::clearGroup('comunidad') %>%
          leaflet::clearGroup('provincia')
      })
    } else {
      return({
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('vegueria') %>%
          leaflet::clearGroup('comarca') %>%
          leaflet::clearGroup('municipi') %>%
          leaflet::clearGroup('provincia') %>%
          leaflet::clearGroup('delegacio') %>%
          leaflet::clearGroup('idparcela') %>%
          leaflet::clearGroup('comunidad') %>%
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
      })
    }
  }

  if (scenario == 'scenario4') {

    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.33,
        detail = label_getter(ifndb, 'esp', 'progress_map_modificator_label', 'detail', 'first')
      )
    }

    # viz_inputs needed
    color_val <- color
    grup_func_val <- grup_func
    statistic_val <- statistic
    inverse_pal_val <- inverse_pal
    admin_div_val <- admin_div

    if (grup_func_val == '') {
      return()
    }

    # debug
    # browser()

    filter_arg_val <- rlang::quo(
      !!rlang::sym(glue::glue('id{agg_level}')) == !!grup_func_val
    )

    data_map <- data_scenario %>%
      purrr::reduce(left_join) %>%
      dplyr::collect() %>%
      tidyIFN::summarise_polygons(
        filter_arg_val,
        polygon_group = admin_div_val,
        func_group = glue::glue('id{agg_level}'),
        .funs = dplyr::funs(
          mean = mean(., na.rm = TRUE),
          sd = stats::sd(., na.rm = TRUE),
          # min = min(., na.rm = TRUE),
          # max = max(., na.rm = TRUE),
          median = stats::median(., na.rm = TRUE),
          # mad = stats::mad(., na.rm = TRUE),
          # q95 = stats::quantile(., probs = 0.95, na.rm = TRUE),
          n = dplyr::n()
        )
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

    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.72,
        detail = label_getter(ifndb, 'esp', 'progress_map_modificator_label', 'detail', 'second')
      )
    }

    if (admin_div_val == '') {
      return({
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('vegueria') %>%
          leaflet::clearGroup('comarca') %>%
          leaflet::clearGroup('municipi') %>%
          leaflet::clearGroup('delegacio') %>%
          leaflet::clearGroup('comunidad') %>%
          leaflet::clearGroup('provincia')
      })
    } else {
      return({
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('vegueria') %>%
          leaflet::clearGroup('comarca') %>%
          leaflet::clearGroup('municipi') %>%
          leaflet::clearGroup('provincia') %>%
          leaflet::clearGroup('delegacio') %>%
          leaflet::clearGroup('idparcela') %>%
          leaflet::clearGroup('comunidad') %>%
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
      })
    }
  }

}

#' genreate the plot for the info panel
#'
#' @param data reactive data
#' @param click input
#' @param color input
#' @param viz_shape input
#' @param agg_level input
#' @param diameter_classes input
#' @param tipo_grup_func input
#' @param ifndb pool object to access the ifn db
#'
#' @export
infopanel_plot_gen <- function(
  data, click, color, viz_shape, agg_level, diameter_classes, tipo_grup_func, ifndb
) {


  scenario <- get_scenario(viz_shape, agg_level)

  # scenarios with diameter classes on
  if (isTRUE(diameter_classes)) {

    # and click in plot not polygon
    if (click$group == 'idparcela') {

      if (scenario == 'scenario1') {

        x_var_plot <- rlang::quo(!!rlang::sym('idcd'))
        y_var_plot <- rlang::quo(!!rlang::sym(color))
        fill_var_plot <- x_var_plot
        y_var_index <- as.character(rlang::get_expr(y_var_plot))

        y_lab_plot <- dplyr::tbl(ifndb, 'infopanel_variables_thesaurus') %>%
          dplyr::filter(infopanel_variable_id == !!y_var_index) %>%
          dplyr::collect() %>%
          magrittr::extract2('esp')
        title_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'cd', scenario_id == scenario, shape_id == 'plot', label_id == 'title') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )
        subtitle_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'cd', scenario_id == scenario, shape_id == 'plot', label_id == 'subtitle') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )

        infopanel_plot <- data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = !! x_var_plot, y = !! y_var_plot, fill = !! fill_var_plot
            )
          ) +
          ggplot2::geom_col() +
          ggplot2::labs(
            title = title_plot, subtitle = subtitle_plot,
            x = '', y = y_lab_plot
          )
      }

      if (scenario == 'scenario2') {

        x_var_plot <- rlang::quo(!!rlang::sym(glue::glue('id{agg_level}')))
        y_var_plot <- rlang::quo(!!rlang::sym(color))
        fill_var_plot <- rlang::quo(!!rlang::sym('idcd'))
        y_var_index <- as.character(rlang::get_expr(y_var_plot))
        y_lab_plot <- dplyr::tbl(ifndb, 'infopanel_variables_thesaurus') %>%
          dplyr::filter(infopanel_variable_id == !!y_var_index) %>%
          dplyr::collect() %>%
          magrittr::extract2('esp')
        title_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'cd', scenario_id == scenario, shape_id == 'plot', label_id == 'title') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )
        subtitle_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'cd', scenario_id == scenario, shape_id == 'plot', label_id == 'subtitle') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )

        infopanel_plot <- data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = !! x_var_plot, y = !! y_var_plot,
              fill = !! fill_var_plot
            )
          ) +
          ggplot2::geom_col(position = 'dodge') +
          ggplot2::labs(
            title = title_plot, subtitle = subtitle_plot,
            x = '', y = y_lab_plot
          )
      }

    } else {

      # click in polygon not in plot
      if (scenario %in% c('scenario1', 'scenario3')) {

        x_var_plot <- rlang::quo(!!rlang::sym('idcd'))
        y_var_plot <- rlang::quo(!!rlang::sym(color))
        fill_var_plot <- x_var_plot
        y_var_index <- as.character(rlang::get_expr(y_var_plot))
        y_lab_plot <- dplyr::tbl(ifndb, 'infopanel_variables_thesaurus') %>%
          dplyr::filter(infopanel_variable_id == !!y_var_index) %>%
          dplyr::collect() %>%
          magrittr::extract2('esp')
        title_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'cd', scenario_id == scenario, shape_id == 'polygon', label_id == 'title') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )
        subtitle_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'cd', scenario_id == scenario, shape_id == 'polygon', label_id == 'subtitle') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )

        infopanel_plot <- data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = !! x_var_plot, y = !! y_var_plot, fill = !! fill_var_plot
            )
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::labs(
            title = title_plot, subtitle = subtitle_plot,
            x = '', y = y_lab_plot
          )
      }

      if (scenario %in% c('scenario2', 'scenario4')) {

        x_var_plot <- rlang::quo(!!rlang::sym(glue::glue('id{agg_level}')))
        y_var_plot <- rlang::quo(!!rlang::sym(color))
        fill_var_plot <- rlang::quo(!!rlang::sym('idcd'))
        y_var_index <- as.character(rlang::get_expr(y_var_plot))
        y_lab_plot <- dplyr::tbl(ifndb, 'infopanel_variables_thesaurus') %>%
          dplyr::filter(infopanel_variable_id == !!y_var_index) %>%
          dplyr::collect() %>%
          magrittr::extract2('esp')
        title_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'cd', scenario_id == scenario, shape_id == 'polygon', label_id == 'title') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )
        subtitle_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'cd', scenario_id == scenario, shape_id == 'polygon', label_id == 'subtitle') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )

        infopanel_plot <- data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = !! x_var_plot, y = !! y_var_plot, fill = !! fill_var_plot
            )
          ) +
          ggplot2::geom_boxplot(position = 'dodge') +
          ggplot2::labs(
            title = title_plot, subtitle = subtitle_plot,
            x = '', y = y_lab_plot
          )
      }
    }
  } else {

    # scenarios with diameter classes off
    if (click$group == 'idparcela') {

      if (scenario == 'scenario1') {

        x_var_plot <- rlang::quo(
          !!rlang::sym(glue::glue('{tipo_grup_func}_dom_percdens'))
        )
        fill_var_plot <- x_var_plot
        y_var_plot <- rlang::quo(!!rlang::sym(color))
        y_var_index <- as.character(rlang::get_expr(y_var_plot))
        y_lab_plot <- dplyr::tbl(ifndb, 'infopanel_variables_thesaurus') %>%
          dplyr::filter(infopanel_variable_id == !!y_var_index) %>%
          dplyr::collect() %>%
          magrittr::extract2('esp')
        title_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'nocd', scenario_id == scenario, shape_id == 'plot', label_id == 'title') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )
        subtitle_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'nocd', scenario_id == scenario, shape_id == 'plot', label_id == 'subtitle') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )

        infopanel_plot <- data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = !! x_var_plot, y = !! y_var_plot, fill = !! fill_var_plot
            )
          ) +
          ggplot2::geom_col() +
          ggplot2::labs(
            title = title_plot, subtitle = subtitle_plot,
            x = '', y = y_lab_plot
          )
      }

      if (scenario == 'scenario2') {

        x_var_plot <- rlang::quo(!!rlang::sym(glue::glue('id{agg_level}')))
        fill_var_plot <- x_var_plot
        y_var_plot <- rlang::quo(!!rlang::sym(color))
        y_var_index <- as.character(rlang::get_expr(y_var_plot))
        y_lab_plot <- dplyr::tbl(ifndb, 'infopanel_variables_thesaurus') %>%
          dplyr::filter(infopanel_variable_id == !!y_var_index) %>%
          dplyr::collect() %>%
          magrittr::extract2('esp')
        title_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'nocd', scenario_id == scenario, shape_id == 'plot', label_id == 'title') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )
        subtitle_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'nocd', scenario_id == scenario, shape_id == 'plot', label_id == 'subtitle') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )

        infopanel_plot <- data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = !! x_var_plot, y = !! y_var_plot, fill = !! fill_var_plot
            )
          ) +
          ggplot2::geom_col() +
          ggplot2::labs(
            title = title_plot, subtitle = subtitle_plot,
            x = '', y = y_lab_plot
          )
      }
    } else {
      if (scenario %in% c('scenario1', 'scenario3')) {

        x_var_plot <- rlang::quo(
          !!rlang::sym(glue::glue('{tipo_grup_func}_dom_percdens'))
        )
        fill_var_plot <- x_var_plot
        y_var_plot <- rlang::quo(!!rlang::sym(color))
        y_var_index <- as.character(rlang::get_expr(y_var_plot))
        y_lab_plot <- dplyr::tbl(ifndb, 'infopanel_variables_thesaurus') %>%
          dplyr::filter(infopanel_variable_id == !!y_var_index) %>%
          dplyr::collect() %>%
          magrittr::extract2('esp')
        title_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'nocd', scenario_id == scenario, shape_id == 'polygon', label_id == 'title') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )
        subtitle_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'nocd', scenario_id == scenario, shape_id == 'polygon', label_id == 'subtitle') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )

        infopanel_plot <- data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = !! x_var_plot, y = !! y_var_plot, fill = !! fill_var_plot
            )
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::labs(
            title = title_plot, subtitle = subtitle_plot,
            x = '', y = y_lab_plot
          )
      }

      if (scenario %in% c('scenario2', 'scenario4')) {

        x_var_plot <- rlang::quo(!!rlang::sym(glue::glue('id{agg_level}')))
        fill_var_plot <- x_var_plot
        y_var_plot <- rlang::quo(!!rlang::sym(color))
        y_var_index <- as.character(rlang::get_expr(y_var_plot))
        y_lab_plot <- dplyr::tbl(ifndb, 'infopanel_variables_thesaurus') %>%
          dplyr::filter(infopanel_variable_id == !!y_var_index) %>%
          dplyr::collect() %>%
          magrittr::extract2('esp')
        title_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'nocd', scenario_id == scenario, shape_id == 'polygon', label_id == 'title') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )
        subtitle_plot <- glue::glue(
          dplyr::tbl(ifndb, 'infopanel_plot_thesaurus') %>%
            dplyr::filter(cd_id == 'nocd', scenario_id == scenario, shape_id == 'polygon', label_id == 'subtitle') %>%
            dplyr::collect() %>%
            magrittr::extract2('esp')
        )

        infopanel_plot <- data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = !! x_var_plot, y = !! y_var_plot, fill = !! fill_var_plot
            )
          ) +
          ggplot2::geom_boxplot() +
          ggplot2::labs(
            title = title_plot, subtitle = subtitle_plot,
            x = '', y = y_lab_plot
          )
      }
    }
  }

  return(infopanel_plot)

}

#' generate the plot for the info panel, in climatic mode
#'
#' @param data list with clima data, filtered and no filtered (for plots)
#' @param color input
#' @param click input
#'
#' @export
infopanel_climaplot_gen <- function(data, color, click) {

  if (click$group == 'idparcela') {

    data_plot <- data[['clima_nf']] %>%
      dplyr::collect()

    point_data <- data_plot %>%
      dplyr::filter(idparcela == click$id) %>%
      dplyr::select(idparcela, !!rlang::sym(color))
    x_var_plot <- 'plot'
    y_var_plot <- rlang::quo(!!rlang::sym(color))

    infopanel_plot <- data_plot %>%
      ggplot2::ggplot(ggplot2::aes(x = x_var_plot, y = !!y_var_plot)) +
      ggplot2::geom_violin() +
      ggplot2::geom_point(
        ggplot2::aes(x = x_var_plot, y = !!y_var_plot), data = point_data,
        color = 'red', size = 5, alpha = 0.8
      )
  } else {

    data_plot <- data[['clima']] %>%
      dplyr::collect()
    y_var_plot <- rlang::quo(!!rlang::sym(color))

    infopanel_plot <- data_plot %>%
      ggplot2::ggplot(ggplot2::aes(y = !!y_var_plot)) +
      ggplot2::geom_boxplot()
  }

  return(infopanel_plot)
}

# Table column visibility selection modal dialog
#
# @param failed Logical
# @param ns session ns
# @param dictionary list
#
# @export
# col_vis_modal <- function(failed = FALSE, ns, dictionary) {
#
#   shiny::modalDialog(
#
#     # inputs
#     shiny::selectInput(
#       ns('col_vis_input'), 'Select the variables to show',
#       choices = dictionary, multiple = TRUE
#     ),
#
#     # check if failed
#     if (failed) {
#       shiny::div(
#         shiny::tags$b("You must select at least one column", style = "color: red;")
#       )
#     },
#
#     # footer
#     footer = shiny::tagList(
#       shiny::modalButton('Cancel'),
#       shiny::actionButton(ns('col_vis_apply'), 'Apply'),
#       shiny::actionButton(ns('col_vis_reset'), 'Res')
#     )
#   )
#
# }

#' modifcate the table based on data inputs
#'
#' this returns the data object to modify the table
#'
#' @param data_scenario tbl_sql object
#' @param scenario character indicating the scenario
#' @param admin_div Input
#' @param agg_level Input
#' @param ifndb pool object to access the ifn db
#' @param diameter_classes Input
#'
#' @export
table_data_modificator <- function(
  data_scenario,
  scenario,
  admin_div,
  agg_level,
  diameter_classes,
  ifndb,
  updateProgress = NULL
) {

  # scenario 1, plots no breakdown
  if (scenario == 'scenario1') {
    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.53,
        detail = label_getter(ifndb, 'esp', 'progress_table_data_modificator', 'detail', 'first')
      )
    }
    return(data_scenario %>% purrr::reduce(left_join) %>% dplyr::collect())
  }

  if (scenario == 'scenario2') {
    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.53,
        detail = label_getter(ifndb, 'esp', 'progress_table_data_modificator', 'detail', 'first')
      )
    }
    return(data_scenario %>% purrr::reduce(left_join) %>% dplyr::collect())
  }

  if (scenario == 'scenario3') {
    # inputs needed
    admin_div_val <- admin_div

    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.53,
        detail = label_getter(ifndb, 'esp', 'progress_table_data_modificator', 'detail', 'first')
      )
    }
    return(
      data_scenario %>%
        purrr::reduce(left_join) %>%
        dplyr::collect() %>%
        tidyIFN::summarise_polygons(
          polygon_group = admin_div_val, cd = diameter_classes,
          .funs = dplyr::funs(
            mean = mean(., na.rm = TRUE),
            sd = stats::sd(., na.rm = TRUE),
            # min = min(., na.rm = TRUE),
            # max = max(., na.rm = TRUE),
            median = stats::median(., na.rm = TRUE),
            # mad = stats::mad(., na.rm = TRUE),
            # q95 = stats::quantile(., probs = 0.95, na.rm = TRUE),
            n = dplyr::n()
          )
        )
    )
  }

  if (scenario == 'scenario4') {
    # inputs needed
    admin_div_val <- admin_div
    agg_level_val <- glue::glue('id{agg_level}')

    # updateProgress setup
    if (is.function(updateProgress)) {
      updateProgress(
        value = 0.53,
        detail = label_getter(ifndb, 'esp', 'progress_table_data_modificator', 'detail', 'first')
      )
    }
    return(
      data_scenario %>%
        purrr::reduce(left_join) %>%
        dplyr::collect() %>%
        tidyIFN::summarise_polygons(
          polygon_group = admin_div_val, func_group = agg_level_val,
          cd = diameter_classes,
          .funs = dplyr::funs(
            mean = mean(., na.rm = TRUE),
            sd = stats::sd(., na.rm = TRUE),
            # min = min(., na.rm = TRUE),
            # max = max(., na.rm = TRUE),
            median = stats::median(., na.rm = TRUE),
            # mad = stats::mad(., na.rm = TRUE),
            # q95 = stats::quantile(., probs = 0.95, na.rm = TRUE),
            n = dplyr::n()
          )
        )
    )
  }
}

#' label getter
#'
#' @param ifndb pool
#' @param lang character
#' @param label_id character
#' @param label_sub1 character
#' @param label_sub2 character
#'
label_getter <- function(
  ifndb, lang = 'esp', label_id, label_sub1 = NULL, label_sub2 = NULL
) {

  label_id_fil <- rlang::quo(label_id == !!label_id)

  if (is.null(label_sub1)) {
    label_sub1_fil <- rlang::quos()
  } else {
    label_sub1_fil <- rlang::quo(label_sub1 == !!label_sub1)
  }

  if (is.null(label_sub2)) {
    label_sub2_fil <- rlang::quos()
  } else {
    label_sub2_fil <- rlang::quo(label_sub2 == !!label_sub2)
  }

  dplyr::tbl(ifndb, 'label_thesaurus') %>%
    dplyr::filter(
      !!! label_id_fil,
      !!! label_sub1_fil, !!! label_sub2_fil
    ) %>%
    dplyr::collect() %>%
    magrittr::extract2(lang)
}

#' query_builder
#'
#' @param mod_data reactives from data module
#' @param col_filter_expressions filter expressions for table module
query_builder <- function(
  mod_data, col_filter_expressions
) {

  ifn <- mod_data$ifn
  admin_div <- mod_data$admin_div
  espai_tipus <- mod_data$espai_tipus
  admin_div_fil <- mod_data$admin_div_fil
  espai_tipus_fil <- mod_data$espai_tipus_fil
  agg_level <- mod_data$agg_level
  diameter_classes <- mod_data$diameter_classes
  viz_shape <- mod_data$viz_shape
  adv_fil_clima_expressions <- mod_data$advancedFilters_reactives$adv_fil_clima_expressions()
  adv_fil_sig_expressions <- mod_data$advancedFilters_reactives$adv_fil_sig_expressions()


  admin_div_fil_vec <- glue::glue("{admin_div} in {glue::collapse(admin_div_fil, sep = ', ', last = ' and ')}")

  espai_tipus_fil_vec <- glue::glue("{espai_tipus} in {glue::collapse(espai_tipus_fil, sep = ', ', last = ' and ')}")

  adv_fil_clima_expressions_vec <- vapply(
    adv_fil_clima_expressions,
    function(x) {
      as.character(rlang::get_expr(x))[c(2,1,3,4)] %>%
        glue::collapse(sep = ' ', last = ' and ')
    },
    character(1)
  )

  adv_fil_sig_expressions_vec <- vapply(
    adv_fil_sig_expressions,
    function(x) {
      as.character(rlang::get_expr(x))[c(2,1,3,4)] %>%
        glue::collapse(sep = ' ', last = ' and ')
    },
    character(1)
  )

  col_filter_expressions_vec <- vapply(
    col_filter_expressions(),
    function(x) {
      tmp_expr <- as.character(rlang::get_expr(x))
      if (length(tmp_expr) < 4) {
        tmp_expr <- tmp_expr[c(2,1,3)] %>%
          glue::collapse(sep = ' ')
      } else {
        tmp_expr <- tmp_expr[c(2,1,3,4)] %>%
          glue::collapse(sep = ' ', last = ' and ')
      }
    },
    character(1)
  )

  filters_txt <- c(
    admin_div_fil_vec, espai_tipus_fil_vec, adv_fil_clima_expressions_vec,
    adv_fil_sig_expressions_vec, col_filter_expressions_vec
  ) %>% purrr::discard(is.na)

  shiny::tagList(
    shiny::h3('Query'),
    shiny::h5(glue::glue('Datos usados: {ifn}')),
    shiny::h5(glue::glue('Nivel de agregación: {agg_level}')),
    shiny::h5(glue::glue('Clases diamétricas: {diameter_classes}')),
    shiny::h5(glue::glue('Parcelas o polígonos?: {viz_shape}')),
    shiny::h5(glue::glue("Filtros usados: ")),
    lapply(filters_txt, function(x) {
      shiny::p(x)
    })
  )

}

#' nav bar with inputs
#'
#' from daattali in
#' https://github.com/daattali/advanced-shiny/tree/master/navbar-add-text
#'
#' @param ... navbarpage arguments
#' @param inputs inputs objects
#'
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)

  browser()
  form <- shiny::tags$form(class = 'navbar-form', inputs)
  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}