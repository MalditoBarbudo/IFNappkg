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
  diameter_classes
) {

  ## SIG data
  # admin_div filter
  if (is.null(admin_div_fil)) {
    filter_expr_admin <- rlang::quo(TRUE)
  } else {
    filter_expr_admin <- rlang::quo(
      !!rlang::sym(admin_div) %in% !!admin_div_fil
    )
  }

  # espai tipus filter
  if (is.null(espai_tipus_fil) ||
      any(espai_tipus_fil == '')) {
    filter_expr_espai <- rlang::quo(TRUE)
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

  sig_filters <- list(filter_expr_admin, filter_expr_espai)

  sig <- tidyIFN::data_sig(ifn, ifndb, !!! sig_filters)

  ## CLIMA data
  clima_filters <- dplyr::quo(TRUE)
  clima <- tidyIFN::data_clima(sig, ifn, ifndb, !!! clima_filters)
  clima_plots <- clima %>% dplyr::pull(idparcela)

  ## CORE data

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
  agg_level
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

  # scenario 1, plots no breakdown
  if (scenario == 'scenario1') {

    # viz_inputs needed
    color_val <- color
    mida_val <- mida
    tipo_grup_func_val <- tipo_grup_func
    grup_func_val <- grup_func
    inverse_pal_val <- inverse_pal
    admin_div_val <- admin_div

    # debug
    # browser()

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

    # viz_inputs needed
    color_val <- color
    tipo_grup_func_val <- tipo_grup_func
    grup_func_val <- grup_func
    statistic_val <- statistic
    inverse_pal_val <- inverse_pal
    admin_div_val <- admin_div

    if (grup_func_val == '') {

      data_map <- data_scenario[['core']] %>%
        dplyr::collect() %>%
        tidyIFN::summarise_polygons(polygon_group = admin_div_val)


    } else {

      filter_arg_val <- rlang::quo(
        !!rlang::sym(glue::glue('{tipo_grup_func_val}_dom_percdens')) ==
          !!grup_func_val
      )

      data_map <- data_scenario[['core']] %>%
        dplyr::collect() %>%
        tidyIFN::summarise_polygons(
          filter_arg_val,
          polygon_group = admin_div_val,
          func_group = glue::glue('{tipo_grup_func_val}_dom_percdens')
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

    if (admin_div_val == '') {
      return({
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('vegueria') %>%
          leaflet::clearGroup('comarca') %>%
          leaflet::clearGroup('municipi') %>%
          leaflet::clearGroup('provincia')
      })
    } else {
      return({
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
      })
    }
  }

  if (scenario == 'scenario4') {

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

    data_map <- data_scenario[['core']] %>%
      dplyr::collect() %>%
      tidyIFN::summarise_polygons(
        filter_arg_val,
        polygon_group = admin_div_val,
        func_group = glue::glue('id{agg_level}')
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
      return({
        leaflet::leafletProxy('map') %>%
          leaflet::clearGroup('vegueria') %>%
          leaflet::clearGroup('comarca') %>%
          leaflet::clearGroup('municipi') %>%
          leaflet::clearGroup('provincia')
      })
    } else {
      return({
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
#'
#' @export
infopanel_plot_gen <- function(
  data, click, color, viz_shape, agg_level, diameter_classes, tipo_grup_func
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
        y_lab_plot <- label_infopanel_variables[['esp']][[y_var_index]]
        title_plot <- glue::glue(
          label_infopanel_plot[['esp']][['cd']][[scenario]][['plot']][['title']]
        )
        subtitle_plot <- glue::glue(
          label_infopanel_plot[['esp']][['cd']][[scenario]][['plot']][['subtitle']]
        )

        infopanel_plot <- data() %>%
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
        y_lab_plot <- label_infopanel_variables[['esp']][[y_var_index]]
        title_plot <- glue::glue(
          label_infopanel_plot[['esp']][['cd']][[scenario]][['plot']][['title']]
        )
        subtitle_plot <- glue::glue(
          label_infopanel_plot[['esp']][['cd']][[scenario]][['plot']][['subtitle']]
        )

        infopanel_plot <- data() %>%
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
        y_lab_plot <- label_infopanel_variables[['esp']][[y_var_index]]
        title_plot <- glue::glue(
          label_infopanel_plot[['esp']][['cd']][[scenario]][['polygon']][['title']]
        )
        subtitle_plot <- glue::glue(
          label_infopanel_plot[['esp']][['cd']][[scenario]][['polygon']][['subtitle']]
        )

        infopanel_plot <- data() %>%
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
        y_lab_plot <- label_infopanel_variables[['esp']][[y_var_index]]
        title_plot <- glue::glue(
          label_infopanel_plot[['esp']][['cd']][[scenario]][['polygon']][['title']]
        )
        subtitle_plot <- glue::glue(
          label_infopanel_plot[['esp']][['cd']][[scenario]][['polygon']][['subtitle']]
        )

        infopanel_plot <- data() %>%
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
        y_lab_plot <- label_infopanel_variables[['esp']][[y_var_index]]
        title_plot <- glue::glue(
          label_infopanel_plot[['esp']][['nocd']][[scenario]][['plot']][['title']]
        )
        subtitle_plot <- glue::glue(
          label_infopanel_plot[['esp']][['nocd']][[scenario]][['plot']][['subtitle']]
        )

        infopanel_plot <- data() %>%
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
        y_lab_plot <- label_infopanel_variables[['esp']][[y_var_index]]
        title_plot <- glue::glue(
          label_infopanel_plot[['esp']][['nocd']][[scenario]][['plot']][['title']]
        )
        subtitle_plot <- glue::glue(
          label_infopanel_plot[['esp']][['nocd']][[scenario]][['plot']][['subtitle']]
        )

        infopanel_plot <- data() %>%
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
        y_lab_plot <- label_infopanel_variables[['esp']][[y_var_index]]
        title_plot <- glue::glue(
          label_infopanel_plot[['esp']][['nocd']][[scenario]][['polygon']][['title']]
        )
        subtitle_plot <- glue::glue(
          label_infopanel_plot[['esp']][['nocd']][[scenario]][['polygon']][['subtitle']]
        )

        infopanel_plot <- data() %>%
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
        y_lab_plot <- label_infopanel_variables[['esp']][[y_var_index]]
        title_plot <- glue::glue(
          label_infopanel_plot[['esp']][['nocd']][[scenario]][['polygon']][['title']]
        )
        subtitle_plot <- glue::glue(
          label_infopanel_plot[['esp']][['nocd']][[scenario]][['polygon']][['subtitle']]
        )

        infopanel_plot <- data() %>%
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

#' Table column visibility selection modal dialog
#'
#' @param failed Logical
#' @param ns session ns
#' @param dictionary list
#'
#' @export
col_vis_modal <- function(failed = FALSE, ns, dictionary) {

  shiny::modalDialog(

    # inputs
    shiny::selectInput(
      ns('col_vis_input'), 'Select the variables to show',
      choices = dictionary, multiple = TRUE
    ),

    # check if failed
    if (failed) {
      shiny::div(
        shiny::tags$b("You must select at least one column", style = "color: red;")
      )
    },

    # footer
    footer = shiny::tagList(
      shiny::modalButton('Cancel'),
      shiny::actionButton(ns('col_vis_apply'), 'Apply')
    )
  )

}

#' modifcate the table based on data inputs
#'
#' this returns the data object to modify the table
#'
#' @param data_scenario tbl_sql object
#' @param scenario character indicating the scenario
#' @param admin_div Input
#' @param agg_level Input
#'
#' @export
table_data_modificator <- function(
  data_scenario,
  scenario,
  admin_div,
  agg_level
) {

  # scenario 1, plots no breakdown
  if (scenario == 'scenario1') {
    return(data_scenario[['core']] %>% dplyr::collect())
  }

  if (scenario == 'scenario2') {
    return(data_scenario[['core']] %>% dplyr::collect())
  }

  if (scenario == 'scenario3') {
    # inputs needed
    admin_div_val <- admin_div

    return(
      data_scenario[['core']] %>%
        dplyr::collect() %>%
        tidyIFN::summarise_polygons(polygon_group = admin_div_val)
    )
  }

  if (scenario == 'scenario4') {
    # inputs needed
    admin_div_val <- admin_div
    agg_level_val <- glue::glue('id{agg_level')

    return(
      data_scenario[['core']] %>%
        dplyr::collect() %>%
        tidyIFN::summarise_polygons(
          polygon_group = admin_div_val, func_group = agg_level_val
        )
    )
  }
}