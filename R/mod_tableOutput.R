#' @title mod_tableOutput and mod_table
#'
#' @description A shiny module to generate the base IFN plots table
#'
#' @param id shiny id
#' @param ifndb pool object to access the ifn db
#'
#' @export
mod_tableOutput <- function(id, ifndb) {

  # ns
  ns <- shiny::NS(id)

  # UI
  shiny::tagList(

    shiny::fluidRow(
      shiny::column(
        2,
        shiny::fluidRow(
          shinyWidgets::downloadBttn(
            ns('dwl_csv_button'),
            label_getter(ifndb, 'esp', 'dwl_csv_button_label'),
            color = 'primary', size = 'sm', block = FALSE,
            style = 'stretch'
          ),
          shinyWidgets::downloadBttn(
            ns('dwl_xlsx_button'),
            label_getter(ifndb, 'esp', 'dwl_xlsx_button_label'),
            color = 'primary', size = 'sm', block = FALSE,
            style = 'stretch'
          )
        ),

        shiny::hr(),

        shiny::h4('Columnas visibles'),
        shiny::fluidRow(
          shiny::inputPanel(
            shinyWidgets::pickerInput(
              ns('col_vis_selector'),
              # label_getter(ifndb, 'esp', 'col_vis_selector_label'),
              label = 'variables IFN',
              choices = '', multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = 'None selected...',
                `select-all-text` = 'All selected',
                `selected-text-format` = 'count',
                `count-selected-text` = "{0} variables selected (of {1})"
              )
            ),
            shinyWidgets::actionBttn(
              ns('apply_col_vis'),
              'Aplicar',
              icon = shiny::icon('eye'),
              style = "stretch",
              block = FALSE,
              size = 'sm'
            )
          )
        ),

        shiny::h4(label_getter(ifndb, 'esp', 'col_filter_h4_label')),
        shiny::fluidRow(
          shiny::inputPanel(
            shinyWidgets::pickerInput(
              ns('col_filter_selector'),
              label_getter(ifndb, 'esp', 'col_filter_selector_label'),
              choices = '', multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = 'None selected...',
                `select-all-text` = 'All selected',
                `selected-text-format` = 'count',
                `count-selected-text` = "{0} variables selected (of {1})"
              )
            ),
            shiny::uiOutput(ns('col_filter')),
            shinyWidgets::actionBttn(
              ns('apply_table_filters'),
              'Aplicar',
              icon = shiny::icon('eye'),
              style = "stretch",
              block = FALSE,
              size = 'sm'
            )
          )
        )
        # shiny::fluidRow(
        #   shinyWidgets::actionBttn(
        #     ns('apply_table_filters'),
        #     label_getter(ifndb, 'esp', 'apply_table_filters_label'),
        #     icon = shiny::icon('eye'),
        #     style = "material-flat",
        #     block = FALSE,
        #     size = 'sm'
        #   )
        # )
      ),
      shiny::column(
        10,
        DT::DTOutput(ns('ifn_table'), height = 'auto') %>%
        # formattable::formattableOutput(ns('ifn_table')) #%>%
          shinycssloaders::withSpinner(
            type = 4, color = '#D2527F'
          ),

        # show query button
        shinyWidgets::actionBttn(
          ns('see_query'),
          label_getter(ifndb, 'esp', 'see_query_label'),
          icon = shiny::icon('database'),
          style = "stretch",
          block = FALSE,
          size = 'sm'
        )
      )
    )
  )
}

#' mod_table server function
#' @param input internal
#' @param output internal
#' @param session internal
#' @param mod_data reactive with the reactive data and the data inputs
#' @param mod_advancedFilters reactive with the reactive values from
#'   advancedFilters module
#' @param mod_map reactive with the mod_map inputs
#' @param ifndb db pool
#'
#' @importFrom magrittr %T>%
#'
#' @export
#'
#' @rdname mod_tableOutput
mod_table <- function(
  input, output, session,
  mod_data, mod_advancedFilters, mod_map, ifndb
) {

  scenario_reac <- shiny::reactive({
    get_scenario(mod_data$viz_shape, mod_data$agg_level)
  })

  # base data reactives
  base_data_reactives <- shiny::reactive({
    base_data_reactives <- list()
    base_data_reactives$admin_div <- mod_data$admin_div
    base_data_reactives$viz_shape <- mod_data$viz_shape
    # base_data_reactives$admin_div_fil <- mod_data$admin_div_fil
    base_data_reactives$espai_tipus <- mod_data$espai_tipus
    # base_data_reactives$espai_tipus_fil <- mod_data$espai_tipus_fil
    base_data_reactives$ifn <- mod_data$ifn
    base_data_reactives$agg_level <- mod_data$agg_level
    base_data_reactives$apply_filters <- mod_data$apply_filters
    base_data_reactives$diameter_classes <- mod_data$diameter_classes
    base_data_reactives$map_draw_new_feature <- mod_map$map_draw_new_feature
    base_data_reactives$map_draw_deleted_features <- mod_map$map_draw_deleted_features

    return(base_data_reactives)
  })

  # base data from data_scenario
  table_base_data_raw <- shiny::eventReactive(
    ignoreInit = FALSE,
    eventExpr = base_data_reactives(),
    valueExpr = {

      # create a progress object to indicate the user this will take time
      progress <- shiny::Progress$new(min = 0, max = 0.32)
      progress$set(
        value = 0,
        message = label_getter(ifndb, 'esp', 'progress_table_base_data_raw_label', 'message')
      )
      on.exit(progress$close())

      updateProgress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }

      data_scenario_table <- data_scenario(
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
        mod_map$custom_polygon(),
        updateProgress = updateProgress
      )

      # check data integrity (zero rows)
      if (
        {
          data_scenario_table[['clima']] %>%
            dplyr::collect() %>%
            nrow()
        } < 1
      ) {

        shinyWidgets::sendSweetAlert(
          session,
          title = label_getter(ifndb, 'esp', 'sweetalert_table_base_data_raw_label', 'title'),
          text = label_getter(ifndb, 'esp', 'sweetalert_table_base_data_raw_label', 'text'),
          type = 'warning'
        )

        return(NULL)

      } else {
        return(data_scenario_table)
      }
    }
  )

  table_base_data <- shiny::reactive({

    # create a progress object to indicate the user this will take time
    progress <- shiny::Progress$new(min = 0.33, max = 0.62)
    progress$set(
      value = 0.33,
      message = label_getter(ifndb, 'esp', 'progress_table_base_data_label', 'message')
    )
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }

    table_base_data_raw() %>%
      table_data_modificator(
        scenario_reac(),
        mod_data$admin_div, mod_data$agg_level, mod_data$diameter_classes,
        ifndb, updateProgress = updateProgress
      )
  })

  # update col_filter_selector variables.
  shiny::observe({

    cd <- ifelse(mod_data$diameter_classes, 'cd', 'nocd')

    present_vars <- table_base_data() %>% names()

    # browser()

    col_vis_choices <- dplyr::tbl(ifndb, 'col_vis_thesaurus') %>%
      dplyr::filter(
        scenario_id == !!scenario_reac(),
        cd_id == cd,
        col_vis_val %in% present_vars
      ) %>%
      dplyr::collect() %>% {
        magrittr::set_names(
          magrittr::extract2(., 'col_vis_val'), magrittr::extract2(., 'esp')
        )
      }

    shinyWidgets::updatePickerInput(
      session, 'col_vis_selector',
      # label_getter(ifndb, 'esp', 'col_vis_selector_label'),
      label = 'variables IFN',
      choices = col_vis_choices,
      selected = col_vis_choices[1:8]
    )

    shinyWidgets::updatePickerInput(
      session, 'col_filter_selector',
      label_getter(ifndb, 'esp', 'col_filter_selector_label'),
      choices = col_vis_choices
    )
  })

  output$col_filter <- shiny::renderUI({

    # get the session ns to be able to tag the inputs with correct id
    ns <- session$ns

    # diameter classes
    cd <- ifelse(mod_data$diameter_classes, 'cd', 'nocd')

    # we create the input list with lapply, easy peachy
    col_filter_inputs <- shiny::reactive({

      data_temp <- table_base_data()

      lapply(
        input$col_filter_selector, function(var) {
          if (!is.numeric(data_temp[[var]])) {

            var_label <- dplyr::tbl(ifndb, 'col_vis_thesaurus') %>%
              dplyr::filter(
                scenario_id == !!scenario_reac(),
                cd_id == cd,
                col_vis_val == var
              ) %>%
              dplyr::collect() %>%
              dplyr::pull(esp)

            shinyWidgets::pickerInput(
              ns(var), label = var_label,
              choices = unique(data_temp[[var]]),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = 'None selected...',
                `select-all-text` = 'All selected',
                `selected-text-format` = 'count',
                `count-selected-text` = "{0} values selected (of {1})"
              ),
              width = '100%'
            )
          } else {
            min_var <- floor(min(data_temp[[var]], na.rm = TRUE))
            max_var <- ceiling(max(data_temp[[var]], na.rm = TRUE))
            var_label <- dplyr::tbl(ifndb, 'col_vis_thesaurus') %>%
              dplyr::filter(
                scenario_id == !!scenario_reac(),
                cd_id == cd,
                col_vis_val == var
              ) %>%
              dplyr::collect() %>%
              dplyr::pull(esp)

            shiny::sliderInput(
              ns(var), label = var_label,
              min = min_var,
              max = max_var,
              round = 1,
              value = c(min_var, max_var),
              width = '100%'
            )
          }
        }
      )
    })

    # tag list to return the inputs
    shiny::tagList(
      col_filter_inputs()
    )
  })

  # reactive for col_vis and col_fil buttons
  apply_buttons_reactives <- shiny::reactive({
    apply_reactives <- list()
    apply_reactives$col_vis <- input$apply_col_vis
    apply_reactives$col_fil <- input$apply_table_filters
    apply_reactives$col_fil <- input$apply_col_vis
  })

  # quo filter expression constructor
  col_filter_expressions <- shiny::eventReactive(
    ignoreInit = FALSE, ignoreNULL = FALSE,
    eventExpr = apply_buttons_reactives(),
    valueExpr = {

      # check if adv_fil_clima_variables is null or empty, to avoid problems in
      # data_scenario helper function
      if (is.null(input$col_filter_selector) || input$col_filter_selector == '') {
        return(rlang::quos())
      } else {
        data_temp <- table_base_data()

        # return the list of quos to supply to tidyIFN::data_clima
        lapply(
          input$col_filter_selector,
          function(var) {

            if (!is.numeric(data_temp[[var]])) {
              rlang::quo(
                !!rlang::sym(var) %in% c(!!input[[var]])
              )
            } else {
              rlang::quo(
                between(!!rlang::sym(var), !!input[[var]][1], !!input[[var]][2])
              )
            }
          }
        )
      }
    }
  )

  # base_data_modifs_reactives
  base_data_modifs_reactives <- shiny::reactive({
    base_data_modifs_reactives <- list()
    base_data_modifs_reactives$table_base_data <- table_base_data()
    # base_data_modifs_reactives$col_vis_selector <- input$col_vis_selector
    base_data_modifs_reactives$apply_table_filters <- input$apply_table_filters
    base_data_modifs_reactives$apply_col_vis <- input$apply_col_vis
  })

  # table final modifications based on table filters and col visibilty
  table_base_data_modifs <- shiny::eventReactive(
    ignoreInit = FALSE, ignoreNULL = FALSE,
    eventExpr = base_data_modifs_reactives(),
    valueExpr = {
      shiny::validate(
        shiny::need(
          table_base_data(),
          label_getter(ifndb, 'esp', 'table_base_data_modifs_validate_label', 'first')
        )
      )

      # create a progress object to indicate the user this will take time
      progress <- shiny::Progress$new(min = 0.63, max = 1)
      progress$set(
        value = 0.63,
        message = label_getter(ifndb, 'esp', 'progress_table_base_data_modifs_label', 'message')
      )
      on.exit(progress$close())

      updateProgress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }

      # if (is.null(input$col_vis_selector)) {
      if (
        all(
          is.null(input$col_vis_selector), is.null(input$col_vis_sig_selector),
          is.null(input$col_vis_clima_selector)
        )
      ) {
        # updateProgress setup
        updateProgress(
          value = 0.73,
          detail = ''
        )

        data_table_temp <- table_base_data() %>%
          dplyr::filter(!!! col_filter_expressions())

        shiny::validate(
          shiny::need(
            nrow(data_table_temp) > 0,
            label_getter(ifndb, 'esp', 'table_base_data_modifs_validate_label', 'second')
          )
        )
      } else {
        # updateProgress setup
        updateProgress(
          value = 0.73,
          detail = ''
        )
        data_table_temp <- table_base_data() %>%
          dplyr::filter(!!! col_filter_expressions()) %>%
          dplyr::select(dplyr::one_of(input$col_vis_selector))


        shiny::validate(
          shiny::need(
            nrow(data_table_temp) > 0,
            label_getter(ifndb, 'esp', 'table_base_data_modifs_validate_label', 'second')
          )
        )
      }

      # updateProgress setup
      updateProgress(value = 0.89, detail = '')

      numeric_cols <- names(data_table_temp)[data_table_temp %>%
                                               purrr::map_lgl(is.numeric)]

      data_table_mutated <- data_table_temp %>%
        DT::datatable(
          style = 'default', rownames = FALSE,
          fillContainer = TRUE, autoHideNavigation = TRUE,
          # class = 'display nowrap',
          options = list(
            dom = 'ti',
            scrollY = '600px',
            scrollCollapse = FALSE,
            deferRender = TRUE,
            # deferLoading = nrow(data_table_temp),
            paging = FALSE
          )
        ) %>%
        DT::formatRound(
          columns = numeric_cols,
          digits = 2
        )

      # data_table_mutated <- data_table_temp %>%
      #   DT::datatable(
      #     style = 'default', rownames = FALSE,
      #     fillContainer = TRUE, autoHideNavigation = TRUE,
      #     extensions = c('Scroller'),
      #     options = list(
      #       autoWidth = TRUE,
      #       deferRender = TRUE, scrollY = '70vh', scroller = TRUE,
      #       dom = 'ti'
      #     )
      #   ) %>%
      #   DT::formatRound(
      #     columns = numeric_cols,
      #     digits = 2
      #   )

      for (var in numeric_cols) {
        data_table_mutated <- data_table_mutated %>%
          DT::formatStyle(
            columns = var,
            background = DT::styleColorBar(
              range(data_table_temp[[var]], na.rm = TRUE) + c(-1,1),
              '#3fc380', 90
            ),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      }

      updateProgress(value = 0.99, detail = '')
      return(data_table_mutated)
    }
  )

  output$ifn_table <- DT::renderDT(
    server = TRUE,
    expr = {
      table_base_data_modifs()
    }
  )

  output$dwl_csv_button <- shiny::downloadHandler(
    filename = function() {
      'IFN_data.csv'
    },
    content = function(file) {

      res <- table_base_data_modifs()

      res$x$data[,-1] %>%
        readr::write_csv(file)
    }
  )

  output$dwl_xlsx_button <- shiny::downloadHandler(
    filename = function() {
      'IFN_data.xlsx'
    },
    content = function(file) {

      res <- table_base_data_modifs()

      res$x$data[,-1] %>%
        writexl::write_xlsx(file)
    }
  )

  # sweetalert to show the query
  shiny::observeEvent(
    eventExpr = input$see_query,
    handlerExpr = {
      # ns <- session$ns
      shinyWidgets::sendSweetAlert(
        session = session,
        title = label_getter(ifndb, 'esp', 'sweetalert_see_query_label', 'title'),
        text = shiny::tags$div(
          query_builder(mod_data, col_filter_expressions)
        ),
        html = TRUE,
        btn_labels = label_getter(ifndb, 'esp', 'sweetalert_see_query_label', 'btn_labels')
      )
    }
  )

}