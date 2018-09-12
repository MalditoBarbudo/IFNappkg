#' @title mod_dataInput and mod_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#' @param ifndb pool object to access the ifn db
#'
#' @export
mod_dataInput <- function(id, ifndb) {

  # ns
  ns <- shiny::NS(id)

  # choices
  ifn_choices <- dplyr::tbl(ifndb, 'ifn_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'ifn_id'), magrittr::extract2(., 'esp')
      )
    }

  viz_shape_choices <- dplyr::tbl(ifndb, 'viz_shape_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'viz_shape_id'), magrittr::extract2(., 'esp')
      )
    }

  admin_div_choices <- dplyr::tbl(ifndb, 'admin_div_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'admin_div_id'), magrittr::extract2(., 'esp')
      )
    }

  espai_tipus_choices <- dplyr::tbl(ifndb, 'espai_tipus_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'espai_tipus_id'), magrittr::extract2(., 'esp')
      )
    }

  agg_level_choices <- dplyr::tbl(ifndb, 'agg_level_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'agg_level_id'), magrittr::extract2(., 'esp')
      )
    }

  admin_div_fil_choices <- dplyr::tbl(ifndb, 'admin_div_fil_thesaurus') %>%
    dplyr::filter(admin_div_fil_id == 'comarca') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'admin_div_fil_value'), magrittr::extract2(., 'esp')
      )
    }

  espai_tipus_fil_choices <- dplyr::tbl(ifndb, 'espai_tipus_fil_thesaurus') %>%
    dplyr::filter(espai_tipus_fil_id == 'proteccio') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'espai_tipus_fil_value'), magrittr::extract2(., 'esp')
      )
    }



  # UI
  shiny::tagList(

    # absolute panel for all, later on we will be able to hide/show the different
    # parts of the panel
    shiny::absolutePanel(
      # panel settings
      id = 'dataControls', class = 'panel panel-default', fixed = TRUE,
      draggable = TRUE, width = 640, height = 'auto',
      # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
      # top = 'auto', left = 'auto', right = 100, bottom = 100,
      top = 60, right = 'auto', left = 50, bottom = 'auto',

      # panel contents
      # 1. data selection (div and id is for shinyjs later application)
      shiny::div(
        id = 'dataSel',

        shiny::h3(label_getter(ifndb, 'esp', 'dataSel_h3_label')),

        shiny::fluidRow(
          shiny::column(
            4,
            shinyWidgets::pickerInput(
              ns('ifn'),
              label = label_getter(ifndb, 'esp', 'ifn_label'),
              choices = ifn_choices,
              selected = 'ifn3'
            )
          ),
          shiny::column(
            6, offset = 2,
            shinyWidgets::radioGroupButtons(
              ns('viz_shape'),
              label_getter(ifndb, 'esp', 'viz_shape_label'),
              choices = viz_shape_choices, selected = 'polygon',
              status = 'info', size = 'sm', justified = TRUE,
              checkIcon = list(
                yes = shiny::icon("check"),
                no = shiny::icon("times")
              )
            )
          )
        ),

        shiny::fluidRow(
          shiny::column(
            6,
            shinyWidgets::pickerInput(
              ns('admin_div'), label_getter(ifndb, 'esp', 'admin_div_label'),
              admin_div_choices, selected = 'comarca'
            )
          ),
          shiny::column(
            6,
            shinyWidgets::pickerInput(
              ns('espai_tipus'), label_getter(ifndb, 'esp', 'espai_tipus_label'),
              espai_tipus_choices, selected = 'proteccio'
            )
          )
        ),

        # buttons module
        mod_buttonsInput(ns('mod_buttonsInput'), ifndb)
      ),

      # 2. data aggregation level (div and id is for shinyjs later application)
      shinyjs::hidden(
        shiny::div(
          id = ns('dataAgg'),

          # horizontal rule to separate
          shiny::hr(),

          shiny::h4(label_getter(ifndb, 'esp', 'dataAgg_h4_label')),

          shiny::fluidRow(
            shiny::column(
              9,
              shinyWidgets::pickerInput(
                ns('agg_level'), label_getter(ifndb, 'esp', 'agg_level_label'),
                choices = agg_level_choices,
                selected = 'parcela', width = '100%'
              ),
              shinyWidgets::awesomeCheckbox(
                ns('diameter_classes'),
                label = label_getter(ifndb, 'esp', 'diameter_classes_label'),
                status = 'info'
              )
            )
          )
        )
      ),

      # 3. data filtering (div and id is for shinyjs later application)
      #   (this inputs are created empty and filled later on in the server based
      #   on the section 1. inputs)
      shinyjs::hidden(
        shiny::div(
          id = ns('dataFil'),

          # horizontal rule to separate
          shiny::hr(),

          shiny::h4(label_getter(ifndb, 'esp', 'dataFil_h4_label')),

          shiny::fluidRow(
            shiny::column(
              6,
              shinyWidgets::pickerInput(
                ns('admin_div_fil'),
                label_getter(ifndb, 'esp', 'admin_div_fil_label', 'comarca'),
                choices = admin_div_fil_choices,
                selected = '', multiple = TRUE, width = '100%',
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = 'None selected...',
                  `select-all-text` = 'All selected',
                  `selected-text-format` = 'count',
                  `count-selected-text` = "{0} divisions selected (of {1})"
                )
              )
            ),
            shiny::column(
              6,
              shinyWidgets::pickerInput(
                ns('espai_tipus_fil'),
                label_getter(ifndb, 'esp', 'espai_tipus_fil_label', 'proteccio'),
                choices = espai_tipus_fil_choices,
                selected = '', multiple = TRUE, width = '100%',
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = 'None selected...',
                  `select-all-text` = 'All selected',
                  `selected-text-format` = 'count',
                  `count-selected-text` = "{0} divisions selected (of {1})"
                )
              )
            )
          ),

          # hidden div for advanced filters
          mod_advancedFiltersUI(ns('mod_advancedFiltersUI'), ifndb),
          # shinyjs::hidden(
          #   shiny::div(
          #     id = ns('advancedFiltersControls'),
          #     mod_advancedFiltersUI(ns('mod_advancedFiltersUI'))
          #   )
          # ),

          shiny::fluidRow(
            shiny::column(
              6, offset = 3,
              shinyWidgets::actionBttn(
                ns('apply_filters'),
                label_getter(ifndb, 'esp', 'apply_filters_label'),
                icon = shiny::icon('check'),
                style = "material-flat",
                block = TRUE,
                size = 'sm'
              )
            )
          )
        )
      )
    ), # absolute panel end

    ## vizControls ####
    shiny::div(
      id = ns('vizInputs'),
      mod_vizInput(ns('mod_vizInput'), ifndb)
    )
  ) # end of tagList
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param ifndb
#'
#' @export
#'
#' @rdname mod_dataInput
mod_data <- function(
  input, output, session, ifndb
) {

  # observers to update the dataFil inputs
  shiny::observe({
    # create the input choices based on the administrative division input
   admin_div_sel <- input$admin_div
    if (is.null(admin_div_sel) | admin_div_sel == '') {

      shinyjs::reset('admin_div_fil')
      shinyjs::disable('admin_div_fil')

    } else {
      admin_div_fil_choices <- dplyr::tbl(ifndb, 'admin_div_fil_thesaurus') %>%
        dplyr::filter(admin_div_fil_id == admin_div_sel) %>%
        dplyr::collect() %>% {
          magrittr::set_names(
            magrittr::extract2(., 'admin_div_fil_value'), magrittr::extract2(., 'esp')
          )
        }

      shinyWidgets::updatePickerInput(
        session, 'admin_div_fil',
        label = label_getter(ifndb, 'esp', 'admin_div_fil_label', admin_div_sel),
        choices = admin_div_fil_choices
      )

      shinyjs::enable('admin_div_fil')
    }
  })

  shiny::observe({
    # get the protection level and create the choices based on the dic
    espai_tipus_sel <- input$espai_tipus

    espai_tipus_fil_choices <- dplyr::tbl(ifndb, 'espai_tipus_fil_thesaurus') %>%
      dplyr::filter(espai_tipus_fil_id == espai_tipus_sel) %>%
      dplyr::collect() %>% {
        magrittr::set_names(
          magrittr::extract2(., 'espai_tipus_fil_value'), magrittr::extract2(., 'esp')
        )
      }

    shinyWidgets::updatePickerInput(
      session, 'espai_tipus_fil',
      label = label_getter(ifndb, 'esp', 'espai_tipus_fil_label', espai_tipus_sel),
      choices = espai_tipus_fil_choices
    )
  })

  # reactive values to return for use in other modules
  data_reactives <- shiny::reactiveValues()

  shiny::observe({
    # inputs
    data_reactives$ifn <- input$ifn
    data_reactives$admin_div <- input$admin_div
    data_reactives$espai_tipus <- input$espai_tipus
    data_reactives$admin_div_fil <- input$admin_div_fil
    data_reactives$espai_tipus_fil <- input$espai_tipus_fil
    data_reactives$show_adv_fils <- input$show_adv_fils
    data_reactives$apply_filters <- input$apply_filters
    data_reactives$agg_level <- input$agg_level
    data_reactives$diameter_classes <- input$diameter_classes
    data_reactives$viz_shape <- input$viz_shape

  })

  # buttons
  buttons_reactives <- shiny::callModule(
    mod_buttons, 'mod_buttonsInput'
  )

  # viz controls
  viz_reactives <- shiny::callModule(
    mod_viz, 'mod_vizInput',
    data_reactives, ifndb
  )

  # advancedFilters
  advancedFIlters_reactives <- shiny::callModule(
    mod_advancedFilters, 'mod_advancedFiltersUI',
    buttons_reactives, ifndb
  )



  # observers to show the hidden panels
  shiny::observeEvent(
    eventExpr = buttons_reactives$show_filter_def,
    handlerExpr = {
      shinyjs::toggleElement(id = 'dataFil')
    }
  )

  shiny::observeEvent(
    eventExpr = buttons_reactives$show_agg,
    handlerExpr = {
      shinyjs::toggleElement(id = 'dataAgg')
    }
  )

  # show the panel when the button is pressed
  # shiny::observeEvent(
  #   eventExpr = buttons_reactives$show_filter_adv,
  #   handlerExpr = {
  #     shinyjs::toggleElement(id = 'advancedFiltersControls')
  #   }
  # )

  # observer to show the panel when teh button is pressed
  shiny::observeEvent(
    eventExpr = buttons_reactives$show_viz,
    handlerExpr = {
      shinyjs::toggleElement('vizInputs')
    }
  )

  shiny::observe({
    # inputs
    data_reactives$color <- viz_reactives$color
    data_reactives$inverse_pal <- viz_reactives$inverse_pal
    data_reactives$mida <- viz_reactives$mida
    data_reactives$tipo_grup_func <- viz_reactives$tipo_grup_func
    data_reactives$grup_func <- viz_reactives$grup_func
    data_reactives$statistic <- viz_reactives$statistic

    # advanced_filtes
    data_reactives$advancedFIlters_reactives <- advancedFIlters_reactives
  })

  return(data_reactives)
}
