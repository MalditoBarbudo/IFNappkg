### Data meta-module

#### Data module ####

#' @title mod_dataInput and mod_data
#'
#' @description Module to create and populate the data inputs
#'
#' @param id shiny id
#' @param nfidb pool object to access the nfi database
#'
#' @export
mod_dataInput <- function(id, nfidb) {

  # ns
  ns <- shiny::NS(id)

  # choices
  # TODO fill with the thesauruses when done
  nfi_choices <- dplyr::tbl(nfidb, 'ifn_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'ifn_id'), magrittr::extract2(., 'esp')
      )
    }

  viz_shape_choices <- dplyr::tbl(nfidb, 'viz_shape_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'viz_shape_id'), magrittr::extract2(., 'esp')
      )
    }

  agg_level_choices <- dplyr::tbl(nfidb, 'agg_level_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'agg_level_id'), magrittr::extract2(., 'esp')
      )
    }

  admin_div_choices <- dplyr::tbl(nfidb, 'admin_div_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'admin_div_id'), magrittr::extract2(., 'esp')
      )
    }

  protec_area_type_choices <- dplyr::tbl(nfidb, 'espai_tipus_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'espai_tipus_id'), magrittr::extract2(., 'esp')
      )
    }

  colour_choices <- dplyr::tbl(nfidb, 'color_thesaurus') %>%
    dplyr::filter(scenario_id == 'scenario3') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'color_id'), magrittr::extract2(., 'esp')
      )
    }

  size_choices <- dplyr::tbl(nfidb, 'mida_thesaurus') %>%
    dplyr::filter(scenario_id == 'scenario3') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'mida_id'), magrittr::extract2(., 'esp')
      )
    }

  statistic_choices <- dplyr::tbl(nfidb, 'statistic_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'statistic_id'), magrittr::extract2(., 'esp')
      )
    }

  # UI
  shiny::tagList(

    #### Data main selector ####
    shiny::h4('Data selection'),
    shiny::fluidRow(

      shiny::column(
        6,
        # nfi version
        shiny::selectInput(
          ns('nfi'),
          label = 'NFI version',
          choices = nfi_choices,
          selected = 'ifn4'
        )
      ),
      shiny::column(
        6,
        # plots or polygons?
        shiny::radioButtons(
          ns('viz_shape'),
          label = 'Data visualization type',
          choices = viz_shape_choices,
          selected = 'parcela'
        )
      )
    ),

    shiny::fluidRow(
      shiny::div(
        # Administrative divisions
        shiny::selectInput(
          ns('admin_div'),
          label = 'Administrative divisions',
          choices = admin_div_choices,
          selected = 'comunidad'
        )
      )
    ),

    shiny::fluidRow(
      shiny::div(
        # Protected areas
        shiny::selectInput(
          ns('protec_area_type'),
          label = 'Protected areas',
          choices = protec_area_type_choices,
          selected = 'proteccio'
        )
      )
    ),

    shiny::fluidRow(
      shiny::div(
        # aggregation level
        shiny::selectInput(
          ns('agg_level'),
          label = 'Aggregation level',
          choices = agg_level_choices
        )
      )
    ),

    shiny::hr(),

    #### Viz inputs ####
    shiny::h4('Data visualization'),
    shiny::fluidRow(
      shiny::div(
        id = 'colour_and reverse',
        # colour selector
        shiny::selectInput(
          ns('colour'),
          label = 'Colour',
          choices = colour_choices
        ),
        # reverse palette
        shiny::checkboxInput(
          ns('reverse_pal'),
          label = 'Reverse palette',
          value = FALSE
        )
      )
    ),

    shiny::fluidRow(
      shiny::div(
        id = 'hideable_size',
        # size selector
        shiny::selectInput(
          ns('size'),
          label = 'Size',
          choices = size_choices,
          selected = ''
        )
      )
    ),

    shinyjs::hidden(
      shiny::div(
        id = ns('hideable_statistic'),
        # statistic selector
        shiny::selectInput(
          ns('statistic'),
          label = 'Statistic',
          choices = statistic_choices
        )
      )
    )
  )
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param nfidb
#'
#' @export
#'
#' @rdname mod_dataInput
mod_data <- function(
  input, output, session, nfidb
) {

  # scenario to play with
  scenario <- shiny::reactive({
    tidyIFN::get_scenario(input$viz_shape, input$agg_level)
  })

  # viz input's updaters
  # colour
  shiny::observeEvent(
    eventExpr = scenario(),
    handlerExpr = {
      colour_choices <- dplyr::tbl(nfidb, 'color_thesaurus') %>%
        dplyr::filter(scenario_id == !!scenario()) %>%
        dplyr::collect() %>% {
          magrittr::set_names(
            magrittr::extract2(., 'color_id'), magrittr::extract2(., 'esp')
          )
        }

      shiny::updateSelectInput(
        session, 'colour',
        label = 'Colour',
        choices = colour_choices
      )
    }
  )

  # size
  shiny::observeEvent(
    eventExpr = scenario(),
    handlerExpr = {

      if (scenario() %in% c('scenario1', 'scenario2')) {
        size_choices <- dplyr::tbl(nfidb, 'mida_thesaurus') %>%
          dplyr::filter(scenario_id == !!scenario()) %>%
          dplyr::collect() %>% {
            magrittr::set_names(
              magrittr::extract2(., 'mida_id'), magrittr::extract2(., 'esp')
            )
          }

        shiny::updateSelectInput(
          session, 'size',
          label = 'Size',
          choices = size_choices
        )

        shinyjs::show('hideable_size')
      } else {
        shinyjs::hide('hideable_size')
      }
    }
  )

  # statistic
  shiny::observeEvent(
    eventExpr = scenario(),
    handlerExpr = {

      if (scenario() %in% c('scenario3', 'scenario4')) {
        shinyjs::show('hideable_statistic')
      } else {
        shinyjs::hide('hideable_statistic')
      }
    }
  )

  # create a reactive values object to return as a result of the function
  mod_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    mod_data_reactives$scenario = scenario
    mod_data_reactives$nfi = input$nfi
    mod_data_reactives$viz_shape = input$viz_shape
    mod_data_reactives$agg_level = input$agg_level
    mod_data_reactives$admin_div = input$admin_div
    mod_data_reactives$protec_area_type = input$protec_area_type
    mod_data_reactives$colour = input$colour
    mod_data_reactives$reverse_pal = input$reverse_pal
    mod_data_reactives$size = input$size
    mod_data_reactives$statistic = input$statistic
  })


  return(mod_data_reactives)


}


#### preFilter module ####
#' @title mod_preFilInput and mod_preFil
#'
#' @description Module to create and populate the pre-filtering inputs
#'
#' @param id shiny id
#' @param nfidb pool object to access the nfi database
#'
#' @export
mod_preFilInput <- function(id, nfidb) {

  ns <- shiny::NS(id)

  admin_div_fil_choices <- dplyr::tbl(nfidb, 'admin_div_fil_thesaurus') %>%
    dplyr::filter(admin_div_fil_id == 'comarca') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'admin_div_fil_value'), magrittr::extract2(., 'esp')
      )
    }

  protec_area_type_fil_choices <- dplyr::tbl(nfidb, 'espai_tipus_fil_thesaurus') %>%
    dplyr::filter(espai_tipus_fil_id == 'proteccio') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'espai_tipus_fil_value'), magrittr::extract2(., 'esp')
      )
    }

  nfi_prefil_vars_choices <- c('var1', 'var2', 'var3')

  shiny::tagList(

    shiny::h4('Data filtering'),

    shiny::fluidRow(
      shiny::column(
        6,
        shinyWidgets::pickerInput(
          ns('admin_div_fil'),
          label = 'Administrative divisions filter',
          choices = admin_div_fil_choices,
          selected = '', multiple = TRUE, width = '100%',
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = 'None selected...',
            `select-all-text` = 'All selected',
            `selected-text-format` = 'count > 3',
            `count-selected-text` = "{0} of {1}",
            `size` = 15
          )
        )
      ),
      shiny::column(
        6,
        shinyWidgets::pickerInput(
          ns('protec_area_type_fil'),
          label = 'Protected areas filter',
          choices = protec_area_type_fil_choices,
          selected = '', multiple = TRUE, width = '100%',
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = 'None selected...',
            `select-all-text` = 'All selected',
            `selected-text-format` = 'count > 3',
            `count-selected-text` = "{0} of {1}",
            `size` = 15
          )
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        12,
        shinyWidgets::pickerInput(
          ns('nfi_prefil_vars'),
          label = 'NFI data variables to filter',
          choices = nfi_prefil_vars_choices,
          selected = '', multiple = TRUE, width = '100%',
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = 'None selected...',
            `select-all-text` = 'All selected',
            `selected-text-format` = 'count > 3',
            `count-selected-text` = "{0} of {1}",
            `size` = 15
          )
        ),
        shiny::uiOutput(ns('nfi_pre_filters'))
      )
    )
  )
}

#' mod_preFil server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param nfidb
#' @param mod_data reactives from mod_data server module
#'
#' @export
#'
#' @rdname mod_preFilInput
mod_preFil <- function(
  input, output, session, nfidb,
  mod_data
) {

  # observers to update the inputs
  shiny::observe({
    # create the input choices based on the administrative division input
    if (is.null(input$admin_div) | input$admin_div == '') {

      shinyjs::reset('admin_div_fil')
      shinyjs::disable('admin_div_fil')

    } else {
      admin_div_fil_choices <- dplyr::tbl(nfidb, 'admin_div_fil_thesaurus') %>%
        dplyr::filter(admin_div_fil_id == input$admin_div) %>%
        dplyr::collect() %>% {
          magrittr::set_names(
            magrittr::extract2(., 'admin_div_fil_value'), magrittr::extract2(., 'esp')
          )
        }

      shinyWidgets::updatePickerInput(
        session, 'admin_div_fil',
        label = label_getter(nfidb, 'esp', 'admin_div_fil_label', input$admin_div),
        choices = admin_div_fil_choices
      )

      shinyjs::enable('admin_div_fil')
    }
  })

  shiny::observe({
    # get the protection level and create the choices based on the dic
    protec_area_type_fil_choices <- dplyr::tbl(nfidb, 'protec_area_type_fil_thesaurus') %>%
      dplyr::filter(protec_area_type_fil_id == input$protec_area_type) %>%
      dplyr::collect() %>% {
        magrittr::set_names(
          magrittr::extract2(., 'protec_area_type_fil_value'),
          magrittr::extract2(., 'esp')
        )
      }

    shinyWidgets::updatePickerInput(
      session, 'protec_area_type_fil',
      label = label_getter(nfidb, 'esp', 'protec_area_type_fil_label', input$protec_area_type),
      choices = protec_area_type_fil_choices
    )
  })


  # renderUI builder and expressions builders (using tidyIFN)
  nfi_prefil_vars <- shiny::reactive({
    input$nfi_prefil_vars
  }) %>%
    # debounce to avoid to frequent invalidations
    shiny::debounce(1000)

  output$nfi_pre_filters <- shiny::renderUI({

    # get the session ns to be able to tag the created inputs woth the correct
    # id
    ns <- session$ns

    # we need to know if the variable is numeric (slider inputs) or categorical
    # (picker inputs). If we have this info in the database we can avoid to
    # load here the data.
    numeric_vars <- dplyr::tbl(nfidb, 'numeric_vars') %>% dplyr::collect()
    categorical_vars <- dplyr::tbl(nfidb, 'categorical_vars') %>% dplyr::collect()

    # we create the inputs list
    inputs_list <- shiny::reactive({

      # dummy function
      dummy_input_fun <- function(var) {
        if (var %in% categorical_vars) {
          var_label <- dplyr::tbl(nfidb, 'col_vis_thesaurus') %>%
            dplyr::filter(
              scenario_id == !!mod_data$scenario(),
              cd_id == cd,
              col_vis_val == var
            ) %>%
            dplyr::collect() %>%
            dplyr::pull(esp)

          shinyWidgets::pickerInput(
            ns(var), label = var_label,
            choices
          )
        }
      }


    })



  })


  # create a reactive values object to return as a result of the function
  mod_prefil_reactives <- shiny::reactiveValues()
  shiny::observe({
    mod_prefil_reactives$admin_div_fil <- input$admin_div_fil
    mod_prefil_reactives$protec_area_type_fil <- input$protec_area_type_fil
    mod_prefil_reactives$nfi_prefil_vars <- input$nfi_prefil_vars

  })


  return(mod_prefil_reactives)


}