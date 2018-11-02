### Data meta-module

# Data module

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

  protec_area_type_choices <- dplyr::tbl(nfidb, 'protec_area_type_thesaurus') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'protec_area_type_id'), magrittr::extract2(., 'esp')
      )
    }

  colour_choices <- dplyr::tbl(nfidb, 'colour_thesaurus') %>%
    dplyr::filter(scenario_id == 'scenario3') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'colour_id'), magrittr::extract2(., 'esp')
      )
    }

  size_choices <- dplyr::tbl(nfidb, 'size_thesaurus') %>%
    dplyr::filter(scenario_id == 'scenario3') %>%
    dplyr::collect() %>% {
      magrittr::set_names(
        magrittr::extract2(., 'size_id'), magrittr::extract2(., 'esp')
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
    shiny::fluidRow(
      # nfi version
      shiny::selectInput(
        ns('nfi'),
        label = 'NFI version',
        choices = nfi_choices,
        selected = 'ifn4'
      )
    ),

    shiny::fluidRow(
      # plots or polygons?
      shiny::radioButtons(
        ns('viz_shape'),
        label = 'Data visualization type',
        choices = viz_shape_choices,
        selected = 'parcela'
      )
    ),

    shiny::fluidRow(
      # aggregation level
      shiny::selectInput(
        ns('agg_level'),
        label = 'Aggregation level',
        choices = agg_level_choices
      )
    ),

    shiny::fluidRow(
      # Administrative divisions
      shiny::selectInput(
        ns('admin_div'),
        label = 'Administrative divisions',
        choices = admin_div_choices,
        selected = 'comunidad'
      )
    ),

    shiny::fluidRow(
      # Protected areas
      shiny::selectInput(
        ns('protec_area_type'),
        label = 'Protected areas',
        choices = protec_area_type_choices,
        selected = 'proteccio'
      )
    ),

    shiny::hr(),

    #### Viz inputs ####
    shiny::fluidRow(
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
    ),

    shiny::fluidRow(
      # size selector
      shiny::selectInput(
        ns('size'),
        label = 'Size',
        choices = size_choices,
        selected = ''
      )
    ),

    shinyjs::hidden(
      shiny::div(
        id = 'hideable_statistic',
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
  shiny::observeEvent(
    eventExpr = scenario(),
    handlerExpr = {

      colour_choices <- dplyr::tbl(nfidb, 'colour_thesaurus') %>%
        dplyr::filter(scenario_id == !!scenario()) %>%
        dplyr::collect() %>% {
          magrittr::set_names(
            magrittr::extract2(., 'colour_id'), magrittr::extract2(., 'esp')
          )
        }

      if (scenario() %in% c('scenario1', 'scenario2')) {
        size_choices <- dplyr::tbl(nfidb, 'size_thesaurus') %>%
          dplyr::filter(scenario_id == !!scenario()) %>%
          dplyr::collect() %>% {
            magrittr::set_names(
              magrittr::extract2(., 'size_id'), magrittr::extract2(., 'esp')
            )
          }

        shinyjs::show('size')
        shinyjs::hide('statistic')
      } else {
        shinyjs::hide('size')
        shinyjs::show('statistic')
      }

      shiny::updateSelectInput(
        session, 'colour',
        label = 'Colour',
        choices = colour_choices
      )

      shiny::updateSelectInput(
        session, 'size',
        label = 'Size',
        choices = size_choices
      )
    }
  )

  # create a reactive values object to return as a result of the function
  mod_data_reactives <- shiny::reactiveValues(
    scenario = scenario(),
    nfi = input$nfi,
    viz_shape = input$viz_shape,
    agg_level = input$agg_level,
    admin_div = input$admin_div,
    protec_area_type = input$protec_area_type,
    colour = input$colour,
    reverse_pal = input$reverse_pal,
    size = input$size,
    statistic = input$statistic
  )

  return(mod_data_reactives)


}