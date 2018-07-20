#' @title mod_dataInput and mod_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_dataInput <- function(id) {

  # ns
  ns <- shiny::NS(id)

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

        shiny::h3('Selecció i filtrat'),

        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(
              ns('ifn'), 'Versió de les dades', dic_ifn_choices[["esp"]],
              selected = 'ifn3'
            )
          ),
          shiny::column(
            6, offset = 2,
            shiny::radioButtons(
              ns('viz_shape'), 'Tipus de visualització',
              choices = dic_viz_shape_choices[["esp"]],
              selected = 'polygon', inline = TRUE
            )
          )
        ),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              ns('admin_div'), 'Divisions administratatives',
              dic_admin_div_choices[["esp"]], selected = 'comarca'
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              ns('espai_tipus'), "Tipus d'espai",
              dic_espai_tipus_choices[["esp"]], selected = 'proteccio'
            )
          )
        )
      ),

      # 2. data filtering (div and id is for shinyjs later application)
      #   (this inputs are created empty and filled later on in the server based
      #   on the section 1. inputs)
      shiny::div(
        id = 'dataFil',

        # horizontal rule to separate
        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              ns('admin_div_fil'), 'Filtra per division administrative',
              choices = dic_admin_div_fil_choices[["esp"]][["comarca"]],
              selected = '', multiple = TRUE, width = '100%'
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              ns('espai_tipus_fil'), "Filtra per tipus d'espai",
              choices = c('Totes' = ''),
              selected = '', multiple = TRUE, width = '100%'
            )
          )
        ),

        # here in the middle must be the advanced fiters, hidden and showed when
        # a button is pressed
        shinyjs::hidden(
          shiny::div(
            id = 'advancedFils',
            shiny::hr(),
            shiny::fluidRow(
              shiny::column(
                12,
                'Aquí los filtros avanzados (TODO)'
              )
            ),
            shiny::hr()
          )
        ),

        shiny::fluidRow(
          shiny::column(
            4, offset = 2,
            shiny::actionButton(
              ns('show_adv_fils'), 'Filtres avançats', width = '100%'
            )
          ),

          shiny::column(
            4, offset = 2,
            shiny::actionButton(
              ns('apply_filters'), 'Aplicar filtres', width = '100%'
            )
          )
        )
      ),

      # 3. data aggregation level (div and id is for shinyjs later application)
      shiny::div(
        id = 'dataAgg',

        # horizontal rule to separate
        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            9,
            shiny::selectInput(
              ns('agg_level'), "Nivell d'agregació", agg_levels,
              selected = 'parcela', width = '100%'
            )
          ),
          shiny::column(
            3,
            shiny::checkboxInput(
              ns('diam_class'), '¿Desglossar per classes diametriques?',
              value = FALSE
            )
          )
        )
      )
    ), # absolute panel end

    ## vizControls ####
    shiny::absolutePanel(
      id = 'vizControls', class = 'panel panel-default', fixed = TRUE,
      draggable = TRUE, width = 320, height = 'auto',
      top = 60, right = 'auto', left = 700, bottom = 'auto',

      mod_vizInput(ns('mod_vizInput'))
    )

  ) # end of tagList
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#'
#' @rdname mod_dataInput
mod_data <- function(
  input, output, session
) {

  # observers to update the dataFil inputs
  shiny::observe({
    # create the input choices based on the administrative division input
    admin_div_sel <- input$admin_div
    if (is.null(admin_div_sel) | admin_div_sel == '') {

      shinyjs::reset('admin_div_fil')
      shinyjs::disable('admin_div_fil')

    } else {
      shiny::updateSelectInput(
        session, 'admin_div_fil', label = paste0('Filtra per ', admin_div_sel),
        choices = dic_admin_div_choices[["esp"]][[admin_div_sel]]
      )

      shinyjs::enable('admin_div_fil')
    }
  })

  shiny::observe({
    # get the protection level and create the choices based on the dic
    espai_tipus_sel <- input$espai_tipus
    espai_tipus_fil_choices <- proteccion_dictionary[[espai_tipus_sel]]

    shiny::updateSelectInput(
      session, 'espai_tipus_fil', label = paste0('Filtra per ', espai_tipus_sel),
      choices = espai_tipus_fil_choices,
      selected = espai_tipus_fil_choices[1]
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
    data_reactives$apply_filters <- input$apply_filters
    data_reactives$agg_level <- input$agg_level
    data_reactives$diam_class <- input$diam_class
    data_reactives$viz_shape <- input$viz_shape

  })

  # viz controls
  viz_reactives <- shiny::callModule(
    mod_viz, 'mod_vizInput',
    data_reactives
  )

  shiny::observe({
    # inputs
    data_reactives$color <- viz_reactives$color
    data_reactives$inverse_pal <- viz_reactives$inverse_pal
    data_reactives$mida <- viz_reactives$mida
    data_reactives$tipo_grup_func <- viz_reactives$tipo_grup_func
    data_reactives$grup_func <- viz_reactives$grup_func
    data_reactives$statistic <- viz_reactives$statistic
  })

  return(data_reactives)
}
