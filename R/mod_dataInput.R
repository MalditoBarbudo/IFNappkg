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

        shiny::h3('Datos'),

        shiny::fluidRow(
          shiny::column(
            4,
            shinyWidgets::pickerInput(
              ns('ifn'),
              label = label_ifn[['esp']],
              choices = dic_ifn_choices[["esp"]],
              selected = 'ifn3'
            )
          ),
          shiny::column(
            6, offset = 2,
            shinyWidgets::radioGroupButtons(
              ns('viz_shape'), label_viz_shape[['esp']],
              choices = dic_viz_shape_choices[["esp"]], selected = 'polygon',
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
              ns('admin_div'), label_admin_div[['esp']],
              dic_admin_div_choices[["esp"]], selected = 'comarca'
            )
          ),
          shiny::column(
            6,
            shinyWidgets::pickerInput(
              ns('espai_tipus'), label_espai_tipus[['esp']],
              dic_espai_tipus_choices[["esp"]], selected = 'proteccio'
            )
          )
        )
      ),

      # 2. data filtering (div and id is for shinyjs later application)
      #   (this inputs are created empty and filled later on in the server based
      #   on the section 1. inputs)
      shinyjs::hidden(
        shiny::div(
          id = ns('dataFil'),

          # horizontal rule to separate
          shiny::hr(),

          shiny::fluidRow(
            shiny::column(
              6,
              shinyWidgets::pickerInput(
                ns('admin_div_fil'), label_admin_div_fil[['esp']][['comarca']],
                choices = dic_admin_div_fil_choices[["esp"]][["comarca"]],
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
                label_espai_tipus_fil[["esp"]][['proteccio']],
                choices = dic_espai_tipus_fil_choices[["esp"]][['proteccio']],
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

          shiny::fluidRow(
            shiny::column(
              4, offset = 2,
              shinyWidgets::actionBttn(
                ns('show_adv_fils'), label_show_adv_fils[['esp']],
                icon = shiny::icon('eye'),
                style = "material-flat",
                block = TRUE,
                size = 'sm'
              )
            ),

            shiny::column(
              4, offset = 2,
              shinyWidgets::actionBttn(
                ns('apply_filters'), label_apply_filters[['esp']],
                icon = shiny::icon('check'),
                style = "material-flat",
                block = TRUE,
                size = 'sm'
              )
            )
          )
        )
      ),

      # 3. data aggregation level (div and id is for shinyjs later application)
      shinyjs::hidden(
        shiny::div(
          id = ns('dataAgg'),

          # horizontal rule to separate
          shiny::hr(),

          shiny::fluidRow(
            shiny::column(
              9,
              shinyWidgets::pickerInput(
                ns('agg_level'), label_agg_level[['esp']],
                choices = dic_agg_level_choices[['esp']],
                selected = 'parcela', width = '100%'
              ),
              shinyWidgets::awesomeCheckbox(
                ns('diameter_classes'),
                label = label_diam_class[['esp']][['on']], status = 'info'
              )
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
#' @param mod_buttons reactives from the mod_buttons module
#'
#' @export
#'
#' @rdname mod_dataInput
mod_data <- function(
  input, output, session,
  mod_buttons
) {

  # observers to update the dataFil inputs
  shiny::observe({
    # create the input choices based on the administrative division input
    admin_div_sel <- input$admin_div
    if (is.null(admin_div_sel) | admin_div_sel == '') {

      shinyjs::reset('admin_div_fil')
      shinyjs::disable('admin_div_fil')

    } else {
      # shiny::updateSelectInput(
      #   session, 'admin_div_fil',
      #   label = label_admin_div_fil[["esp"]][[admin_div_sel]],
      #   choices = dic_admin_div_fil_choices[["esp"]][[admin_div_sel]]
      # )

      shinyWidgets::updatePickerInput(
        session, 'admin_div_fil',
        label = label_admin_div_fil[["esp"]][[admin_div_sel]],
        choices = dic_admin_div_fil_choices[["esp"]][[admin_div_sel]]
      )

      shinyjs::enable('admin_div_fil')
    }
  })

  shiny::observe({
    # get the protection level and create the choices based on the dic
    espai_tipus_sel <- input$espai_tipus
    # shiny::updateSelectInput(
    #   session, 'espai_tipus_fil',
    #   label = label_espai_tipus_fil[["esp"]][[espai_tipus_sel]],
    #   choices = dic_espai_tipus_fil_choices[["esp"]][[espai_tipus_sel]]
    # )
    shinyWidgets::updatePickerInput(
      session, 'espai_tipus_fil',
      label = label_espai_tipus_fil[["esp"]][[espai_tipus_sel]],
      choices = dic_espai_tipus_fil_choices[["esp"]][[espai_tipus_sel]]
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

  # viz controls
  viz_reactives <- shiny::callModule(
    mod_viz, 'mod_vizInput',
    data_reactives
  )

  # observers to show the hidden panels
  shiny::observeEvent(
    eventExpr = mod_buttons$show_filter_def,
    handlerExpr = {
      shinyjs::toggleElement(id = 'dataFil')
    }
  )

  shiny::observeEvent(
    eventExpr = mod_buttons$show_agg,
    handlerExpr = {
      shinyjs::toggleElement(id = 'dataAgg')
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
  })

  return(data_reactives)
}
