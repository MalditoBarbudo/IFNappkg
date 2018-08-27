#' @title mod_buttonsInput and mod_buttons
#'
#' @description A shiny module to create and populate the buttons inputs
#'
#' @param id shiny id
#'
#' @export
mod_buttonsInput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # Buttons
  shiny::absolutePanel(
    # panel settings
    id = 'dataControls', class = 'panel panel-default', fixed = TRUE,
    draggable = TRUE, width = 240, height = 'auto',
    # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
    # top = 'auto', left = 'auto', right = 100, bottom = 100,
    top = 500, right = 'auto', left = 50, bottom = 'auto',

    # save map button
    # shinyWidgets::actionBttn(
    #   ns('save_map_bttn'), 'Save the map',
    #   icon = shiny::icon('download'), style = 'material-circle', size = 'sm'
    # )

    # ... other buttons, to develop
    shinyWidgets::actionBttn(
      ns('show_filter_def'), 'F',
      style = 'material-circle', size = 'sm'
    ),
    shinyWidgets::actionBttn(
      ns('show_filter_adv'), 'A-F',
      style = 'material-circle', size = 'sm'
    ),
    shinyWidgets::actionBttn(
      ns('show_agg'), 'Ag',
      style = 'material-circle', size = 'sm'
    )

  )
}

#' mod_buttons server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @export
#'
#' @rdname mod_buttonsInput
mod_buttons <- function(
  input, output, session
) {

  # reactive values from buttons to return
  mod_buttons_reactives <- shiny::reactiveValues()

  shiny::observe({
    mod_buttons_reactives$show_filter_def <- input$show_filter_def
    mod_buttons_reactives$show_filter_adv <- input$show_filter_adv
    mod_buttons_reactives$show_agg <- input$show_agg
  })

  return(mod_buttons_reactives)

  # observeEvent for save_map_bttn, to trigger the modal dialog
  # shiny::observeEvent(
  #   eventExpr = input$save_map_bttn,
  #   handlerExpr = {
  #
  #     ns <- session$ns
  #
  #     shiny::showModal(
  #       shiny::modalDialog(
  #         # pseudo title
  #         shiny::h4('Select the format to download'),
  #
  #         # footer with buttons
  #         footer = shiny::tagList(
  #           shinyWidgets::downloadBttn(
  #             ns('save_map_png'), 'png', 'material-flat', size = 'sm'
  #           ),
  #           shinyWidgets::downloadBttn(
  #             ns('save_map_pdf'), 'pdf', 'material-flat', size = 'sm'
  #           ),
  #           # shinyWidgets::downloadBttn(
  #           #   ns('save_map_sh'), 'shapefile', 'material-circle', size = 'sm'
  #           # ),
  #           shiny::modalButton('Dismiss')
  #         )
  #       )
  #     )
  #   }
  # )

  # outputs for the downloadHandlers
  # output$save_map_png <- shiny::downloadHandler(
  #   filename = 'ifn_map.html',
  #   content = function(file) {
  #     htmlwidgets::saveWidget(mod_map$input_map(), file = file)
  #   }
  # )
  #
  # output$save_map_pdf <- shiny::downloadHandler(
  #   filename = 'ifn_map.pdf',
  #   content = function(file) {
  #     mapview::mapshot(mod_map$input_map(), file = file)
  #   }
  # )

  # shiny::observe({
  #   foo <- mod_map$input_map()
  #   browser()
  # })

  # output$save_map_sh <- shiny::downloadHandler(
  #   filename = 'ifn_map.sh',
  #   content = function(file) {
  #     mapview::mapshot(mod_map$map_input(), file = file)
  #   }
  # )
}