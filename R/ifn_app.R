#' function to launch the ifn app
#'
#' @importFrom magrittr %>%
#'
#' @export
ifn_app <- function() {

  ### DB access ################################################################
  ifndb <- tidyIFN::ifn_connect(
    'guest',
    'guest',
    'ifndb'
  )

  ## UI ####
  ui <- shiny::tagList(

    shinyjs::useShinyjs(),
    shinyWidgets::chooseSliderSkin(skin = "Flat", color = '#0DB3D4'),

    shiny::navbarPage(
      # opts
      title = "Eines d'anàlisi IFN",
      id = 'nav',
      collapsible = TRUE,

      # contents
      shiny::tabPanel(
        "Mapa interactiu",

        shiny::div(
          class = "outer",
          shiny::tags$head(
            # custom css
            shiny::includeCSS(
              system.file('resources', 'ifn.css', package = 'IFNappkg')
            ),
            # custom scripts
            ## easyPrint leaflet plugin
            shiny::tags$script(
              src = "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"
            )
          ),

          ########################################################### debug ####
          # shiny::absolutePanel(
          #   id = 'debug', class = 'panel panel-default', fixed = TRUE,
          #   draggable = TRUE, width = 640, height = 'auto',
          #   # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
          #   # top = 'auto', left = 'auto', right = 100, bottom = 100,
          #   top = 60, left = 'auto', right = 50, bottom = 'auto',
          #
          #   shiny::textOutput('debug1'),
          #   shiny::textOutput('debug2'),
          #   shiny::textOutput('debug3')
          # ),
          ####################################################### end debug ####

          ## mod_data ####
          # mod_data module, it includes the dataSel, dataFil and dataAgg inputs
          mod_dataInput('mod_dataInput', ifndb),

          ## mod_map ####
          # mod_map, it includes the map
          mod_mapUI('mod_mapUI'),

          ## mod_infoPanel ####
          # mod_infoPanel, it includes the map events info panel
          mod_infopanelUI('mod_infopanelUI'),

          ## cite div ####
          shiny::tags$div(
            id = 'cite',
            "Dades compilats pel CREAF & CTFC basats en l'IFN"
          )
        )
      ),

      # data tab
      shiny::tabPanel(
        "Explora les dades",

        shiny::div(
          class = 'inner',
          mod_tableOutput('mod_tableOutput')
        )
      ),

      # Alometrias tab
      shiny::tabPanel(
        "Alometrías"
      )
    )
  )

  ## SERVER ####
  server <- function(input, output, session) {

    ## module calling ####

    # data
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput',
      ifndb
    )

    # map
    map_reactives <- shiny::callModule(
      mod_map, 'mod_mapUI',
      data_reactives, data_reactives$advancedFIlters_reactives, ifndb
    )

    # info panel
    shiny::callModule(
      mod_infopanel, 'mod_infopanelUI',
      data_reactives, map_reactives, data_reactives$advancedFIlters_reactives, ifndb
    )

    # table
    shiny::callModule(
      mod_table, 'mod_tableOutput',
      data_reactives, data_reactives$advancedFIlters_reactives, map_reactives, ifndb
    )

    ## debug #####
    # output$debug1 <- shiny::renderPrint({
    #   map_reactives$map_draw_edited_features
    # })
    # output$debug2 <- shiny::renderPrint({
    #   map_reactives$map_draw_all_features
    # })
    # output$debug3 <- shiny::renderPrint({
    #   map_reactives$map_draw_new_feature
    # })
  }

  # Run the application
  ifn_app_res <- shiny::shinyApp(
    ui = ui, server = server,
    onStart = function() {

      ## on stop routine to cloose the db pool
      shiny::onStop(function() {
        pool::poolClose(ifndb)
      })
    }
  )

  # shiny::runApp(ifn_app)
  return(ifn_app_res)

}