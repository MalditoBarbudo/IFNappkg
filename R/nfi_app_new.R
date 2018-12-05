## Main function

#' Main funcion to launch the app
#'
#' @importFrom magrittr %>%
#'
#' @export
nfi_app <- function() {

  ## DB access
  nfidb <- tidyIFN::ifn_connect()

  ## UI
  ui <- shiny::tagList(

    shinyjs::useShinyjs(),

    # layout & contents
    shiny::navbarPage(
      title = 'NFI app', id = 'navigation_tabs', collapsible = TRUE,

      # map&data tab
      shiny::tabPanel(
        title = 'Map & Data', icon = shiny::icon('tree'),

        # config
        shiny::tags$head(
          shiny::includeCSS(
            system.file('resources', 'ifn.css', package = 'IFNappkg')
          ),
          shiny::tags$script(
            src = "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"
          )
        ),

        # general sidebar for map&data
        shiny::sidebarLayout(

          sidebarPanel = shiny::sidebarPanel(
            width = 3,

            shiny::tabsetPanel(
              shiny::tabPanel(
                title = 'Data selection', icon = shiny::icon('database'),
                mod_dataInput('mod_dataInput', nfidb)
              ),
              shiny::tabPanel(
                title = 'Data filtering', icon = shiny::icon('filter'),
                mod_preFilInput('mod_preFilInput', nfidb)
              )
            )

          ),

          mainPanel = shiny::mainPanel(

            shiny::tabsetPanel(
              shiny::tabPanel(
                title = 'Map', icon = shiny::icon('map-marker'),

                #### DEBUG
                shiny::absolutePanel(
                  id = 'debuG', class = 'panel panel-default', fixed = TRUE,
                  draggable = TRUE, width = 380, height = 'auto',
                  top = 400, right = 60, left = 'auto', bottom = 'auto',

                  shiny::textOutput('debug1'),
                  shiny::textOutput('debug2'),
                  shiny::textOutput('debug3')
                )
                ####
              ),
              shiny::tabPanel(
                title = 'Table', icon = shiny::icon('table')
              )
            )

          )

        )

      )

    )

  )

  server <- function(input, output, session) {

    ## module calling ####

    # data
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput',
      nfidb
    )
    pre_fil_reactives <- shiny::callModule(
      mod_preFil, 'mod_preFilInput',
      nfidb, data_reactives
    )

    output$debug1 <- shiny::renderPrint(
      data_reactives$scenario()
    )

  }

  # Run the application
  nfi_app_res <- shiny::shinyApp(
    ui = ui, server = server,
    onStart = function() {
      ## on stop routine to cloose the db pool
      shiny::onStop((function() {
        pool::poolClose(nfidb)
      }))
    }
  )

  return(nfi_app_res)

}