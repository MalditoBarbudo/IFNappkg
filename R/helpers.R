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