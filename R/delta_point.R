
#' Builds Delta Points
#'
#' @param asset_data
#'
#' @return a \code{data.table} containing the delta points.
#' @export
delta_point.build <- function(asset_data) {

  dt <- asset_data[, .(date, close)]
  percent_data <- 100 * c(NA, diff(dt[, close]))  / dt[, close]
  dt[, delta_point := percent_data]
  return(dt[, .(date, delta_point)])
}
