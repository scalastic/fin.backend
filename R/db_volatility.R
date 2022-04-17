#
# Volatility (e.g. VIX, VXD)
#

#' Load Volatility Index Index from DB.
#'
#' @param name the name of the index (e.g. VIX, VXD)
#'
#' @return a \code{data.table} containing the Volatility Index index.
#' @export
db_volatility.load <- function(name) {

  volatility <- db_utils.load("ref_volatility", name)
  volatility <- db_utils.name_column(as.data.table(volatility), c("date"), c("open", "high", "low", "close"), tolower(name))

  return(volatility)
}

#' Save Volatility Index Index into DB.
#'
#' @param name the name of the Volatility Index (e.g. VIX, VXD)
#' @param volatility  \code{data.table} containing the Volatility Index index to save.
#'
#' @return Nothing.
#' @export
db_volatility.save <- function(name, volatility) {

  volatility$name = name

  db_utils.save(volatility, "ref_volatility", name)
}


#' Update one specified Volatility Index into DB if exists.
#'
#' @param name the name of the Volatility Index (e.g. VIX, VXD)
#' @param to_update_volatility the \code{data.table} containing the Volatility Index to update.
#'
#' @return Nothing.
#'
#' @export
db_volatility.update <- function(name, to_update_volatility) {

  to_update_volatility$name <- name
  setkey(to_update_volatility, date)

  db_utils.update(to_update_volatility, "ref_volatility", name)
}


#' Delete one specified Volatility Index from DB if exists.
#'
#' @param name the name of the Volatility Index (e.g. VIX, VXD)
#' @param to_delete_volatility the \code{data.table} containing the Volatility Index to delete.
#'
#' @return Nothing.
#'
#' @export
db_volatility.delete <- function(name, to_delete_volatility) {

  db_utils.delete(to_delete_volatility, "ref_volatility", name)
}
