
#
# Asset
#

#' Load asset data from DB.
#'
#' @param name the name of the asset (e.g. CAC_40, BNP)
#'
#' @return a \code{data.table} containing the asset data.
#'
#' @export
db_asset.load <- function(name) {

  db_utils.load("ref_asset", name)
}


#' Save new asset only into DB.
#'
#' @param name the name of the asset (e.g. CAC_40, BNP)
#' @param asset the \code{data.table} containing the asset data.
#'
#' @return Nothing
#'
#' @import data.table
#' @export
db_asset.save <- function(name, asset) {

  asset$name <- name
  db_utils.save(asset, "ref_asset", name)
}


#' Update specified asset into DB if exists.
#'
#' @param name the name of the asset (e.g. CAC_40, BNP)
#' @param to_update_asset the \code{data.table} containing the asset to update.
#'
#' @return Nothing.
#'
#' @import data.table
#' @export
db_asset.update <- function(name, to_update_asset) {

  to_update_asset$name <- name
  setkey(to_update_asset, strdate)

  db_utils.update(to_update_asset, "ref_asset", name)
}


#' Delete specified asset from DB if exists.
#'
#' @param name the name of the asset (e.g. CAC_40, BNP)
#' @param to_delete_asset the \code{data.table} containing the asset to delete.
#'
#' @return Nothing.
#'
#' @import data.table
#' @export
db_asset.delete <- function(name, to_delete_asset) {

  db_utils.delete(to_delete_asset, "ref_asset", name)
}
