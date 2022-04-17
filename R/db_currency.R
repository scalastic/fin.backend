#
# Currency (e.g. EUR_CHF, EUR_USD, EUR_GBP, EUR_JPY)
#

#' Load all Currencies from DB.
#'
#' @return a \code{data.table} containing all the Currencies.
#' @export
db_currency.load <- function() {

  currency_aud <- db_currency.load_one("AUD")
  currency_cad <- db_currency.load_one("CAD")
  currency_chf <- db_currency.load_one("CHF")
  currency_cny <- db_currency.load_one("CNY")
  currency_gbp <- db_currency.load_one("GBP")
  currency_inr <- db_currency.load_one("INR")
  currency_jpy <- db_currency.load_one("JPY")
  currency_usd <- db_currency.load_one("USD")

    technical_tools.dynamic_merge(list(currency_aud, currency_cad, currency_chf, currency_cny, currency_gbp, currency_inr, currency_jpy, currency_usd))
}

#' Load one Currency from DB.
#'
#' @param name the name of the Currency (e.g. EUR_CHF, EUR_USD)
#'
#' @return a \code{data.table} containing the Currency.
#' @import data.table
#' @export
db_currency.load_one <- function(name) {

  currency <- db_utils.load("ref_currency", name)
  #currency <- db_utils.name_column(data.table::as.data.table(currency), c("date"), c("close"), name)
  currency <- db_currency.name_column (data.table::as.data.table(currency), c("date"), name)

  return(currency)
}

#' Title
#'
#' @param dt
#' @param to_keep
#' @param name
#'
#' @return
db_currency.name_column <- function(dt, to_keep, name) {

  suppressWarnings(dt[ , c("name") := NULL])
  old_names <- c(to_keep, tolower(name))
  new_names <- c(to_keep, sapply(name, function(x) paste("close", tolower(x), sep = "_")))
  setnames(x = dt, old = old_names, new = new_names)

  dt
}

#' Save Currency into DB.
#'
#' @param name the name of the Currency (e.g. EUR_CHF, EUR_USD)
#' @param currency  \code{data.table} containing the Currency to save.
#'
#' @return Nothing.
#' @export
db_currency.save <- function(name, currency) {

  currency$name = name

  db_utils.save(currency, "ref_currency", name)
}


#' Update one specified Currency into DB if exists.
#'
#' @param name the name of the Currency (e.g. EUR_CHF, EUR_USD)
#' @param to_update_currency the \code{data.table} containing the Currency to update.
#'
#' @return Nothing.
#'
#' @export
db_currency.update <- function(name, to_update_currency) {

  to_update_currency$name <- name
  setkey(to_update_currency, date)

  db_utils.update(to_update_currency, "ref_currency", name)
}


#' Delete one specified Currency from DB if exists.
#'
#' @param name the name of the Currency (e.g. EUR_CHF, EUR_USD)
#' @param to_delete_currency the \code{data.table} containing the Currency to delete.
#'
#' @return Nothing.
#'
#' @export
db_currency.delete <- function(name, to_delete_currency) {

  name <- paste(name, period, sep = "_")
  db_utils.delete(to_delete_currency, "ref_currency", name)
}
