

#' Title
#'
#' @return
#' @export
init_db.import_data <- function() {

  # FCHI = CAC40
  dt_fchi <- data_file.load_fchi()
  db_asset.save(name="CAC_40", dt_fchi)

  # Currencies
  dt_currencies <- data_file.load_currencies()
  db_currency.save(name = "AUD", currency = dt_currencies[, .(date, aud)])
  db_currency.save(name = "CAD", currency = dt_currencies[, .(date, cad)])
  db_currency.save(name = "CHF", currency = dt_currencies[, .(date, chf)])
  db_currency.save(name = "CNY", currency = dt_currencies[, .(date, cny)])
  db_currency.save(name = "GBP", currency = dt_currencies[, .(date, gbp)])
  db_currency.save(name = "INR", currency = dt_currencies[, .(date, inr)])
  db_currency.save(name = "JPY", currency = dt_currencies[, .(date, jpy)])
  db_currency.save(name = "USD", currency = dt_currencies[, .(date, usd)])

  #EURIBOR
  dt_euribor <- data_file.load_euribor()
  dt_euribor[ , value:=`1s`]
  db_ibor.save(name = "EURIBOR", period = "S1", ibor = dt_euribor[ , .(date, value)])
  dt_euribor[ , value:=`1m`]
  db_ibor.save(name = "EURIBOR", period = "M1", ibor = dt_euribor[ , .(date, value)])
  dt_euribor[ , value:=`3m`]
  db_ibor.save(name = "EURIBOR", period = "M3", ibor = dt_euribor[ , .(date, value)])
  dt_euribor[ , value:=`6m`]
  db_ibor.save(name = "EURIBOR", period = "M6", ibor = dt_euribor[ , .(date, value)])
  dt_euribor[ , value:=`12m`]
  db_ibor.save(name = "EURIBOR", period = "M12", ibor = dt_euribor[ , .(date, value)])

  # VIX
  dt_vix <- data_file.load_vix()
  db_volatility.save("VIX", dt_vix)
  # VXD
  dt_vxd <- data_file.load_vxd()
  db_volatility.save("VXD", dt_vxd)

}

