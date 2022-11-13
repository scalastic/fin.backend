

#' Load currencies data from CSV file
#'
#' @return a data.table containing the currencies
#' @importFrom readr read_delim cols col_date col_double col_skip locale
#' @import data.table
#' @importFrom stats na.omit
#' @export
#' @example data_file.load_currencies()
data_file.load_currencies <- function() {

  suppressWarnings(
  data_list <- readr::read_delim(system.file("testdata/HISTORICAL_CURRENCY_RATES.csv", package = "fin.backend"),
                                        delim = ";", escape_double = FALSE, col_types = cols(
                                          `Titre :` = col_date(format = "%d/%m/%Y"),
                                          `Dollar australien (AUD)` = col_double(),
                                          `Lev bulgare (BGN)` = col_skip(),
                                          `Real brésilien (BRL)` = col_skip(),
                                          `Dollar canadien (CAD)` = col_double(),
                                          `Franc suisse (CHF)` = col_double(),
                                          `Yuan renminbi chinois (CNY)` = col_double(),
                                          `Livre chypriote (CYP)` = col_skip(),
                                          `Couronne tchèque (CZK)` = col_skip(),
                                          `Couronne danoise (DKK)` = col_skip(),
                                          `Couronne estonienne (EEK)` = col_skip(),
                                          `Livre sterling (GBP)` = col_double(),
                                          `Dollar de Hong Kong (HKD)` = col_skip(),
                                          `Kuna croate (HRK)` = col_skip(),
                                          `Forint hongrois (HUF)` = col_skip(),
                                          `Roupie indonésienne (IDR)` = col_skip(),
                                          `Sheqel israélien (ILS)` = col_skip(),
                                          `Roupie Indienne (100 paise)` = col_double(),
                                          `Couronne islandaise (ISK)` = col_skip(),
                                          `Yen japonais (JPY)` = col_double(),
                                          `Won coréen (KRW)` = col_skip(),
                                          `Litas lituanien (LTL)` = col_skip(),
                                          `Lats letton (LVL)` = col_skip(),
                                          `Livre maltaise (MTL)` = col_skip(),
                                          `Peso méxicain (MXN)` = col_skip(),
                                          `Ringgit malaisien (MYR)` = col_skip(),
                                          `Couronne norvégienne (NOK)` = col_skip(),
                                          `Dollar neo-zélandais (NZD)` = col_skip(),
                                          `Peso philippin (PHP)` = col_skip(),
                                          `Zloty polonais (PLN)` = col_skip(),
                                          `Leu roumain (RON)` = col_skip(),
                                          `Rouble russe (RUB)` = col_skip(),
                                          `Couronne suédoise (SEK)` = col_skip(),
                                          `Dollar de Singapour (SGD)` = col_skip(),
                                          `Tolar slovène (SIT)` = col_skip(),
                                          `Couronne slovaque (SKK)` = col_skip(),
                                          `Baht thaïlandais (THB)` = col_skip(),
                                          `Livre turque (TRY)` = col_skip(),
                                          `Dollar des Etats-Unis (USD)` = col_double(),
                                          `Rand sud-africain (ZAR)` = col_skip()
                                        ),
                                        locale = locale(date_names = "fr", decimal_mark = ","),
                                        trim_ws = TRUE)
  )

  data_dt <- na.omit(setDT(data_list, check.names = TRUE))
  data_dt <- setnames(data_dt, c("date", "aud", "cad", "chf", "cny", "gbp", "inr", "jpy", "usd"))
  data_dt <- data_dt[, date := format(date, "%Y-%m-%d")]

  setkey (data_dt, date)

}

#' Load FCHI (CAC40) indice from CSV file
#'
#' @return a data.table containing the indice's values
#' @importFrom readr read_delim
#' @export
data_file.load_fchi <- function() {

  suppressWarnings(
  data_list <- readr::read_delim(system.file("testdata/HISTORICAL_FCHI.csv", package = "fin.backend"),
                              col_types = cols(
                                Date = col_date(format = "%Y-%m-%d"),
                                Open = col_double(),
                                High = col_double(),
                                Low = col_double(),
                                Close = col_skip(),
                                `Adj Close` = col_double(),
                                Volume = col_double()
                              ),
                              na = "NA")
  )

  data_dt <- na.omit(setDT(data_list, check.names = TRUE))
  data_dt <- setnames(data_dt, c("date", "open", "high", "low", "close", "volume"))
  data_dt_new <- data_dt[, date := as.character(date)]
  data_dt_new <- setkey(data_dt_new, date)

  data_dt_new[data_dt_new$volume > 0,]
}

#' Load EURIBOR values from CSV file
#'
#' @return a data.table containing the EURIBOR's values
#' @importFrom readr read_delim
#' @export
data_file.load_euribor <- function() {

  suppressWarnings(
  data_csv <-   data_list <- readr::read_delim( system.file("testdata/HISTORICAL_INTERBANK_RATES.csv", package = "fin.backend"),
                          delim = ";", escape_double = FALSE, col_types = cols(
                            `Titre :` = col_date(format = "%d/%m/%Y"),
                            `EURIBOR à 1 mois` = col_double(),
                            `EURIBOR à 1 semaine` = col_double(),
                            `EURIBOR à 12 mois` = col_double(),
                            `EURIBOR à 3 mois` = col_double(),
                            `EURIBOR à 6 mois` = col_double(),
                            `€STR - Méthode de calcul` = col_skip(),
                            `€STR - Nombre de banques` = col_skip(),
                            `€STR - Nombre de transactions` = col_skip(),
                            `€STR - Taux au 25ème percentile des volumes` = col_skip(),
                            `€STR - Taux au 75ème percentile des volumes` = col_skip(),
                            `€STR - Publication` = col_skip(),
                            `€STR - Volume total` = col_skip(),
                            `€STR - Part des 5 plus grandes banques` = col_skip(),
                            `€STR - Taux moyen ajusté pondéré en fonction du volume` = col_skip()
                          ),
                          locale = locale(date_names = "fr", decimal_mark = ","),
                          na = "NA", trim_ws = TRUE )
  )

  data_dt <- na.omit(setDT(data_list, check.names = TRUE))
  data_dt <- setnames(data_dt, c("date", "1m", "1s", "12m", "3m", "6m"))
  data_dt <- data_dt[, date := format(date, "%Y-%m-%d")]
  setkey (data_dt, date)
}

#' Load VIX values from CSV file
#'
#' @return a data.table containing the VIX's values
#' @importFrom readr read_delim
#' @importFrom stats na.omit
#' @export
data_file.load_vix <- function() {

  suppressWarnings(
  data_list <- readr::read_delim(system.file("testdata/HISTORICAL_VIX.csv", package = "fin.backend"),
                        col_types = cols(Date = col_date(format = "%m/%d/%Y"),
                                         Close = col_skip(),
                                         Volume = col_skip()))
  )

  data_dt <- na.omit(setDT(data_list, check.names = TRUE))
  data_dt <- setnames(data_dt, c("date", "open", "high", "low", "close"))
  data_dt <- data_dt[, date:=as.character(parse_date(date, format="%m/%d/%Y"))]
  setkey (data_dt, date)
}

#' Load VXD values from CSV file
#'
#' @return a data.table containing the VXD's values
#' @importFrom readr read_delim
#' @importFrom stats na.omit
#' @export
data_file.load_vxd <- function() {

  suppressWarnings(
  data_list <- readr::read_delim(system.file("testdata/HISTORICAL_VXD.csv", package = "fin.backend"),
                        col_types = cols(DATE = col_date(format = "%m/%d/%Y")))
  )

  data_dt <- na.omit(setDT(data_list, check.names = TRUE))
  data_dt <- setnames(data_dt, c("date", "open", "high", "low", "close"))
  #data_dt <- data_dt[, date:=as.character(parse_date(date, format="%m/%d/%Y"))]
  setkey (data_dt, date)
}

