
#' Retrieve all possible forecasting dates for this asset.
#'
#' @param asset_code a character vector indicating the asset. code.
#' @param start_date the date when forecasting starts.
#' @param end_date the date when forecasting ends.
#'
#' @return a vector containing the forecasting dates.
#'
#' @import data.table
#' @export
data_tools.get_forecasting_dates <- function(asset_code, start_date, end_date) {

  indice_dates <- data_file.load_fchi()
  indice_dates <- indice_dates[date >= start_date & date <= end_date, .(date)]
  currency_dates <- data_file.load_currencies()
  currency_dates <- currency_dates[date >= start_date & date <= end_date, .(date)]
  vix_dates <- data_file.load_vix()
  vix_dates <- vix_dates[date >= start_date & date <= end_date, .(date)]
  vxd_dates <- data_file.load_vxd()
  vxd_dates <- vxd_dates[date >= start_date & date <= end_date, .(date)]

  indice_dates[currency_dates[vix_dates[vxd_dates, nomatch = 0], nomatch = 0], nomatch = 0][, date]
}

#' Retrieve all possible daily forecasting dates for this asset.
#'
#' @param asset_code a character vector indicating the asset. code.
#' @param last_date the last date of existing forecasts.
#'
#' @return a vector containing the daily forecasting dates.
#'
#' @import data.table
#' @export
data_tools.get_daily_forecasting_dates <- function(asset_code, last_date) {

  indice_dates <- data_file.load_fchi()
  indice_dates <- indice_dates[date > last_date, .(date)]
  currency_dates <- data_file.load_currencies()
  currency_dates <- currency_dates[date > last_date, .(date)]
  vix_dates <- data_file.load_vix()
  vix_dates <- vix_dates[date > last_date, .(date)]
  vxd_dates <- data_file.load_vxd()
  vxd_dates <- vxd_dates[date > last_date, .(date)]

  indice_dates[currency_dates[vix_dates[vxd_dates, nomatch = 0], nomatch = 0], nomatch = 0][, date]

}

#' Loads all data needed for forecasting.
#'
#' @param indice_code a character vector indicating the indice code on which forecast will be done.
#' @param day_date a character vector indicating the date of the day on which forecast will be done.
#' @param geometric_point_t1 a boolean indicating if geometric point of type 1 should be computed
#' @param geometric_point_t2 a boolean indicating if geometric point of type 2 should be computed
#' @param delta_point a boolean indicating if delta point should be computed
#' @param dynamic_flag a boolean indicating if dynamic flag should be computed
#'
#' @return a list of \code{data.table} containing all useful data for the forecasting process.
#'
#' @export
data_tools.load_raw_data <- function(indice_code, day_date,
                                     geometric_point_t1 = F, geometric_point_t2 = F,
                                     delta_point = T, dynamic_flag = T) {

  raw_data <- list()

  # From DB
  raw_data$indice_data <- data_file.load_fchi()[date <= day_date, ]
  raw_data$currency_data <- data_file.load_currencies()[date <= day_date, ]
  raw_data$vix_data <- data_file.load_vix()[date <= day_date, ]
  raw_data$vxd_data <- data_file.load_vxd()[date <= day_date, ]

  if(geometric_point_t1) raw_data$geo_pt_t1_data <- db_geo_pt_t1.load(indice_code)[date <= day_date, ]
  if(geometric_point_t2) raw_data$geo_pt_t2_data <- db_geo_pt_t2.load(indice_code)[date <= day_date, ]
  if(delta_point) raw_data$delta_pt_data <- delta_point.build(raw_data$indice_data[date <= day_date, ])

  # Computed : build all
  if(dynamic_flag) {
    list_flag_r_data <- flag_r.build(raw_data$indice_data)
    raw_data$flag_r_data <- list_flag_r_data$value
    raw_data$ttt_data <- list_flag_r_data$ttt
  } else {
    raw_data$flag_r_data <- db_flag.load(indice_code)[date <= day_date, ]
  }

  raw_data$indicator_technic_data <- indicator_technic.get(indice_code, day_date)
  raw_data$euribor_pca_data <- euribor.get_pca(day_date)

  return(raw_data)
}


#' Update previously computed data used for forecasting with new day_date data.
#'
#' @param raw_data raw data initially coming from \code{data_tools.load_raw_data}
#' @param indice_code a character vector indicating the indice code on which forecast will be done.
#' @param day_date a character vector indicating the date of the day on which forecast will be done.
#' @param geometric_point_t1 a boolean indicating if geometric point of type 1 should be computed
#' @param geometric_point_t2 a boolean indicating if geometric point of type 2 should be computed
#' @param delta_point a boolean indicating if delta point should be computed
#' @param dynamic_flag a boolean indicating if dynamic flag should be computed
#'
#' @return a list of \code{data.table} containing all useful data for the forecasting process.
#'
#' @export
data_tools.update_raw_data <- function(raw_data, indice_code, day_date,
                                       geometric_point_t1 = F, geometric_point_t2 = F,
                                       delta_point = T, dynamic_flag = T) {

  # From DB
  raw_data$indice_data <- data_file.load_fchi()[date <= day_date, ]
  raw_data$currency_data <- data_file.load_currencies()[date <= day_date, ]
  raw_data$vix_data <- data_file.load_vix()[date <= day_date, ]
  raw_data$vxd_data <- data_file.load_vxd()[date <= day_date, ]

  if(geometric_point_t1) raw_data$geo_pt_t1_data <- db_geo_pt_t1.load(indice_code)[date <= day_date, ]
  if(geometric_point_t2) raw_data$geo_pt_t2_data <- db_geo_pt_t2.load(indice_code)[date <= day_date, ]
  if(delta_point) raw_data$delta_pt_data <- delta_point.build(raw_data$indice_data[date <= day_date, ])

  # Computed : update
  if(dynamic_flag) {
    ttt_data <- flag_r.update_daily(raw_data$ttt_data, raw_data$indice_data[date == day_date, ])
    raw_data$ttt_data <- ttt_data
    raw_data$flag_r_data <- flag_r.extract(ttt_data)
  } else {
    raw_data$flag_r_data <- db_flag.load(indice_code)[date <= day_date, ]
  }

  raw_data$indicator_technic_data <- indicator_technic.get(indice_code, day_date)
  raw_data$euribor_pca_data <- euribor.get_pca(day_date)

  return(raw_data)
}



#' Flats raw data by selecting good known flag r and define Target variable.
#'
#' @param raw_data raw data initially coming from \code{data_tools.load_raw_data}
#' @param level the level to select on
#' @param day_date the date of the day from where the flags r are computed
#' @param start_flat_raw_data the date from where the data are returned
#'
#' @return a data.table containing the computed data
#' @import data.table
#' @export
data_tools.flat_raw_data <- function(raw_data, level, day_date, start_flat_raw_data) {

  tmp_data <- technical_tools.dynamic_merge(raw_data, exclude = "ttt_data")

  tmp_data[, Target := eval(as.symbol(paste0("Trend_", level, "pc")))]
  suppressWarnings(tmp_data[, eval(paste0("Trend_", level, "pc")) := NULL])

  if(!is.null(raw_data$ttt_data)) {
    flag_ok <- paste("flag", level, "ok", sep="_")
    date_ok <- raw_data$ttt_data[[flag_r.get_levels_index(level)]][ date >= start_flat_raw_data
                                               & eval(as.symbol(flag_ok)) == TRUE, date ]
    date_ok <- unique(c(date_ok, day_date))

    return(tmp_data[date %in% date_ok, ])
  } else {

    return(tmp_data[date >= start_flat_raw_data, ])
  }
}


#' Flats and re-balances raw data by selecting good known flag r and define Target variable.
#'
#' @param raw_data raw data initially coming from \code{data_tools.load_raw_data}
#' @param level the level to re-balance on
#' @param day_date the date of the day from where the flags r are computed
#' @param start_flat_raw_data the date from where the data are returned
#' @param n the number of samples for each target
#'
#' @return a data.table containing the computed data
#' @import data.table
#' @export
data_tools.flat_rebalance_raw_data <- function(raw_data, level, day_date, start_flat_raw_data, n) {

  tmp_data <- technical_tools.dynamic_merge(raw_data, exclude = "ttt_data")

  tmp_data[, Target := eval(as.symbol(paste0("Trend_", level, "pc")))]
  suppressWarnings(tmp_data[, eval(paste0("Trend_", level, "pc")) := NULL])

  result <- data.table()
  if(!is.null(raw_data$ttt_data)) {
    flag_ok <- paste("flag", level, "ok", sep="_")
    date_ok <- raw_data$ttt_data[[flag_r.get_levels_index(level)]][ date >= start_flat_raw_data
                                                                    & eval(as.symbol(flag_ok)) == TRUE, date ]
    date_ok <- unique(c(date_ok, day_date))

    result <- rbindlist(list(
      result,
      tail(tmp_data[date %in% date_ok & Target == -level, ], n = n),
      tail(tmp_data[date %in% date_ok & Target == level, ], n = n),
      tail(tmp_data[date %in% date_ok & Target == -constant.NEUTRAL_VALUE, ], n = n),
      tail(tmp_data[date %in% date_ok & Target == constant.NEUTRAL_VALUE, ], n = n)))

  } else {

    result <- rbindlist(list(
      result,
      tail(tmp_data[date >= start_flat_raw_data & Target == -level, ], n = n),
      tail(tmp_data[date >= start_flat_raw_data & Target == level, ], n = n),
      tail(tmp_data[date >= start_flat_raw_data & Target == -constant.NEUTRAL_VALUE, ], n = n),
      tail(tmp_data[date >= start_flat_raw_data & Target == constant.NEUTRAL_VALUE, ], n = n)))
  }

  return(result)
}



#' Filters non numerical columns
#'
#' @param x the \code{data.table} to filter.
#'
#' @import data.table
#' @export
data_tools.filter_nonnumeric <- function(x) {
  nums <- sapply(x, is.numeric)
  y <- x[ , ..nums]
  suppressWarnings(y[, date := NULL])
}


#' Filters \code{Trend} and Target columns
#'
#' @param x the \code{data.table} to filter.
#'
#' @return the \code{data.table} filtered from all \code{Trend}.
#' @export
data_tools.filter_trend <- function(x){
  modelVariables <- colnames(x)
  #variablesToremove <- grep(x = modelVariables, pattern = "Trend", value = TRUE)
  variablesToremove <- c(sapply(c(2:7), FUN = function(i) paste0("Trend_", i, "pc")), "Target")
  modelVariables <- modelVariables [! modelVariables %in% variablesToremove]
  x[, ..modelVariables]
}


#' Filters \code{Trend} column of specific level.
#'
#' @param x  the \code{data.table} to filter.
#' @param level the level to filter.
#'
#' @return the \code{data.table} filtered from \code{Trend} of \code{level}.
#' @export
data_tools.filter_trend_by_level <- function(x, level){
  modelVariables <- colnames(x)
  variablesToremove <- grep(x = modelVariables, pattern = paste0("Trend_", level), value = TRUE)
  modelVariables <- modelVariables [! modelVariables %in% variablesToremove]
  x[, ..modelVariables]
}


#' Builds two-class \code{Factor} target
#'
#' @param x the \code{data.table} containing the target classes.
#' @param order the meaningful order type (e.g. Constants.BUY, Constants.SELL, Constants.NEUTRAL)
#'
#' @return the two-class \code{Factor} target
#' @export
data_tools.get_target <- function(x, order){

  #TARGETS = list( c("N", "P"), c("P", "N"), c("P", "N"), c("N", "P"), c("P", "N"))

  return(base::factor(x[, Target], labels = c("N", "P")))

}


#' Transforms flat data into sample ones.
#'
#' @param x the \code{data.table} to transform.
#' @param order the meaningful order type (e.g. Constants.ACHAT, Constants.VENTE, Constants.NEUTRE).
#' @param level the level to consider.
#' @param with_geo_pt_t2 TRUE if Geometric Points T2 are used.
#'
#' @return a \code{data.table} containing the sample data.
#'
#' @export
data_tools.transform_to_sample <- function(x, order, level, with_geo_pt_t2){

  x <- data_tools.transform_target(x, order, level)

  if(with_geo_pt_t2) {

    if(order == Constants.ACHAT) { order_type = "a" }
    if(order == Constants.VENTE) { order_type = "v" }

    model_variables <- colnames(x)

    variables_geometric <- grep(x = model_variables, pattern = "GEOT2", value = TRUE)
    variable_to_remove <- grep(x = variables_geometric, pattern = paste("GEOT2", order_type, level, sep = "_"),
                               value = TRUE, invert = TRUE)

    model_variables <- model_variables [! model_variables %in% variable_to_remove]

    return(x[, ..model_variables])

  } else {

    return(x)
  }
}

#' Transform Target to a 2-class factor depending on level and order.
#'
#' @param x the \code{data.table} containing the target to transform.
#' @param order the type of order (see \code{constant})
#' @param level the level to consider.
#'
#' @return
#' @export
data_tools.transform_target <- function(x, order, level){
  y <- copy(x)

  if (order %in% c(constant.BUY, constant.BUY_OR_NEUTRAL, constant.BUY_IN_BUY_OR_SELL)) {
    y[Target %in% c(-level, -constant.NEUTRAL_VALUE, +constant.NEUTRAL_VALUE), Target := 0]
    y[Target == level, Target := 1]
  }
  else if (order %in% c(constant.SELL, constant.SELL_OR_NEUTRAL, constant.SELL_IN_BUY_OR_SELL)) {
    y[Target %in% c(-constant.NEUTRAL_VALUE, +constant.NEUTRAL_VALUE, +level), Target := 0]
    y[Target == -level, Target := 1]
  }
  else if (order %in% c(constant.NEUTRAL, constant.NEUTRAL_OR_NOT)) {
    y[Target %in% c(-level, +level), Target := 0]
    y[Target == +constant.NEUTRAL_VALUE, Target := 1]
  }
  else if (order %in% c(constant.NEUTRAL_SELL)) {
    y[Target %in% c(-level, +constant.NEUTRAL_VALUE, +level), Target := 0]
    y[Target == -constant.NEUTRAL_VALUE, Target := 1]
  }
  else if (order %in% c(constant.NEUTRAL_BUY)) {
    y[Target %in% c(-level, -constant.NEUTRAL_VALUE, +level), Target := 0]
    y[Target == +constant.NEUTRAL_VALUE, Target := 1]
  }

  return(y)
}


# data_tools.target_to_factor <- function(learning_data, order, level){
#
#   learning_data[, Target := as.factor(Target)]
# }


#' Re-balances target on learning data
#'
#' @param learningData the data to re-balance
#' @param level the level of the target
#'
#' @return
#' @export
BalanceMinority <- function(learningData, level) {

  N.level = nrow(learningData[Target == level])
  N.neutre = nrow(learningData[Target == 0])

  if (N.level < N.neutre) {
    return(c(level, 0))
  }
  else {
    return(c(0, level))
  }
}


#' Re-balances target on learning data with \code{unbalanced::ubBalance}
#'
#' @param learningData the data to re-balance
#' @param level the level of the target
#' @param type the balancing technique to use (ubOver, ubUnder, ubSMOTE, ubOSS, ubCNN, ubENN, ubNCL, ubTomek).
#'
#' @return
#' @import data.table
#' @importFrom unbalanced ubBalance
ReBalanceData <- function(learningData, level, type = "ubSMOTE"){

  order_level <- BalanceMinority(learningData, level)
  learningData[, Target := factor(Target, levels = order_level, labels = c(2, 1))]


  Target_pos <- which(colnames(learningData) == "Target")
  input <- learningData[ , -Target_pos, with=F]
  output <- learningData[, Target]

  data <- unbalanced::ubBalance(X = as.data.frame(input), Y = output, type = type, positive = 2)

  balanced_data <- data.table(data$X, Target=data$Y)
  balanced_data$Target <- factor(balanced_data[, Target], levels = c(2, 1), labels = order_level)

  return(balanced_data)
}


#' Samples data to filter forecast.
#'
#' @param flat_data the data to sample
#' @param forecast_data the data from to sample
#' @param order
#' @param level
#' @param day_date
#'
#' @return
#' @export
data_tools.sample <- function(flat_data, forecast_data, order, level, day_date){
  flat <- copy(flat_data)
  forecast <- copy(forecast_data)

  #SAMPLES <- list( c(-1, +1, +level), c(-1, +1, -level), c(-1, +1), c(-level, +level), c(-level, +level) )
  SAMPLES <- list( c(0, +level), c(0, -level), c(0), c(-level, +level), c(-level, +level) )
  sample_dates <- unique(c(forecast[f %in% SAMPLES[[order]], d], day_date))

  return(flat[date %in% sample_dates, ])
}

# Transform data depending on their lambda (center, scale, BoxCox, YeoJohnson, Log, inverse,...)
#' @param x the \code{data.table} to transform.
#'
#' @param selected_variables
#' @param to_scale
#'
#' @export
data_tools.transform <- function(x, selected_variables, to_scale = TRUE) {

  to_bind_data <- x[, .(date, Target)]
  to_transform_data <- x[, ..selected_variables]

  if(to_scale) {
    transform_params <- preProcess(to_transform_data, method=c("center", "scale", "YeoJohnson")) # BoxCox
    transformed_data <- predict(transform_params, to_transform_data)

    return(cbind(to_bind_data, transformed_data))

  } else {

    return(cbind(to_bind_data, to_transform_data))
  }
}

# Transform data depending on their lambda (center, scale, BoxCox, YeoJohnson, Log, inverse,...)
#' @param x the \code{data.table} to transform.
#'
#' @param level
#' @param selected_variables
#'
#' @export
data_tools.transform_by_level <- function(x, level, selected_variables) {

  to_bind_data <- x[, .(date, eval(as.symbol(paste0("Trend_", level, "pc"))))]
  colnames(to_bind_data) <- c("date", paste0("Trend_", level, "pc"))
  to_transform_data <- x[, ..selected_variables]

  transform_params <- preProcess(to_transform_data, method=c("center", "scale", "YeoJohnson")) # BoxCox
  transformed_data <- predict(transform_params, to_transform_data)

  cbind(to_bind_data, transformed_data)
}


#' Title
#'
#' @param learning_data
#' @param order
#' @param level
#'
#' @return
#' @export
data_tools.target_to_factor <- function(learning_data, order, level){

  learning_data[, Target := data_tools.get_target(learning_data, order)]
}

#' Extracts the learning data depending on the forecast date
#'
#' @param x the \code{data.table} containing the learning data
#'
#' @param jour_date
#' @param level
#'
#' @export
data_tools.extract_learning_data <- function(x, jour_date, level){

  y <- x[date < jour_date, ]

  # target_symbol <- rlang::sym(paste0("Trend_", level, "pc"))
  # y[, Target := eval(target_symbol)]

  to_remove_variables <- c("date")
  variables <- colnames(y)
  to_keep_variables <- variables[!variables %in% to_remove_variables]

  y[, ..to_keep_variables]
}

#' Extracts the learning data depending on the forecast date and the level.
#'
#' @param x the \code{data.table} containing the learning data
#'
#' @param jour_date
#' @param level
#'
#' @export
data_tools.extract_learning_data_by_level <- function(x, jour_date, level){

  y <- x[date < jour_date, ]

  # target_symbol <- rlang::sym(paste0("Trend_", level, "pc"))
  # y[, Target := eval(target_symbol)]

  to_remove_variables <- c("date", sapply(c(2:7), FUN = function(i) paste0("Trend_", i, "pc")))
  variables <- colnames(y)
  to_keep_variables <- variables[!variables %in% to_remove_variables]

  y[, ..to_keep_variables]
}

#' Extracts the forecasting data depending on the forecast date
#'
#' @param x the \code{data.table} containing the forecasting data
#'
#' @param jour_date
#'
#' @export
data_tools.extract_forecasting_data <- function(x, jour_date){
  y <- x[date == jour_date, ]

  to_remove_variables <- c("date", "Target")
  variables <- colnames(y)
  to_keep_variables <- variables[!variables %in% to_remove_variables]

  y[, ..to_keep_variables]
}

#' Extracts the forecasting data depending on the forecast date and the level.
#'
#' @param x the \code{data.table} containing the forecasting data
#' @param jour_date
#' @param level
#'
#' @export
data_tools.extract_forecasting_data_by_level <- function(x, jour_date, level){
  y <- x[date == jour_date, ]

  to_remove_variables <- c("date", grep(paste0("Trend_", level), colnames(y), value = TRUE))
  variables <- colnames(y)
  to_keep_variables <- variables[!variables %in% to_remove_variables]

  y[, ..to_keep_variables]
}
