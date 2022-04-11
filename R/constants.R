Constants.Core <- 4

Constants.VERBOSE = FALSE


Constants.LAST_DATE_PREVISION = "2000-01-01"

#
# Previsions parameters
#
Constants.PREV_RATIO_STOPLOSS <- 1/5

#
# Neutral Filter parameters
#
Constants.NEUTRAL_FILTER_RATIO_QDA <- 0.99

#
# Performance parameters
#
Constants.PERF_RATIO_CAPITAL <- 1/2
Constants.PERF_RATIO_STOPLOSS <- 1/5

#
#
#'
#' @export

Constants.ACHAT <- {
  .Deprecated(new = "constant.BUY", package = "fin.backend", old = "Constants.ACHAT", msg = "Constants.ACHAT is deprecated, use constant.BUY instead.")
  1
}
#' @export
Constants.VENTE <- {
  .Deprecated(new = "constant.SELL", package = "fin.backend", old = "Constants.VENTE", msg = "Constants.VENTE is deprecated, use constant.SELL instead.")
  -1
}
#' @export
Constants.NEUTRE <- {
  .Deprecated(new = "constant.NEUTRAL", package = "fin.backend", old = "Constants.NEUTRE", msg = "Constants.NEUTRE is deprecated, use constant.NEUTRAL instead.")
  -1
}


#' @export
constant.ORDER_LABELS <- c("BUY", "SELL", "NEUTRAL", "NEUTRAL_SELL", "NEUTRAL_BUY", "BUY_IN_BUY_SELL", "SELL_IN_BUY_SELL")
#' @export
constant.BUY_OR_NEUTRAL = 1
#' @export
constant.BUY = 1
#' @export
constant.SELL_OR_NEUTRAL = 2
#' @export
constant.SELL = 2
#' @export
constant.NEUTRAL_OR_NOT = 3
#' @export
constant.NEUTRAL = 3
#' @export
constant.NEUTRAL_SELL = 4
#' @export
constant.NEUTRAL_BUY = 5
#' @export
constant.BUY_IN_BUY_OR_SELL = 6
#' @export
constant.SELL_IN_BUY_OR_SELL = 7
#' @export
constant.ORDER_VALUES <- c(constant.BUY_OR_NEUTRAL, constant.SELL_OR_NEUTRAL, constant.NEUTRAL_OR_NOT, constant.NEUTRAL_SELL, constant.NEUTRAL_BUY, constant.BUY_IN_BUY_OR_SELL, constant.SELL_IN_BUY_OR_SELL)
#' @export
constant.get_order_label <- function(value) {
  return(constant.ORDER_LABELS[value])
}

#' @export
constant.NEUTRAL_VALUE <- 0.01

#
# Algo Type
#
Constants.PREV_INDIVIDUAL = "INDIVIDUAL"

#
# Algo Type
#
Constants.ALGO_QDA = "QDA"
Constants.ALGO_LDA = "LDA"
Constants.ALGO_DT = "DT"
Constants.ALGO_SVM = "SVM"
Constants.ALGO_KHN = "KHN"
Constants.ALGO_BAY = "BAY"
Constants.ALGO_BOO = "BOO"
Constants.ALGO_RF = "RF"

#
# Formula Type
#
Constants.FORMULA_SIMPLE = "FS"
Constants.FORMULA_CARRE = "FC"
Constants.FORMULA_CARRE2 = "FC2"
Constants.FORMULA_LOG = "FL"
Constants.FORMULA_LOG2 = "FL2"
Constants.FORMULA_SQRT = "SQ"

#
# Data Transformation Type
#
Constants.TRANSFORM_SKEWNESS = "SK"

#
# Flag Type
#
Constants.FLAG_REAL = "R"
Constants.FLAG_ZIGZAG = "Z"
Constants.FLAG_EXTENDED = "X"
Constants.FLAG_FILTERED = "F"
Constants.FLAG_REBALANCE = "BAL"


Constants.FLAG_REAL_EXTENDED = "RX"
Constants.FLAG_ZIGZAG_EXTENDED = "ZX"

Constants.FLAG_REAL_QDA = paste0(Constants.FLAG_REAL, Constants.ALGO_QDA)
Constants.FLAG_ZIGZAG_QDA = paste0(Constants.FLAG_ZIGZAG, Constants.ALGO_QDA)

Constants.FLAG_REAL_EXTENDED_FILTERED_QDA = paste0(Constants.FLAG_REAL_EXTENDED, Constants.FLAG_FILTERED, Constants.ALGO_QDA)
Constants.FLAG_ZIGZAG_EXTENDED_FILTERED_QDA = paste0(Constants.FLAG_ZIGZAG_EXTENDED, Constants.FLAG_FILTERED, Constants.ALGO_QDA)
