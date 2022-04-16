
#' Technical Indicator
#'
#' @name indicator_technic.get
#'
#' @description This function computes all technicals indicators.
#'
#' @param indice_code a character vector indicating the indice code on which computation will be done.
#' @param day_date a character vector indicating the date of the day until which computation will be done.
#' @return a data.table containing the technical indicators.
#'
#' @import data.table
#' @import tm
#' @import NLP
#' @import TTR
#' @export
indicator_technic.get <- function(indice_code, day_date) {

  dt <- data_file.load_fchi()[date <= day_date, ]

  # Lancement des calculs

  #---------------------------------------
  # Calcul du Return (specific)
  #---------------------------------------
  label <- "Return"
  result <- diff(dt[,close], lag = 1, differences = 1)
  TI_RETURN <- c(NA,result)

  #---------------------------------------
  # Calcul du Directional Movement Index
  #---------------------------------------
  label <- "DMI"
  result <- ADX(dt[,.(high,low,close)], n=14)
  colnames(result) <- c("DMI_dip", "DMI_din", "DMI_dx", "DMI_adx")
  TI_DMI <- result

  label <- "DMI7"
  result <- ADX(dt[,.(high,low,close)], n=7)
  colnames(result) <- c("DMI_dip7", "DMI_din7", "DMI_dx7", "DMI_adx7")
  TI_DMI7 <- result

  label <- "DMI21"
  result <- ADX(dt[,.(high,low,close)], n=21)
  colnames(result) <- c("DMI_dip21", "DMI_din21", "DMI_dx21", "DMI_adx21")
  TI_DMI21 <- result

  #-----------------------------------------------------------
  # The Aroon indicator attempts to identify starting trends.
  #-----------------------------------------------------------
  label <- "aroon"
  result <- aroon(dt[,.(high,low)], n=20)
  colnames(result) <- c("AROON_up", "AROON_dn", "AROON_oscillator")
  TI_TREND <- result

  #-----------------------------------------------------------------------
  # True range (TR) is a measure of volatility of a High-Low-Close series
  #-----------------------------------------------------------------------
  label <- "ATR"
  result <- ATR(dt[,.(high,low,close)], n=14)
  colnames(result) <- c("ATR_tr", "ATR_atr", "ATR_truehigh", "ATR_truelow")
  TI_ATR <- result

  #-----------------------------------------------------------------------
  # Bollinger Bands are a way to compare a security's volatility
  # and price levels over a period of time.
  #-----------------------------------------------------------------------
  label <- "BBands"
  result <- BBands(dt[,.(high,low,close)])
  colnames(result) <- c("BBANDS_dn", "BBANDS_mavg", "BBANDS_up", "BBANDS_pctb")
  TI_BBANDS <- result

  #-----------------------------------------------------------------------
  # The Commodity Channel Index (CCI)
  # attempts to identify starting and ending trends.
  #-----------------------------------------------------------------------
  label <- "CCI"
  result <- CCI(dt[,.(high,low,close)])
  TI_CCI <- result

  #-----------------------------------------------------------------------
  # The Chaikin Accumulation / Distribution (AD) line is a measure
  # of the money flowing into or out of a security.
  #-----------------------------------------------------------------------
  label <- "chaikinAD"
  result <- chaikinAD(dt[,.(high,low,close)], dt[,volume])
  TI_CHAIKINAD <- result

  #-----------------------------------------------------------------------
  # Chaikin Volatility measures the rate of change of the
  # security's trading range.
  #-----------------------------------------------------------------------
  label <- "chaikinVolatility"
  result <- chaikinVolatility(dt[,.(high,low)])
  TI_CHAIKINVOLATILITY <- result

  #-----------------------------------------------------------------------
  # The Close Location Value (CLV) relates the day's
  # close to its trading range.
  #-----------------------------------------------------------------------
  label <- "CLV"
  result <- CLV(dt[,.(high,low,close)])
  TI_CLV <- result

  #-----------------------------------------------------------------------
  # Chaikin Money Flow compares total volume over the last
  # n time periods to total volume times the Close Location
  # Value (CLV) over the last n time periods.
  #-----------------------------------------------------------------------
  label <- "CMF"
  result <- CMF(dt[,.(high,low,close)], dt[,volume])
  TI_CMF <- result

  #-----------------------------------------------------------------------
  # The Chande Momentum Oscillator (CMO) is a modified RSI.
  #-----------------------------------------------------------------------
  label <- "CMO"
  result <- CMO(dt[,close])
  TI_CMO <- result

  #-----------------------------------------------------------------------
  # Donchian Channels were used to generate buy and sell
  # signals for the Turtle Trading system.
  # DonchianChannel(HL, n = 10, include.lag = FALSE)
  #-----------------------------------------------------------------------
  label <- "DonchianChannel"
  result <- DonchianChannel(dt[,.(high,low)])
  colnames(result) <- c("DONCHIANCHANNEL_high","DONCHIANCHANNEL_mid","DONCHIANCHANNEL_low")
  TI_DONCHIANCHANNEL <- result

  #-----------------------------------------------------------------------
  # The DV Intermediate oscillator (DVI) is a very smooth
  # momentum oscillator
  #-----------------------------------------------------------------------
  label <- "DVI"
  result <- DVI(dt[,close])
  colnames(result) <- c("DVI_mag", "DVI_str", "DVI_dvi")
  TI_DVI <- result

  #-----------------------------------------------------------------------
  # Calculate the Guppy Multiple Moving Average of a series.
  #-----------------------------------------------------------------------
  label <- "GMMA"
  result <- GMMA(dt[,close], short = c(3, 5, 8, 10, 12, 15, 20, 25), long = c(30, 35, 40, 45, 50, 60, 100, 150))
  colnames(result) <- c("GMMA_lag3", "GMMA_lag5", "GMMA_lag8", "GMMA_lag10", "GMMA_lag12", "GMMA_lag15", "GMMA_lag20", "GMMA_lag25", "GMMA_lag30", "GMMA_lag35", "GMMA_lag40", "GMMA_lag45", "GMMA_lag50", "GMMA_lag60", "GMMA_lag100", "GMMA_lag150")
  # result <- cbind(result, GMMA_diff_3_30 = result[,"GMMA_lag3"] - result[,"GMMA_lag30"])
  # result <- cbind(result, GMMA_diff_5_35 = result[,"GMMA_lag5"] - result[,"GMMA_lag35"])
  # result <- cbind(result, GMMA_diff_8_40 = result[,"GMMA_lag8"] - result[,"GMMA_lag40"])
  # result <- cbind(result, GMMA_diff_10_45 = result[,"GMMA_lag10"] - result[,"GMMA_lag45"])
  # result <- cbind(result, GMMA_diff_12_50 = result[,"GMMA_lag12"] - result[,"GMMA_lag50"])
  # result <- cbind(result, GMMA_diff_15_60 = result[,"GMMA_lag15"] - result[,"GMMA_lag60"])
  # result <- cbind(result, GMMA_diff_20_100 = result[,"GMMA_lag20"] - result[,"GMMA_lag100"])
  # result <- cbind(result, GMMA_diff_25_150 = result[,"GMMA_lag25"] - result[,"GMMA_lag150"])
  TI_GMMA <- result

  #-----------------------------------------------------------------------
  # The Know Sure Thing (KST) is a smooth, summed,
  # rate of change indicator.
  #-----------------------------------------------------------------------
  label <- "KST"
  result <- KST(dt[,close])
  colnames(result) <- c("KST_kst", "KST_signal")
  TI_KST <- result

  label <- "KST4MA"
  result <- KST(dt[,close], maType=list(list(SMA),list(EMA),list(DEMA),list(WMA)))
  colnames(result) <- c("KST4MA_kst", "KST4MA_signal")
  TI_KST4MA <- result

  #-----------------------------------------------------------------------
  # The MACD function compares a fast moving average (MA)
  # of a series with a slow MA of the same series.
  #-----------------------------------------------------------------------
  label <- "MACD1"
  result <- MACD(dt[,close], nFast = 12, nSlow = 26, nSig = 9, maType="EMA" )
  colnames(result) <- c("MACD1_macd", "MACD1_signal")
  TI_MACD1 <- result

  label <- "MACD2"
  result <- MACD(dt[,close], nFast = 12, nSlow = 26, nSig = 9, maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)))
  colnames(result) <- c("MACD2_macd", "MACD2_signal")
  TI_MACD2 <- result

  #-----------------------------------------------------------------------
  # The MFI is a ratio of positive and negative money flow over time.
  #-----------------------------------------------------------------------
  # label <- "MFI"
  # result <- MFI(dt[,.(high,low,close)], dt[,volume])
  # TI_MFI <- result

  #-----------------------------------------------------------------------
  # On Balance Volume (OBV) is a measure of the money flowing
  # into or out of a security. It is similar to Chaikin
  # Accumulation / Distribution.
  #-----------------------------------------------------------------------
  label <- "OBV"
  result <- OBV(dt[,close], dt[,volume])
  TI_OBV <- result

  #-----------------------------------------------------------------------
  # PBands : Construct (optionally further smoothed and centered )
  # volatility bands around prices
  #-----------------------------------------------------------------------
  label <- "PBands"
  result <- PBands(dt[,close])
  colnames(result) <- c("PBANDS_dn","PBANDS_center","PBANDS_up")
  TI_PBANDS <- result

  #-----------------------------------------------------------------------
  # Calculate the (rate of) change of a series over
  # n periods.
  #-----------------------------------------------------------------------
  label <- "ROC"
  result <- ROC(dt[,close],n = 10)
  TI_ROC <- result

  label <- "ROC2"
  result <- ROC(dt[,close])
  TI_ROC2 <- result

  label <- "MOM"
  result <- momentum(dt[,close],n = 10)
  TI_MOM <- result

  label <- "MOM2"
  result <- momentum(dt[,close])
  TI_MOM2 <- result

  #-----------------------------------------------------------------------
  # The Relative Strength Index (RSI) calculates a ratio of
  # the recent upward price movements to the absolute price movement.
  #-----------------------------------------------------------------------

  # Default case
  label <- "RSI"
  result <- RSI(dt[,close], n=14)
  result <- cbind(result, RSI(dt[,close], n=9))
  result <- cbind(result, RSI(dt[,close], n=25))
  colnames(result) <- c("RSI14", "RSI9", "RSI25")
  TI_RSI <- result

  # Case of one 'maType' for both MAs
  label <- "RSI_MA1"
  result <- RSI(dt[,close], n=14, maType="WMA", wts=dt[,volume])
  TI_RSI_MA1 <- result

  # Case of two different 'maType's for both MAs
  label <- "RSI_MA2"
  result <- RSI(dt[,close], n=14, maType=list(maUp=list(EMA), maDown=list(WMA)))
  TI_RSI_MA2 <- result

  #-----------------------------------------------------------------------
  # The Parabolic Stop-and-Reverse calculates a trailing stop.
  #-----------------------------------------------------------------------
  label <- "SAR"
  result <- SAR(dt[,.(high,low)])
  colnames(result) <- c("SAR")
  TI_SAR <- result

  #-----------------------------------------------------------------------
  # Calculate various moving averages (MA) of a series.
  #-----------------------------------------------------------------------
  label <- "SMA"
  result <- SMA(dt[,close], n=20)
  TI_SMA <- result

  label <- "SMA10"
  result <- SMA(dt[,close], n=10)
  TI_SMA10 <- result

  label <- "EMA"
  result <- EMA(dt[,close], n=20)
  TI_EMA <- result

  label <- "EMA10"
  result <- EMA(dt[,close], n=10)
  TI_EMA10 <- result

  label <- "DEMA"
  result <- DEMA(dt[,close], n=20)
  TI_DEMA <- result

  label <- "DEMA10"
  result <- DEMA(dt[,close], n=10)
  TI_DEMA10 <- result

  label <- "WMA"
  result <- WMA(dt[,close], n=20)
  TI_WMA <- result

  label <- "WMA10"
  result <- WMA(dt[,close], n=10)
  TI_WMA10 <- result

  label <- "EVWMA"
  result <- EVWMA(dt[,close], dt[,volume], n=20)
  TI_EVWMA <- result

  label <- "EVWMA10"
  result <- EVWMA(dt[,close], dt[,volume], n=10)
  TI_EVWMA10 <- result

  label <- "ZLEMA"
  result <- ZLEMA(dt[,close], n=20)
  TI_ZLEMA <- result

  label <- "ZLEMA10"
  result <- ZLEMA(dt[,close], n=10)
  TI_ZLEMA10 <- result

  label <- "VWAP"
  result <- VWAP(dt[,close], dt[,volume], n=20)
  TI_VWAP <- result

  label <- "VWAP10"
  result <- VWAP(dt[,close], dt[,volume], n=10)
  TI_VWAP10 <- result

  label <- "HMA"
  result <- HMA(dt[,close], n=20)
  TI_HMA <- result

  label <- "HMA10"
  result <- HMA(dt[,close], n=10)
  TI_HMA10 <- result

  label <- "ALMA"
  result <- ALMA(dt[,close], n=20)
  TI_ALMA <- result

  label <- "ALMA10"
  result <- ALMA(dt[,close], n=10)
  TI_ALMA10 <- result

  #-----------------------------------------------------------------------
  # The stochastic oscillator is a momentum indicator that
  # relates the location of each day's close relative
  # to the high/low range over the past n periods
  #-----------------------------------------------------------------------
  label <- "stoch"
  result <- stoch(dt[,.(high,low,close)])
  colnames(result) <- c("STOCH_fastk","STOCH_fastd","STOCH_slowd")
  TI_STOCH <- result

  label <- "SMI"
  result <- SMI(dt[,.(high,low,close)],
                maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)) )
  colnames(result) <- c("SMI_smi","SMI_signal")
  TI_SMI <- result

  label <- "stochRSI"
  result <- stoch( RSI(dt[,close]) )
  colnames(result) <- c("STOCHRSI_fastk","STOCHRSI_fastd","STOCHRSI_slowd")
  TI_STOCHRSI <- result

  label <- "WPR"
  result <- WPR(dt[,.(high,low,close)])
  TI_WPR <- result

  #-----------------------------------------------------------------------
  # Log Price
  #-----------------------------------------------------------------------
  label <- "log_Price"
  result <- log(dt[,close])
  TI_LOG_PRICE <- result

  #-----------------------------------------------------------------------
  # TRIX
  #-----------------------------------------------------------------------
  label <- "TRIX"
  result <- TRIX(dt[,close], maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA), list(DEMA)))
  colnames(result) <- c("TRIX_trix","TRIX_signal")
  TI_TRIX <- result

  #-----------------------------------------------------------------------
  # TDI : The Trend Detection Index (TDI) attempts to identify starting and ending trends.
  #-----------------------------------------------------------------------
  label <- "TDI"
  result <- TDI(dt[,close], n=20)
  colnames(result) <- c("TDI_tdi","TDI_di")
  TI_TDI <- result

  #-----------------------------------------------------------------------
  # VHF : The Vertical Horizontal Filter (VHF) attempts to identify starting and ending trends.
  #-----------------------------------------------------------------------
  label <- "VHF"
  result <- VHF(dt[,close], n=20)
  TI_VHF <- result

  #-----------------------------------------------------------------------
  # EMV : Arms' Ease of Movement Value (EMV) emphasizes days where the
  # security moves easily and minimizes days where the security does not move easily.
  #-----------------------------------------------------------------------
  label <- "EMV"
  result <- EMV(dt[,.(high, low)], dt[,volume])
  colnames(result) <- c("EMV_emv","EMV_maemv")
  TI_EMV <- result

  #-----------------------------------------------------------------------
  # Pivots : undocumented...
  #-----------------------------------------------------------------------
  # label <- "PIVOT"
  # result <- TTR:::pivots(dt[,.(high, low, close)], lagts=FALSE)
  # TI_PIVOT <- result

  #-----------------------------------------------------------------------
  # AGGREGATION DES RESULTATS
  #-----------------------------------------------------------------------
  TECHNICALS_INDICATORS <-
    data.table(date = dt[,date],
               TI_DMI, TI_DMI7, TI_DMI21, TI_TREND, TI_ATR,TI_BBANDS,TI_STOCH,TI_DONCHIANCHANNEL,TI_DVI,
               TI_GMMA,TI_KST,TI_KST4MA,TI_MACD1,TI_MACD2,
               TI_PBANDS,
               CCI = TI_CCI,
               chaikinAD = TI_CHAIKINAD,
               chaikinVol = TI_CHAIKINVOLATILITY,
               CLV = TI_CLV,
               CMF = TI_CMF,
               CMO = TI_CMO,
               #MFI = TI_MFI,
               OBV = TI_OBV,
               ROC = TI_ROC,
               ROC2 = TI_ROC2,
               MOM = TI_MOM,
               MOM2 = TI_MOM2,
               TI_RSI,
               RSI_MA1 = TI_RSI_MA1,
               RSI_MA2 = TI_RSI_MA2,
               TI_SAR,
               SMA = TI_SMA,
               SMA10 = TI_SMA10,
               EMA = TI_EMA,
               EMA10 = TI_EMA10,
               DEMA = TI_DEMA,
               DEMA10 = TI_DEMA10,
               WMA = TI_WMA,
               WMA10 = TI_WMA10,
               EVWMA = TI_EVWMA,
               EVWMA10 = TI_EVWMA10,
               ZLEMA = TI_ZLEMA,
               ZLEMA10 = TI_ZLEMA10,
               VWAP = TI_VWAP,
               VWAP10 = TI_VWAP10,
               HMA = TI_HMA,
               HMA10 = TI_HMA10,
               #ALMA = TI_ALMA,
               #ALMA10 = TI_ALMA10,
               WPR = TI_WPR,
               PNL = TI_RETURN,
               log_Price = TI_LOG_PRICE,
               TI_TRIX,
               TI_TDI,
               TI_SMI,
               TI_STOCHRSI,
               TI_EMV,
               TI_VHF
    )

  setkey(TECHNICALS_INDICATORS, date)

  return(TECHNICALS_INDICATORS)
}
