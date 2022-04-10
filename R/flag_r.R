

#' Title
#'
#' @param levels
#'
#' @return
#' @export
flag_r.set_levels <- function(levels_value) {

  assign("yoolook.levels", levels_value, envir = .GlobalEnv)
}

#' Title
#'
#' @param levels
#'
#' @return
#' @export
flag_r.get_levels <- function() {
  if (exists("yoolook.levels", envir = .GlobalEnv)) {
    return(get("yoolook.levels", envir = .GlobalEnv))
  } else {
    default_levels <- c(0.25, 2:7)
    flag_r.set_levels(default_levels)
    return(default_levels)
  }
}

#' Title
#'
#' @param level_value
#'
#' @return
#' @export
flag_r.get_levels_index <- function(level_value) {
  levels <- flag_r.get_levels()
  return(which(levels == level_value))
}


#' Builds Flag R at once (bulk process).
#'
#' @param asset_data a data.table containing the asset data.
#'
#' @return a \code{list} containing
#' \itemize{
##'  \item{\code{value}   a \code{data.table} containing the Flag R values.}
##'  \item{\code{ttt}     a TTT (Technical Temporary Table) used for updating.}
#' }
#'
#' @import future
#' @import future.apply
#' @export
flag_r.build <- function(asset_data){

  # levels <- c(2:7)
  levels <- flag_r.get_levels()
  ttt_data <- list()

  # ttt_data <- parallel::mclapply(levels, FUN = flag_r_build_by_level,
  #                                asset_data = asset_data, mc.cores = 6L, mc.cleanup = TRUE)

  ttt_data <- future.apply::future_lapply(levels, flag_r_build_by_level,
                                          asset_data = asset_data)

  setkey(ttt_data[[1]], date)
  flag_r_data <- flag_r.extract(ttt_data)

  return(list(value = flag_r_data, ttt = ttt_data))
}

flag_r_build_by_level <- function(level, asset_data) {

  new_ttt_data <- flag_r_initialize_objectif_stoploss(level = level, asset_data = asset_data)
  new_ttt_data <- flag_r_compute_flag_r_init(level = level, ttt_data = new_ttt_data)
  new_ttt_data <- flag_r_get_flag_neutral(level = level, DT = new_ttt_data)

  return(new_ttt_data)
}


#' Builds and updates R Flag daily
#'
#' @param previous_ttt_data the TTT data from previous day.
#' @param day_asset_data asset data on one day.
#'
#' @return the updated Flag R with one more day.
#'
#' @import future
#' @import future.apply
#' @export
flag_r.update_daily <- function(previous_ttt_data, day_asset_data){

  # levels <- c(2:7)
  levels <- flag_r.get_levels()
  new_ttt_data <- list()

  new_ttt_data <- future.apply::future_lapply(levels, flag_r_update_daily_by_level,
                                              day_asset_data = day_asset_data, previous_ttt_data = previous_ttt_data)

  return(new_ttt_data)
}

flag_r_update_daily_by_level <- function(level, day_asset_data, previous_ttt_data) {

  new_ttt_data <- flag_r_initialize_objectif_stoploss(level = level, asset_data = day_asset_data)
  new_ttt_data <- flag_r_bind_ttt_data(previous_ttt_data[[flag_r.get_levels_index(level)]], new_ttt_data)
  new_ttt_data <- flag_r_compute_flag_r_init(level = level, ttt_data = new_ttt_data)
  #new_ttt_data <- flag_r_get_flag_neutral(level = level, DT = new_ttt_data)
  new_ttt_data <- flag_r_fix_flag_neutral(level = level, new_ttt_data = new_ttt_data, previous_ttt_data = previous_ttt_data)


  setkey(new_ttt_data, date)
  return(new_ttt_data)
}


#' Binds 2 lists of \code{data.table}.
#'
#' @param table_flag1 the list #1.
#' @param table_flag2 the list #2.
#'
#' @return one bound list.
#' @export
flag_r_bind_table_flag <- function(table_flag1, table_flag2) {

  for(i in 1:length(table_flag1)){
    table_flag1[[i]] <- rbind(table_flag1[[i]], table_flag2[[i]])
  }

  return(table_flag1)
}

flag_r_bind_ttt_data <- function(ttt_data1, ttt_data2) {

  ttt_data <- rbind(ttt_data1, ttt_data2)

  return(ttt_data)
}


#' Extracts Flag R from its temporary technical table.
#'
#' @param ttt_data the temporary technical table.
#'
#' @return the \code{flag_r}
#'
#' @import data.table
#' @export
flag_r.extract <- function(ttt_data) {

  flag_r <- technical_tools.dynamic_merge(ttt_data)

  extract_columns <- c("date", sapply(flag_r.get_levels(), FUN = function(i) paste("flag", i, "pc", sep = "_")))
  flag_r <- flag_r[, extract_columns, with =  FALSE]
  colnames(flag_r) <- c("date", sapply(flag_r.get_levels(), FUN = function(i) paste0("Trend_", i, "pc")))

  return(flag_r)
}

#' Initializes the temporary technical table (TTT).
#'
#' @param level the level to initialize.
#' @param DT the asset data used as skeleton for the TTT.
#' @param seuil the threshold used to compute target and stoploss.
#'
#' @return the temporary technical table (TTT).
#'
#' @import data.table
flag_r_initialize_objectif_stoploss <- function(level, asset_data, seuil = 1/5){

  DT <- copy(asset_data)

  stoploss_a <- paste0("stoploss_", level, "_a")
  stoploss_v <- paste0("stoploss_", level, "_v")

  objectif_a <- paste0("objectif_", level, "_a")
  objectif_v <- paste0("objectif_", level, "_v")

  objectif_a_ok <- paste0("objectif_", level, "_a_ok")
  objectif_v_ok <- paste0("objectif_", level, "_v_ok")

  stoploss_a_ok <- paste0("stoploss_", level, "_a_ok")
  stoploss_v_ok <- paste0("stoploss_", level, "_v_ok")

  flag_ok <- paste0("flag_", level, "_ok")
  flag <- paste0("flag_", level,  "_pc")

  DT[,eval(stoploss_a) := DT[,close] * (1 - seuil * level/100 )]
  DT[,eval(stoploss_v) := DT[,close] * (1 + seuil * level/100 )]

  DT[,eval(objectif_a) := DT[,close] * (1 + level/100 )]
  DT[,eval(objectif_v) := DT[,close] * (1 - level/100 )]

  DT[,eval(objectif_a_ok) := FALSE ]
  DT[,eval(objectif_v_ok) := FALSE ]
  DT[,eval(stoploss_a_ok) := FALSE ]
  DT[,eval(stoploss_v_ok) := FALSE ]
  DT[,eval(flag_ok) := FALSE ]

  DT[,eval(flag) := 0 ]

  suppressWarnings(DT[ , adjusted_close := NULL])
  #suppressWarnings(DT[ , date := NULL])
  suppressWarnings(DT[ , volume := NULL])
  suppressWarnings(DT[ , name := NULL])

  setkey(DT, date)

  return(DT)
}


#' Computes stoploss, target, signal and neutral flags.
#'
#' @param level the variation level.
#' @param DT the temporary technical table (TTT).
#'
#' @return the temporary technical table (TTT) with the computed flags.
#'
#' @import data.table
#' @import future
#' @import future.apply
flag_r_compute_flag_r_init <- function(level, ttt_data){

  flag_ok <- paste0("flag_", level, "_ok")

  # Checks Stoploss, Objectif and sets Flag R
  sapply(ttt_data[ttt_data[[flag_ok]] == FALSE, date], flag_r_check_all, level = level, DT_by_level = ttt_data)

  return(ttt_data)
}


#' Title
#'
#' @param d_day
#' @param level
#' @param DT_by_level
#'
#' @return
flag_r_check_all <- function(d_day, level, DT_by_level){
  flag_r_check_stoploss(d_day, level, DT_by_level)
  flag_r_check_objectif(d_day, level, DT_by_level)
  flag_r_check_flag_r(d_day,level, DT_by_level)
  flag_r_check_flag_neutral(level, DT_by_level)

  return(DT_by_level)
}


#' Title
#'
#' @param d_day
#' @param level
#' @param DT_by_level
#'
#' @return
flag_r_check_stoploss <- function(d_day, level, DT_by_level){

  stoploss_a <- paste0("stoploss_", level, "_a")
  stoploss_v <- paste0("stoploss_", level, "_v")

  stoploss_a_ok <- paste0("stoploss_", level, "_a_ok")
  stoploss_v_ok <- paste0("stoploss_", level, "_v_ok")

  flag_ok <- paste0("flag_", level, "_ok")

  DT_by_level[!(eval(as.symbol(flag_ok))) # pas de flag achat/vente
       & date < d_day # selection jours précédents
       & eval(as.symbol(stoploss_a)) >= DT_by_level[date == d_day , close ] ,
     eval(stoploss_a_ok) := TRUE ]

  DT_by_level[!(eval(as.symbol(flag_ok))) &
       date < d_day &
       eval(as.symbol(stoploss_v)) <= DT_by_level[date == d_day , close ] ,
     eval(stoploss_v_ok) := TRUE ]

  return(DT_by_level)
}


#' Title
#'
#' @param d_day
#' @param level
#' @param DT_by_level
#'
#' @return
flag_r_check_objectif <- function(d_day, level, DT_by_level){

  objectif_a <- paste0("objectif_", level, "_a")
  objectif_v <- paste0("objectif_", level, "_v")

  objectif_a_ok <- paste0("objectif_", level, "_a_ok")
  objectif_v_ok <- paste0("objectif_", level, "_v_ok")

  stoploss_a_ok <- paste0("stoploss_", level, "_a_ok")
  stoploss_v_ok <- paste0("stoploss_", level, "_v_ok")

  flag_ok <- paste0("flag_", level, "_ok")

  DT_by_level[!(eval(as.symbol(flag_ok))) &
       !(eval(as.symbol(stoploss_a_ok))) &
       date < d_day &
       eval(as.symbol(objectif_a)) <= DT_by_level[date == d_day , close ] ,
     eval(objectif_a_ok) := TRUE ]

  DT_by_level[!(eval(as.symbol(flag_ok))) &
       !(eval(as.symbol(stoploss_v_ok))) &
       date < d_day &
       eval(as.symbol(objectif_v)) >= DT_by_level[date == d_day , close ] ,
     eval(objectif_v_ok) := TRUE ]

  return(DT_by_level)
}


#' Title
#'
#' @param d_day
#' @param level
#' @param DT_by_level
#'
#' @return
flag_r_check_flag_r <- function(d_day, level, DT_by_level){

  objectif_a_ok <- paste0("objectif_", level, "_a_ok")
  objectif_v_ok <- paste0("objectif_", level, "_v_ok")

  stoploss_a_ok <- paste0("stoploss_", level, "_a_ok")
  stoploss_v_ok <- paste0("stoploss_", level, "_v_ok")

  flag_ok <- paste0("flag_", level, "_ok")
  flag <- paste0("flag_", level,  "_pc")

  DT_by_level[!(eval(as.symbol(flag_ok))) &
                eval(as.symbol(objectif_v_ok)) &
                date < d_day ,
             c(flag_ok ,flag):= list( TRUE, -level ) ]

  DT_by_level[!(eval(as.symbol(flag_ok))) &
                eval(as.symbol(objectif_a_ok)) &
                date < d_day ,
             c(flag_ok ,flag):= list( TRUE, level ) ]

  return(DT_by_level)
}


#' Title
#'
#' @param level
#' @param DT
#'
#' @return
flag_r_check_flag_neutral <- function(level, DT){

  stoploss_a_ok <- paste0("stoploss_", level, "_a_ok")
  stoploss_v_ok <- paste0("stoploss_", level, "_v_ok")

  flag_ok <- paste0("flag_", level, "_ok")
  flag <- paste0("flag_", level,  "_pc")

  DT[!eval(as.symbol(flag_ok))  &
      eval(as.symbol(stoploss_a_ok)) &
      eval(as.symbol(stoploss_v_ok)),
     c(flag_ok):= list( TRUE )]

  return(DT)
}


#' Computes neutral flags on bulk process.
#'
#' @param level
#' @param DT
#'
#' @return
flag_r_get_flag_neutral <- function(level, DT){

  flag <- paste0("flag_", level,  "_pc")

  #check_flag_neutral(level, DT)

  vectFlag <- DT[,eval(as.symbol(flag))]
  # initialisation: i = 1
  if(vectFlag[1] == 0){
    value <- sign(vectFlag[which(vectFlag != 0)[1]])
    vectFlag[1] <- ifelse(is.na(value), 1, value)
  }

  for(i in 2:length(vectFlag)){
    if(vectFlag[i] == 0){
      vectFlag[i] <- ifelse(sign(vectFlag[i-1]) >= 0, constant.NEUTRAL_VALUE, -constant.NEUTRAL_VALUE)
    }
  }

  DT[ ,eval(flag) := vectFlag]

  return(DT)
}


#' Computes and fixes neutral flags on daily process.
#'
#' @param level
#' @param new_ttt_data
#' @param previous_ttt_data
#'
#' @return
flag_r_fix_flag_neutral <- function(level, new_ttt_data, previous_ttt_data){

  new_ttt_data <- copy(new_ttt_data)
  previous_ttt_data <- copy(previous_ttt_data)[[flag_r.get_levels_index(level)]]

  flag <- paste0("flag_", level,  "_pc")
  flag_ok <- paste0("flag_", level,  "_ok")

  updated_values <- new_ttt_data[(1:(nrow(new_ttt_data)-1)), eval(as.symbol(flag_ok))] != previous_ttt_data[, eval(as.symbol(flag_ok))]
  updated_row_num <- min(which(updated_values))

  if(is.finite(updated_row_num)) {
    start_date <- new_ttt_data[updated_row_num, date]
  } else {
    start_date <- new_ttt_data[nrow(new_ttt_data), date]
  }

  setkey(new_ttt_data, date)
  #setindexv(new_ttt_data, c(paste0("flag_", level,  "_pc")))

  for(date_to_correct in new_ttt_data[date >= start_date, date]) {
    new_ttt_data[ date == date_to_correct &
        abs(eval(as.symbol(flag))) <= constant.NEUTRAL_VALUE,
        eval(flag) := new_ttt_data[new_ttt_data[  , .I[date == date_to_correct]] - 1, sign(eval(as.symbol(flag))) * constant.NEUTRAL_VALUE]
    ]
  }

  return(new_ttt_data)
}
