
#' Dynamic merge of data.tables list with INNER join.
#'
#' @param list_dt a list of data.tables to merge.
#' The list should contains at least 2 data.tables.
#' And they should have all the same data.table keys.
#' @param exclude the name of a \code{data.table} in list_dt to exclude from the merge.
#'
#' @return a data.table containing all the data.tables merged fields.
#' @export
technical_tools.dynamic_merge <- function(list_dt, exclude = "something_that_we_will_never_name_like_that"){
  names_dt <- names(list_dt)

  if(is.null(names_dt)) return(technical_tools.dynamic_merge_from_unamed(list_dt))

  dimension <- which(names_dt != exclude)
  command <- paste0( "list_dt[[", dimension[1], "]]", paste0("[list_dt[[", dimension[2:length(dimension)], "]], nomatch = 0]", collapse = "") )
  return(eval(parse(text = command)))
}


#' Dynamic merge of data.tables list with FULL OUTER join.
#'
#' @param list_dt a list of data.tables to merge.
#' The list should contains at least 2 data.tables.
#' And they should have all the same data.table keys.
#'
#' @return a data.table containing all the data.tables merged fields.
#' @export
technical_tools.dynamic_merge_full <- function(list_dt){
  dimension <- length(list_dt)
  tmp <- list_dt[[1]]
  for(i in c(2:dimension)){
    tmp <- merge(x = tmp, y = list_dt[[i]], all = TRUE)
  }
  return(tmp)
}


#' Subtracts period from a date.
#'
#' @param a_date the date.
#' @param year the number of years to substract (e.g. 0.5, 1, 4)
#'
#' @return the result of the subtract.
#' @export
technical_tools.subtract_year <- function( a_date, year){
  tmp_date <- as.Date(a_date, "%Y-%m-%d")

  if (year >= 1) {
    result_date <- seq(tmp_date, length = 2, by = paste0("-", year, " year"))[2]
  }
  else {
    result_date <- seq(tmp_date, length = 2, by = paste0("-", 12*year, " months"))[2]
  }
  return(result_date)
}


technical_tools.dynamic_merge_from_unamed <- function(list_dt){
  dimension <- length(list_dt)
  command <- paste0( "list_dt[[1]]", paste0("[list_dt[[", 2:dimension, "]], nomatch = 0]", collapse = "") )
  eval(parse(text = command))
}

technical_tools.set_env_var <- function(var_name, var_value, env){
  # print("set")
  # if(exists(var_name, envir = env)) print( get(var_name, envir = env) )
  env[[var_name]] <- var_value
  # if(exists(var_name, envir = env)) print( get(var_name, envir = env) )
}

technical_tools.get_env_var <- function(var_name, env){
  # print("get")
  # if(exists(var_name, envir = env)) print( get(var_name, envir = env) )
  get(var_name, envir = env)
  # if(exists(var_name, envir = env)) print( get(var_name, envir = env) )
}
