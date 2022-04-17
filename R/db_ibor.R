#
# IBOR
#

#' Load all IBOR Index from DB.
#'
#' @param name the name of the index (e.g. EURIBOR, LIBOR)
#'
#' @return a \code{data.table} containing the IBOR index.
#' @export
db_ibor.load <- function(name) {

  if(name == "EURIBOR") {
    euribor_m1 <- db_ibor.load_one("EURIBOR", "M1")
    euribor_s1 <- db_ibor.load_one("EURIBOR", "S1")
    euribor_m12 <- db_ibor.load_one("EURIBOR", "M12")
    euribor_m3 <- db_ibor.load_one("EURIBOR", "M3")
    euribor_m6 <- db_ibor.load_one("EURIBOR", "M6")
    ibor <- technical_tools.dynamic_merge(list(euribor_s1, euribor_m1, euribor_m3, euribor_m6, euribor_m12))
  }

  return(ibor)
}

#' Load one IBOR Index from DB.
#'
#' @param name the name of the index (e.g. EURIBOR, LIBOR)
#' @param period the name of the index (e.g. 1m, 12m)
#'
#' @return a \code{data.table} containing the IBOR index.
#' @export
db_ibor.load_one <- function(name, period) {

  name <- paste(name, period, sep = "_")

  ibor <- db_utils.load("ref_ibor", name)
  ibor <- db_ibor.name_column(as.data.table(ibor), c("date"), name)

  return(ibor)
}


#' Title
#'
#' @param dt
#' @param to_keep
#' @param name
#'
#' @return
#' @export
#'
#' @examples
db_ibor.name_column <- function(dt, to_keep, name) {

  suppressWarnings(dt[ , c("name") := NULL])
  old_names <- c(to_keep, "value")
  new_names <- c(to_keep, sapply(name, function(x) tolower(x)))
  setnames(x = dt, old = old_names, new = new_names)

  dt
}

#' Save IBOR Index into DB.
#'
#' @param name the name of the IBOR (e.g. EURIBOR, LIBOR)
#' @param period the period of the IBOR (e.g. 3M, 12M)
#' @param ibor  \code{data.table} containing the IBOR index to save.
#'
#' @return Nothing.
#' @export
db_ibor.save <- function(name, period, ibor) {

  name <- paste(name, period, sep = "_")
  ibor$name = name

  db_utils.save(ibor, "ref_ibor", name)
}


#' Update one specified IBOR into DB if exists.
#'
#' @param name the name of the IBOR (e.g. EURIBOR, LIBOR)
#' @param period the period of the IBOR (e.g. 3M, 12M)
#' @param to_update_ibor the \code{data.table} containing the IBOR to update.
#'
#' @return Nothing.
#'
#' @export
db_ibor.update <- function(name, period, to_update_ibor) {

  name <- paste(name, period, sep = "_")
  to_update_ibor$name <- name
  setkey(to_update_ibor, date)

  db_utils.update(to_update_ibor, "ref_ibor", name)
}


#' Delete one specified IBOR from DB if exists.
#'
#' @param name the name of the IBOR (e.g. EURIBOR, LIBOR)
#' @param period the period of the IBOR (e.g. 3M, 12M)
#' @param to_delete_ibor the \code{data.table} containing the IBOR to delete.
#'
#' @return Nothing.
#'
#' @export
db_ibor.delete <- function(name, period, to_delete_ibor) {

  name <- paste(name, period, sep = "_")
  db_utils.delete(to_delete_ibor, "ref_ibor", name)
}
