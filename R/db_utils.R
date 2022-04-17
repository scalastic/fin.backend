
#' Defines the MongoDB URI.
#'
#' @return the mongodb URI.
#' @export
db_utils.mongodb_uri <- function() {
  if (nchar(Sys.getenv("MONGODB_URI")) > 1) {
    Sys.getenv("MONGODB_URI")
  } else {
    "mongodb://localhost:27017/fin-db"
  }
}


#' Internal optimization storage by renaming column.
#'
#' @param dt the \code{data.table} to process.
#' @param to_keep a vector of columns to NOT process.
#' @param to_name a vector of columns to process.
#' @param name the prefix to add.
#'
#' @return the renamed columns \code{data.table}.
#'
#' @import data.table
#' @export
db_utils.name_column <- function(dt, to_keep, to_name, name) {

  suppressWarnings(dt[ , c("name") := NULL])
  old_names <- c(to_keep, to_name)
  new_names <- c(to_keep, sapply(to_name, function(x) paste(name, x, sep = "_")))
  setnames(x = dt, old = old_names, new = new_names)

  dt
}


#' Internal optimized load function by factorizing code.
#'
#' @param collection_name the name of the mongodb collection.
#' @param document_name the name of the documents in this collection.
#'
#' @return a \code{data.table} containing the loaded data.
#'
#' @import mongolite
#' @import data.table
db_utils.load <- function(collection_name, document_name) {

  conin <- mongolite::mongo(collection = collection_name, url = db_utils.mongodb_uri())
  data_db <- conin$find(query = paste0('{"name" : "', document_name, '"}'), sort = '{"date":1}')

  data_dt <- as.data.table(data_db)

  if(nrow(data_dt) > 0) setkey(data_dt, date)

 # rm(conin)
 # rm(data_db)

  return(data_dt)
}


#'  Internal optimized save function by factorizing code.
#'
#' @param new_data the \code{data.table} containing the data to save.
#' @param collection_name the name of the mongodb collection.
#' @param document_name the name of the documents in this collection.
#'
#' @return Nothing
#'
#' @import mongolite
#' @import data.table
#'
db_utils.save <- function(new_data, collection_name, document_name) {

  setkey(new_data, date)
  existing_data <- db_utils.load(collection_name, document_name)
  if(nrow(existing_data) > 0)
    new_data_date <- existing_data[new_data][is.na(name), date]
  else
    new_data_date <- new_data[, date]

  conout <- mongo(collection = collection_name, url = db_utils.mongodb_uri())
  conout$insert(as.data.frame(new_data[date %in% new_data_date, ]))
  conout$index(add = '{"name" : 1}')
  conout$index(add = '{"date" : 1}')

 # rm(conout)
 # rm(existing_data)
 # rm(new_data)
}


#' Internal optimized update function by factorizing code.
#'
#' @param to_update_data the \code{data.table} containing the data to update.
#' @param collection_name the name of the mongodb collection.
#' @param document_name the name of the documents in this collection.
#'
#' @return Nothing
#'
#' @import data.table
#'
db_utils.update <- function(to_update_data, collection_name, document_name) {

  existing_data <- db_utils.load(collection_name, document_name)
  to_update_data_date <- existing_data[to_update_data, nomatch=0][, date]

  db_utils.delete(to_update_data[date %in% to_update_data_date, ], collection_name, document_name)
  db_utils.save(to_update_data[date %in% to_update_data_date, ], collection_name, document_name)
}


#' Internal optimized delete function by factorizing code.
#'
#' @param to_delete_data the \code{data.table} containing the data to delete.
#' @param collection_name the name of the mongodb collection.
#' @param document_name the name of the documents in this collection.
#'
#' @return Nothing
#'
#' @import mongolite
#' @import data.table
#'
db_utils.delete <- function(to_delete_data, collection_name, document_name) {

  conout <- mongo(collection = collection_name, url = db_utils.mongodb_uri())
  conout$remove(
    paste0('{"date" : {"$in" : [',
           paste(shQuote(to_delete_data[, date], type = "cmd"), collapse=", ") ,
           ']}, "name" : "', document_name, '"}')
  )

 # rm(conout)
 # rm(to_delete_data)
}
