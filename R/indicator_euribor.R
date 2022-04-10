

#' Computes PCA on EURIBOR.
#'
#' @param day_date a character vector indicating the date of the day until which PCA will be computed.
#'
#' @return a data.table containing the formatted result of the EURIBOR PCA.
#'
#' @importFrom  FactoMineR PCA
#' @export
euribor.get_pca <- function(day_date) {

  day_date <- as.Date(day_date)

  euribor_data <- data_file.load_euribor()[date <= day_date]

  euribor_pca <- PCA(euribor_data[date <= day_date, c("1m", "1s", "12m", "3m", "6m")],
                     scale.unit = TRUE, graph = FALSE)

  result <- data.table(
    "date"=euribor_data[date <= day_date][,date],
    "EURIBOR_dim1" = euribor_pca$ind$coord[ , c("Dim.1")],
    "EURIBOR_dim2" = euribor_pca$ind$coord[ , c("Dim.2")],
    "EURIBOR_dim3" = euribor_pca$ind$coord[ , c("Dim.3")],
    "EURIBOR_dim4" = euribor_pca$ind$coord[ , c("Dim.4")],
    "EURIBOR_dim5" = euribor_pca$ind$coord[ , c("Dim.5")]
  )
  setkey(result, date)

  return(result)
}
