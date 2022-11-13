
library(future)
library(progress)


#' Export X & y as CSV files
#'
#' @export
export_csv.X_y <- function(asset_code = "CAC_40", start_date = "2022-08-15", end_date = "2022-11-13", full_backup_path) {

  full_backup_path <- "/Users/jeanjerome/PROJETS/finance/data-raw"

  all_levels <- c(2:7)

  raw_data <- NULL

  dates_to_forecast <- data_tools.get_forecasting_dates(asset_code, start_date, end_date)


  pb <- progress_bar$new(format = "  computing :what [:bar] :percent eta: :eta",
                         clear = FALSE, width = 100,
                         total = length(dates_to_forecast)*length(all_levels))

  for(day_date in dates_to_forecast) {


    if(is.null(raw_data)) {
      raw_data <- data_tools.load_raw_data(asset_code, day_date)
    } else {
      raw_data <- data_tools.update_raw_data(raw_data, asset_code, day_date)
    }

    start_flat_raw_data <- technical_tools.subtract_year(day_date, 10)

    for(level in all_levels){
      pb$tick(tokens = list(what = paste("CSV file", level, "for", day_date)))

      flat_raw_data <- data_tools.flat_raw_data(raw_data, level, day_date, start_flat_raw_data)

      X_data <- data_tools.filter_trend(flat_raw_data)
      y_data <- flat_raw_data[, c("date", "Target")]

      write.csv(X_data, paste0(full_backup_path, "/X_", day_date, "_", level, "pc.csv"), row.names = FALSE)
      write.csv(y_data, paste0(full_backup_path, "/y_", day_date, "_", level, "pc.csv"), row.names = FALSE)
    }
  }

}
