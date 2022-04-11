

plan("multisession")


test_that("data_tools.get_forecasting_dates returns dates", {

  expect_identical(
    data_tools.get_forecasting_dates("t", "2015-01-01", "2015-01-08"),
    c('2015-01-02', '2015-01-05', '2015-01-06', '2015-01-07', '2015-01-08')
  )

})


test_that("data_tools.get_daily_forecasting_dates returns dates", {

  expect_identical(
    data_tools.get_daily_forecasting_dates("t", "2022-04-06"),
    c('2022-04-07')
  )

})


test_that("data_tools.load_raw_data works", {

  expect_success(
    data_tools.load_raw_data("t", "2010-01-01")
  )

})


plan("sequential")
