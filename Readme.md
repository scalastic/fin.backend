
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fin.backend

This is a basic `R` package used in AI forecasts, which is intended to
define loading functions, technical indicator calculations and
rebalancing data. Its specific objective is also to calculate  
target values.

This R package is intended to be part of a trading signals application.

## Example

``` r
library(fin.backend)
#> Le chargement a nécessité le package : unbalanced
#> Le chargement a nécessité le package : mlr
#> Le chargement a nécessité le package : ParamHelpers
#> Warning message: 'mlr' is in 'maintenance-only' mode since July 2019.
#> Future development will only happen in 'mlr3'
#> (<https://mlr3.mlr-org.com>). Due to the focus on 'mlr3' there might be
#> uncaught bugs meanwhile in {mlr} - please consider switching.
#> Le chargement a nécessité le package : foreach
#> Le chargement a nécessité le package : doParallel
#> Le chargement a nécessité le package : iterators
#> Le chargement a nécessité le package : parallel
library(future)
library(ggplot2)

plan(multisession)

dt <- data_tools.load_raw_data("t", "2022-04-01")

dt_plot <- data_tools.flat_raw_data(dt, 7, "2022-04-01", "2015-01-01")

ggplot(dt_plot[date >= "2021-01-01", .(date, close, Target)]) + geom_line(aes(x = as.Date(date), y = close)) + geom_point(aes(x = as.Date(date), y = close, color = factor(round(Target))))
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
plan(sequential)
```

## Data

-   Historical FCHI : [Yahoo
    Finance](https://fr.finance.yahoo.com/quote/%5EFCHI/history?p=%5EFCHI)

-   Historical InterBank exchanges rates : [Banque de
    France](http://webstat.banque-france.fr/fr/downloadFile.do?id=5385564&exportType=csv)

-   Historical currencies rates : [Banque de
    France](http://webstat.banque-france.fr/fr/downloadFile.do?id=5385698&exportType=csv)

-   Historical VIX : [Yahoo
    Finance](https://finance.yahoo.com/quote/%5EVIX/history?period1=631238400&period2=1649462400&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true)
    CBOE Volatility Index (^VIX)

-   Other historical VIX :
    [CBOE](https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX_History.csv)

-   Historical VXD :
    [CBOE](https://cdn.cboe.com/api/global/us_indices/daily_prices/VXD_History.csv)
    DJIA Volatility
