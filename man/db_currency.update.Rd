% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_currency.R
\name{db_currency.update}
\alias{db_currency.update}
\title{Update one specified Currency into DB if exists.}
\usage{
db_currency.update(name, to_update_currency)
}
\arguments{
\item{name}{the name of the Currency (e.g. EUR_CHF, EUR_USD)}

\item{to_update_currency}{the \code{data.table} containing the Currency to update.}
}
\value{
Nothing.
}
\description{
Update one specified Currency into DB if exists.
}
