% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_volatility.R
\name{db_volatility.delete}
\alias{db_volatility.delete}
\title{Delete one specified Volatility Index from DB if exists.}
\usage{
db_volatility.delete(name, to_delete_volatility)
}
\arguments{
\item{name}{the name of the Volatility Index (e.g. VIX, VXD)}

\item{to_delete_volatility}{the \code{data.table} containing the Volatility Index to delete.}
}
\value{
Nothing.
}
\description{
Delete one specified Volatility Index from DB if exists.
}
