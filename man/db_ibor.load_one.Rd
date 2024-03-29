% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_ibor.R
\name{db_ibor.load_one}
\alias{db_ibor.load_one}
\title{Load one IBOR Index from DB.}
\usage{
db_ibor.load_one(name, period)
}
\arguments{
\item{name}{the name of the index (e.g. EURIBOR, LIBOR)}

\item{period}{the name of the index (e.g. 1m, 12m)}
}
\value{
a \code{data.table} containing the IBOR index.
}
\description{
Load one IBOR Index from DB.
}
