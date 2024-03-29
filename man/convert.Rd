% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missRanger.R
\name{convert}
\alias{convert}
\title{Conversion de variables no-factores/no-numericas.}
\usage{
convert(X, check = FALSE)
}
\arguments{
\item{X}{Un data frame.}

\item{check}{Si \code{TRUE}, la funcion chequea si las columnas convertidas pueden ser revertidas sin cambios.}
}
\value{
Una lista con los siguientes elementos: \code{X} es la data frame convertida, \code{vars}, \code{types}, \code{classes} son los nombres, tipos y clases de las variables convertidas. Finalmente, \code{bad} los nombres de las variables en \code{X} que deberian ser convertidas pero que no lo fueron.
}
\description{
Convierte variables no-factores/no-numericas en un data frame de factores/numericas. Guarda la informacion para su reversion.
}
\author{
ecoteam2019
}
