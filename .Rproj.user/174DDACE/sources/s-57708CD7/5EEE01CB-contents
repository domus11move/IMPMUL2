#' Imputacion Univariada
#'
#' @description Llena los valores faltantes de un vector, una matriz o data frame
#' por muestreo con reemplazamiento de los valores no faltantes. Para data frames,
#' el muestreo es propio de cada columna.
#'
#' @author ecoteam2019
#'
#' @param x Un vector, matriz o data frame.
#' @param v Un vector caracter de nombre de columna a imputar (solo relevante si \code{x} es un data frame). El defecto \code{NULL} imputa todas las columnas.
#' @param seed Una semilla.
#'
#' @return \code{x} con valores imputados.
#' @export
#'
#' @examples
#' impUniv(c(NA, 0, 1, 0, 1))
#' impUniv(c("A", "A", NA))
#' impUniv(as.factor(c("A", "A", NA)))
#' head(impUniv(genNAs(iris)))
#' head(impUniv(genNAs(iris), v = "Species"))
#' head(impUniv(genNAs(iris), v = c("Species", "Petal.Length")))
impUniv <- function(x, v = NULL, seed = NULL) {
  stopifnot(is.atomic(x) || is.data.frame(x))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  imputeVec <- function(z) {
    na <- is.na(z)
    if ((s <- sum(na))) {
      if (s == length(z)) {
        stop("No non-missing elements to sample from.")
      }
      z[na] <- sample(z[!na], s, replace = TRUE)
    }
    z
  }

  # Vector o matriz
  if (is.atomic(x)) {
    return(imputeVec(x))
  }

  # Data frame
  v <- if (is.null(v)) names(x) else intersect(v, names(x))
  x[, v] <- lapply(x[, v, drop = FALSE], imputeVec)

  x
}
