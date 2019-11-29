#' Añade valores faltantes a un vectir, matriz o data frame
#'
#' @description Crea o importa un vector, matriz o data frame y reemplaza algunos de sus valores por NAs.
#'
#' @author ecoteam2019
#'
#' @param x Un vector, matriz o \code{data.frame}.
#' @param p Proporcion de valores faltantes para añadir al \code{x}. En caso \code{x} es un \code{data.frame}, \code{p} puede tambien ser un vector de probabilidades por columna o un vector nombrado (ver ejemplos).
#' @param seed Planta una semilla.
#'
#' @return \code{x} con valores faltantes.
#'
#' @export
#'
#' @examples
#' genNAs(1:10, p = 0.5, seed = 3345)
#' genNAs(rep(Sys.Date(), 10))
#' genNAs(cbind(1:10, 10:1), p = 0.2)
#' head(genNAs(iris))
#' head(genNAs(iris, p = 0.2))
#' head(genNAs(iris, p = c(0, 1, 0.5, 0.5, 0.5)))
#' head(genNAs(iris, p = c(Sepal.Length = 1)))
#' head(genNAs(iris, p = c(Species = 0.2, Sepal.Length = 0.5)))
genNAs <- function(x, p = 0.1, seed = NULL) {
  stopifnot(p >= 0, p <= 1, is.atomic(x) || is.data.frame(x))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  generateNaVec <- function(z, p) {
    n <- length(z)
    z[sample(n, round(p * n))] <- NA
    z
  }

  # Vector o matriz
  if (is.atomic(x)) {
    return(generateNaVec(x, p))
  }

  # Data frame
  v <- if (is.null(names(p))) names(x) else intersect(names(p), names(x))
  x[, v] <- Map(generateNaVec, x[, v, drop = FALSE], p)

  x
}
