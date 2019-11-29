#' Predictive Mean Matching
#'
#' @importFrom stats rmultinom
#' @importFrom FNN knnx.index
#'
#' @description Por cada valor en el vector de prediccion \code{xtest},
#' uno de los cercanos \code{k} valores en el vector de prediccion \code{xtrain}
#' es elegido aleatoriamente y su valor es observado en \code{ytrain} es retornado.
#'
#' @title missRanger pmm
#' @author ecoteam2019
#'
#' @param xtrain Vector con valores predichos in la data de training. Puede ser de tipo logical, numeric, character, o factor.
#' @param xtest Vector como \code{xtrain} con valores predichos en el test data. Valores faltantes no son permitidos.
#' @param ytrain Vector de los valores observados en el training data. Deben ser de la misma longitud que \code{xtrain}. Valores faltantes en alguno de \code{xtrain} o \code{ytrain} va a ser droppeado de manera pareada.
#' @param k Numero de vecinos cercanos para muestrear desde.
#' @param seed Semilla.
#'
#' @return Vector de la misma longitud que \code{xtest} con valores de \code{xtrain}.
#' @export
#'
#' @examples
#' pmm(xtrain = c(0.2, 0.2, 0.8), xtest = 0.3, ytrain = c(0, 0, 1)) # 0
#' pmm(xtrain = c(TRUE, FALSE, TRUE), xtest = FALSE, ytrain = c(2, 0, 1)) # 0
#' pmm(xtrain = c(0.2, 0.8), xtest = 0.3, ytrain = c("A", "B"), k = 2) # "A" or "B"
#' pmm(xtrain = c("A", "A", "B"), xtest = "A", ytrain = c(2, 2, 4), k = 2) # 2
#' pmm(xtrain = factor(c("A", "B")), xtest = factor("C"), ytrain = 1:2) # 2
pmm <- function(xtrain, xtest, ytrain, k = 1L, seed = NULL) {
  stopifnot(length(xtrain) == length(ytrain),
            sum(ok <- !is.na(xtrain) & !is.na(ytrain)) >= 1L,
            (nt <- length(xtest)) >= 1L, !anyNA(xtest),
            mode(xtrain) %in% c("logical", "numeric", "character"),
            mode(xtrain) == mode(xtest),
            k >= 1L)

  xtrain <- xtrain[ok]
  ytrain <- ytrain[ok]

  # Manejar caso trivial
  if (length(u <- unique(ytrain)) == 1L) {
    return(rep(u, nt))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # PASO 1: Transforma xtrain y xtest en numeros.
  # Maneja el caso de niveles de factores inconsistentes de xtrain y xtest.
  if (is.factor(xtrain) && (nlevels(xtrain) != nlevels(xtest) ||
                            !all(levels(xtrain) == levels(xtest)))) {
    xtrain <- as.character(xtrain)
    xtest <- as.character(xtest)
  }

  # Convierte vectores de caracteres en factores.
  if (is.character(xtrain)) {
    lvl <- unique(c(xtrain, xtest))
    xtrain <- factor(xtrain, levels = lvl)
    xtest <- factor(xtest, levels = lvl)
  }

  # Convierte todo en numeros.
  if (!is.numeric(xtrain) && mode(xtrain) %in% c("logical", "numeric")) {
    xtrain <- as.numeric(xtrain)
    xtest <- as.numeric(xtest)
  }

  # PASO 2: PMM basado en k-vecinos cercanos.
  k <- min(k, length(xtrain))
  nn <- knnx.index(xtrain, xtest, k)
  take <- t(rmultinom(nt, 1L, rep(1L, k)))
  ytrain[rowSums(nn * take)]
}
