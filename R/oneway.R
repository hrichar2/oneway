#' Oneway: One-way Analysis of Variance
#'
#' The linmod package allow models to specified by their design matrix or by a formula.
#'
#' @docType package
#' @name oneway
NULL

#' @param z a data frame for the model.
#' @param formula a symbolic description of the model to be analyzed.
#' @param data an optional data frame containing the variables in the model.
#' @param \dots not used.
#' @return An objet of class \code{ANOVA}, basically a list including elements:
#' \item{diet}{ diet }
#'
#' \item{residuals}{ residuals }
#' @examples
#' oneway(coagulation)
#' oneway(coag ~ diet, data = coagulation)
#' summary.oneway(coagulation)
#'
#' @export
oneway <- function(z, ...) UseMethod("oneway")

#' @describeIn oneway oneway.default(z, ...)
#' @export
oneway.default <- function(z, ...)
{
  g <- length(z)
  n <- length(unlist(z))

  test <- sapply(z, is.numeric)
  if(all(test, TRUE)){y.grp.mean <- sapply(z, mean)
  } else {data.numbers <- sapply(z, as.numeric)
  y.grp.mean <- sapply(as.data.frame(data.numbers), mean)
  }

  y.grand.mean <- mean(unlist(z))
  y.grp.var <- sapply(z, var)
  y.grp.n <- sapply(z, length)

  ssb <- sum(y.grp.n * (y.grp.mean - y.grand.mean)^2)
  ssw <- sum((y.grp.n - 1) * y.grp.var)

  list(ssb=ssb, ssw=ssw, dfb=(g - 1), dfw=(n - g))
}

#' @describeIn oneway oneway.formula(formula, data = list(), ...)
#' @export
oneway.formula <- function(formula, data=list(), ...)
{
  mf <- model.frame(formula = formula, data = data)

  nums <- sapply(mf, is.numeric)
  y <- mf[ , nums]
  z <- mf[ , !nums]

  oneway(split(y, z))
}

#' Default print for oneway.
#'
#' @param x an object of class \code{"oneway"}, i.e., a an analysis of variance.
#' @param \dots not used.
#'
#' @export print.oneway
print.oneway <- function(x, ...)
{
  y <- oneway(x)
  tab <- as.data.frame(y)

  cat("Analysis:\n")
  print(tab, row.names = FALSE)
}

#' Summary method for oneway.
#'
#' @param object an objet of class \code{"oneway"}, an analysis of variance.
#' @param \dots not used.
#'
#' @export summary.oneway
summary.oneway <- function(object, ...)
{
  x <- as.data.frame(oneway(object))
  msb <- x$ssb/ x$dfb
  msw <- x$ssw / x$dfw
  F <- msb / msw
  p <- pf(F, x$dfb, x$dfw, lower.tail = FALSE)

  tab <- with(x, cbind(DF = c(x$dfb, x$dfw),
                       SS = c(x$ssb, x$ssw),
                       MS = c(msb, msw),
                       F = c(F, NA),
                       "Pr(>F)" = c(p, NA)))
  rownames(tab) <- c("Diet", "Residuals")
  tab

}

#' Print method for the summary method.
#'
#' @param x an object of class \code{"summary.oneway"}, i.e., an analysis of variance.
#' @param \dots not used.
#'
#' @export print.summary.oneway
print.summary.oneway <- function(x, ...)
{
  y <- summary.oneway(x)
  cat("\nAnalysis of Variance Table: \n")
  printCoefmat(y, P.values=TRUE, has.Pvalue=TRUE, signif.stars=TRUE, na.print="")

}

#' Plot method for oneway.
#'
#' @param x an object of class \code{"oneway"} i.e., an analysis of variance.
#' @param \dots not used.
#'
#' @export plot.oneway
plot.oneway <- function(x, ...) {
  plot(x)
}

