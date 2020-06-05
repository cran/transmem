#' Calculates permeability coefficients
#'
#' Permeability coefficients across a membrane as derived from integrated
#' Fick's law can be obtained from transport data according to the equation
#' \deqn{\ln{\Bigg(\frac{C}{C^0}\Bigg)}=
#' -\frac{P~a}{V}t} where \eqn{P} is the permeability coefficient, \eqn{a} is
#' the membrane exposed area, \eqn{C} and \eqn{C^0} are
#' the species concentrations at any time and at initial time in the feed phase,
#' respectively, and \eqn{V} is solution volume.
#'
#' Species concentration units may be arbitrary as long as the permeability
#' coefficient is calculated using the change in concentration ratio which is,
#' as most ratios, adimensional
#'
#' @param trans  Data frame with the complete transport information of
#'               interest species. Must be generated using
#'               \code{\link{conc2frac}}.
#' @param conc0  Initial concentration of the species in the feed solution. The
#'               value may be extracted from transport information if the data
#'               frame provided in \code{trans} is not normalized. See
#'               \code{\link{conc2frac}} for details.
#' @param vol    Volume of the feed solution.
#' @param area   Membrane exposed area to the feed solution.
#' @param units  Units in which volume, area and time are provided. Volume
#'               and area are function's parameters while the time is
#'               extracted from the \code{trans} data frame.
#' @param plot   logical default to \code{TRUE}. Should the plot be made?
#'
#' @return A numeric vector with the permeability coefficient and it's
#' standard uncertainty from the regression. Units are meters per second.
#' @example
#'   data(reusecycles)
#'   (permcoef(trans = reusecycles[[1]], vol = 85,
#'            area = pi * 1.25^2, units = c('cm^3', 'cm^2', 'h')))
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export


permcoef <- function(trans, vol, area, units = c('cm^3', 'cm^2', 'h'),
                     conc0 = NULL, plot = FALSE) {
  if (missing(conc0)) conc0 <- trans$Fraction[1]
  conc <- trans[which(trans$Phase == "Feed"), ]
  y <- log(conc[, 3] / conc[1, 3])
  t <- trans[which(trans$Phase == "Feed"), 1]
  if (units[3] == 'h') t <- t * 3600
  if (units[1] == 'cm^3') vol <- vol / 1000000
  if (units[2] == 'cm^2') area <- area / 10000
  x <- (area / vol) * t
  model <- lm(y ~ 0 + x)

  if (plot) {
    plot(y ~ t)
    abline(lm(y ~ 0 + t), col = 4)
  }

  Xm <- - 1 * summary(model)$coefficients[1]
  sd <- summary(model)$coefficients[2]
  sd <- signif(sd, 2)
  Xm <- signif(Xm, 2 + ceiling(log10(Xm/sd)))
  message("Permeability coefficient: ", Xm, "+/-", sd, ' m/s \n')
  return(c(Xm, sd))
}
