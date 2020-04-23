#' Interpolates secondary species concentration at missing time values
#'
#' If the secondary species concentration is determined in just a fraction
#' of the aliquots and for some reason, the concentration in all the aliquots
#' is required or desired, the function fits a polynomial trend line to the
#' existing data and interpolates the concentration in missing aliquots.
#'
#' @param conc      Species concentration original vector.
#' @param time      Times at which given concentrations were determined.
#' @param compTime  Times at which the given species concentration must be
#'                  interpolated.
#' @param order     Order of the polynomial to be fitted to data (1 or 2).
#'                  Default to 2.
#' @return Vector of interpolated concentrations at times provided in
#'         \code{compTime}.
#' @example
#'   data(seawaterLiNaK)
#'   compTime <- seawaterLiNaK$Lithium.1$Time[1:7]
#'   time <- seawaterLiNaK$Sodium.1$Time[1:4]
#'   (conc <- seawaterLiNaK$Sodium.1$Fraction[1:4])
#'   (fixedconc <- fixSecondary(conc = conc, time = time, compTime = compTime))
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export

fixSecondary <- function(conc, time, compTime, order = 2) {
  model <- calibCurve(curve = data.frame(Conc = time,
                                         Signal = conc),
                      intercept = TRUE, order = order, plot = FALSE)

  fConc <- predict(model, newdata = data.frame(Conc = compTime))
  return(fConc)
}
