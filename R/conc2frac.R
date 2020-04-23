#' Creates a data frame as a complete self-contained transport data set
#'
#' The function transforms the data contained in concentration vectors of
#' feed and strip phases to a data frame that contains the complete data
#' of a transport process. This new data frame can be used by several functions
#' inside the package. The output data frame may contain normalized fractions
#' remaining in the feed and already transported to the strip phase, or the
#' original data provided in concentration units.
#'
#' The change in concentration of species in the feed and strip phases as a
#' function of time are the main magnitudes being measured in processes
#' involving transport across membranes. The best form to deal with such data
#' is inside a dataframe containing the information about the concentration
#' of given species in both phases and the time transcurred.
#'
#' Usually, this function is required after using \code{\link{signal2conc}}
#' wich convert instrumental signals to concentrations.
#'
#' @param feed          Numeric vector with concentrations in the feed phase.
#' @param strip         Numeric vector with concentrations in the strip phase.
#' @param time          Numeric vector with time at which the aliquots were
#'                      sampled. It is an optional parameter. If not provided,
#'                      regular unitary time intervals are assumed.
#' @param correct.strip Logical. If \code{FALSE}, the default, the information
#'                      about the amount transported to the strip phase is used
#'                      as received but if it is set to \code{TRUE}, the
#'                      initial concentration in the strip phase is substracted
#'                      to all concentrations in the same phase. This is
#'                      particularly useful when the blank signal is
#'                      significative or there is background noise.
#' @param normalize     Logical. If \code{TRUE}, the default, all
#'                      concentrations are divided by the initial concentration
#'                      in the feed phase to give results in fraction units.
#' @examples
#'   transData <- conc2frac(feed = c(0.200, 0.169, 0.152, 0.141, 0.138),
#'                          strip = c(0.000, 0.035, 0.045, 0.062, 0.069),
#'                          time = c(0, 2, 4, 6, 8))
#'   print(transData)
#' @return Data frame with the transport proccess information
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export
#'

conc2frac <- function(feed, strip, time = NULL, correct.strip = FALSE,
                      normalize = TRUE){

  if (missing(time)) time <- 1:length(feed)
  if (normalize) {
    strip <- strip / feed[1]
    feed  <- feed / feed[1]
  }

  if (correct.strip) strip <- strip - strip[1]

  DF <- data.frame(Time = rep(time, 2), Phase = rep(c('Feed', 'Strip'),
                                                    each = length(time)),
                   Fraction = c(feed, strip))

  return(DF)
}
