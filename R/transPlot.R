#' Plots transport profiles of single run experiments
#'
#' Given the transport complete information of the interest species and,
#' optionally, secondary and tertiary species, the function plots transport
#' profiles including (if given) non-linear regression models that can be
#' obtained using \code{\link{transTrend}}.
#'
#' Most \code{transmem} graphical representations are made using the package
#' \code{ggplot2} so the function returns a ggplot2 object that can be
#' assigned to a variable for further modification.
#'
#' This function has a version that uses replicated experiments and
#' may be useful to illustrate repeateability. For more information see
#' \code{\link{transPlotWR}}.
#'
#' @references
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York. ISBN 978-3-319-24277-4,
#' \url{https://ggplot2.tidyverse.org}.
#'
#' @param trans     Data frame with the complete transport information of
#'                  interest species. Must be generated using
#'                  \code{\link{conc2frac}}. This is the only non-optional
#'                  parameter.
#' @param trend     Non-linear regression model of the main transport profile
#'                  generated using \code{\link{transTrend}}.
#' @param secondary Secondary species transport data frame (see
#'                  \code{\link{conc2frac}}).
#' @param tertiary  Tertiaty species transport data frame (see
#'                  \code{\link{conc2frac}}).
#' @param sec.trend Type of trend line to be used for secondary and tertiary
#'                  species data. Default is \code{'spline'} but
#'                  \code{'linear'}, \code{'loess'} and \code{'logarithmic'}
#'                  are also allowed.
#' @param lin.secon Deprecated. Use \code{sec.trend = 'linear'} instead.
#' @param span      Amount of smoothing when \code{sec.tred = 'loess'}. Is a
#'                  value between 0 and 1. Default is 0.75
#' @param legend    Logical. If \code{FALSE}, the default, the legend is not
#'                  included.
#' @param xlab      Label to be used for x axis. Text and expression allowed.
#' @param ylab      Label to be used for y axis. Text and expression allowed.
#' @param xlim      Numeric vector of limits for X-axis.
#' @param xbreaks   Numeric vector of x-axis breaks.
#' @param ylim      Numeric vector of limits for X-axis.
#' @param ybreaks   Numeric vector of x-axis breaks.
#' @param size      Size used for points in the plot.
#' @param bw        Logical, if \code{FALSE}, the default, a color version of
#'                  the plot is given. If a black and white version is
#'                  required, it must be set to \code{TRUE}.
#' @param srs       Relative size of the void space in shapes of the plot
#'                  when \code{bw = TRUE}. Needs to be adjusted according to
#'                  the graphical device resolution and desired appearance.
#' @param plot      Logical. If \code{TRUE}, the default, the plot is printed
#'                  in the current graphical device.
#'
#' @return Plot of the transport profile considering all provided species.
#' @importFrom grDevices hcl
#' @import ggplot2 stats graphics ggformula
#' @examples
#'   data(seawaterLiNaK)
#'   trend <- transTrend(trans = seawaterLiNaK$Lithium.1, model = 'paredes')
#'   transPlot(trans = seawaterLiNaK$Lithium.1, trend = trend,
#'             secondary = seawaterLiNaK$Sodium.1,
#'             tertiary = seawaterLiNaK$Potassium.1)
#'   transPlot(trans = seawaterLiNaK$Lithium.1, trend = trend,
#'             secondary = seawaterLiNaK$Sodium.1,
#'             tertiary = seawaterLiNaK$Potassium.1, bw = TRUE, srs = 0.75)
#'   transPlot(trans = seawaterLiNaK$Lithium.1, trend = trend,
#'             secondary = seawaterLiNaK$Sodium.1,
#'             tertiary = seawaterLiNaK$Potassium.1, bw = TRUE, srs = 0.5)
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export

transPlot <- function(trans, trend = NULL, secondary = NULL, tertiary = NULL,
                      sec.trend = 'spline', lin.secon = FALSE, span = 0.75,
                      legend = FALSE, xlab = 'Time (h)',
                      ylab = expression(Phi), xlim = NULL, ylim = NULL,
                      xbreaks = NULL, ybreaks = NULL, size = 2.8, bw = FALSE,
                      srs = 0.8, plot = TRUE){
  #Missing global variables issue correction
  Time <- Fraction <- Phase <- SD <- NULL

  p <- ggplot(data = trans, aes(x = Time, y = Fraction, group = Phase)) +
    theme_bw() +  geom_point(size = size, shape = 15, aes(color = Phase)) +
    labs(y = ylab, x = xlab) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"))

  if (!missing(trend)) {
    if (trend$model == 'paredes') {
      e <- trend$eccen
      trend <- list(trend)
      if (bw) colbw <- c("black", "black") else colbw <- c("red", "black")
      p <- p + stat_function(fun = AddParTrend(trend, 1, 'strip', e),
                             color = colbw[1], args = list(i = 1),
                             xlim = c(0, trans$Time[length(trans$Time)])) +
        stat_function(fun = AddParTrend(trend, 1, 'feed', e),
                      color = colbw[2],  args = list(i = 1),
                      xlim = c(0, trans$Time[length(trans$Time)]))
    }
  }
  if (!missing(lin.secon)) {
    warning("lin.secon is deprecated. Use sec.trend = 'linear' instead.")
    sec.trend = 'linear'
  }

  if (bw) {
    p <- p + geom_point(data = trans[which(trans$Phase == 'Strip'), ],
                        col = 'white', size = size*srs,
                        aes(x = Time, y = Fraction), shape = 15)
  }

  if (!missing(secondary)) {
    secondary$Phase <- paste0(secondary$Phase, ".")
    if (sec.trend == 'linear') {
      p <- p + scale_shape_identity() +
        geom_smooth(method = "lm", data = secondary, se = FALSE, size = 0.5,
                    aes(x = Time, y = Fraction, group = Phase, color = Phase))
    }
    if (sec.trend == 'spline') {
      p <- p + scale_shape_identity() +
        geom_spline(data = secondary, spar = 0.7, size = 0.5,
                    aes(x = Time, y = Fraction, group = Phase, color = Phase))
    }
    if (sec.trend == 'logarithmic') { #Still under implementation
      p <- p + scale_shape_identity() +
        stat_smooth(data = secondary, method = "lm", formula = y ~ log(x),
                    size = 0.5, se = FALSE,
                    aes(x = Time, y = Fraction, group = Phase, color = Phase))
    }
    if (sec.trend == 'loess') {
      p <- p + scale_shape_identity() +
        stat_smooth(data = secondary, method = "loess", span = span,
                    size = 0.5, se = FALSE,
                    aes(x = Time, y = Fraction, group = Phase, color = Phase))
    }
    if (bw) {
      p <- p + geom_point(data = secondary, size = size, shape = 17,
                          aes(x = Time, y = Fraction), color = 'black')
      sec.stript <- secondary[which(secondary$Phase == 'Strip.'), ]
      p <- p + geom_point(data = sec.stript, size = size * srs, shape = 17,
                          aes(x = Time, y = Fraction), color = 'white')
    } else {
      p <- p + geom_point(data = secondary, size = 3, shape = 17,
                          aes(x = Time, y = Fraction, group = Phase,
                              color = Phase))
    }
  }

  if (!missing(tertiary)) {
    tertiary$Phase <- paste0(tertiary$Phase, ".")
    if (sec.trend == 'linear') {
      p <- p + scale_shape_identity() +
        geom_smooth(method = "lm", data = tertiary, se = FALSE, size = 0.5,
                    aes(x = Time, y = Fraction, group = Phase, color = Phase))
    }
    if (sec.trend == 'spline') {
      p <- p + scale_shape_identity() +
        geom_spline(data = tertiary, spar = 0.7, size = 0.5,
                    aes(x = Time, y = Fraction, group = Phase, color = Phase))
    }
    if (sec.trend == 'logarithmic') { #Still under implementation
      p <- p + scale_shape_identity() +
        stat_smooth(data = tertiary, method = "lm", formula = y ~ log(x),
                    size = 0.5, se = FALSE,
                    aes(x = Time, y = Fraction, group = Phase, color = Phase))
    }
    if (sec.trend == 'loess') {
      p <- p + scale_shape_identity() +
        stat_smooth(data = tertiary, method = "loess", span = span, size = 0.5,
                    se = FALSE,
                    aes(x = Time, y = Fraction, group = Phase, color = Phase))
    }
    if (bw) {
      p <- p + geom_point(data = tertiary, size = size, shape = 16,
                          aes(x = Time, y = Fraction), color = 'black')
      Tert.strip <- tertiary[which(tertiary$Phase == 'Strip.'), ]
      p <- p + geom_point(data = Tert.strip, size = size * srs, shape = 16,
                          aes(x = Time, y = Fraction), color = 'white')
    } else {
      p <- p + geom_point(data = tertiary, size = 3,
                          aes(x = Time, y = Fraction,
                              group = Phase, shape = 16, color = Phase))
    }
  }

  if (!missing(xlim) && !missing(xbreaks)) {
    p <- p  + scale_x_continuous(breaks = xbreaks, limits = xlim)
  } else {
    if (!missing(xlim)) {
      p <- p  + scale_x_continuous(limits = xlim)
    }
    if (!missing(xbreaks)) {
      p <- p  + scale_x_continuous(breaks = xbreaks)
    }
  }

  if (!missing(ylim) && !missing(ybreaks)) {
    p <- p  + scale_y_continuous(breaks = ybreaks, limits = ylim)
  } else {
    if (!missing(ylim)) {
      p <- p  + scale_y_continuous(limits = ylim)
    }
    if (!missing(ybreaks)) {
      p <- p  + scale_y_continuous(breaks = ybreaks)
    }
  }

  if (!legend) {
    p <- p + theme(legend.position = 'none')
  }

  if (bw) {
    if (missing(secondary)) {
      p <- p + scale_color_manual(values = rep("black", 2))
    } else {
      p <- p + scale_color_manual(values = rep("black", 4))
    }
  } else {
    if (missing(secondary)) {
      p <- p + scale_color_manual(values = c("black", "red"))
    } else {
      p <- p + scale_color_manual(values = c("black", "gray48", "red",
                                             "indianred1"))
    }
  }

  print(p)
  return(p)
}
