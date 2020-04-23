#' Plots transport profiles of replicated experiments
#'
#' The function works the same way as \code{\link{transPlot}} but requires
#' several experimental data sets that must be concatenated in lists.
#' This allows the process reproducibility to be evaluated in the analysis
#' of the results.
#'
#' Most \code{transmem} graphical representations are made using the package
#' \code{ggplot2} so the function returns a ggplot2 object that can be
#' assigned to a variable for further modification.
#'
#' @references
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York. ISBN 978-3-319-24277-4,
#' \url{https://ggplot2.tidyverse.org}.
#'
#'
#' @param trans     List of data frames with the complete transport
#'                  information of interest species. Must be generated
#'                  using \code{\link{conc2frac}}. This is the only
#'                  non-optional parameter.
#' @param trend     List of Non-linear regression models of the main species
#'                  transport profil. Generated using \code{\link{transTrend}}.
#' @param secondary List of secondary species transport data frame (see
#'                  \code{\link{conc2frac}}).
#' @param tertiary  List of tertiary species transport data frame (see
#'                  \code{\link{conc2frac}}).
#' @param explicit  Logical, if \code{FALSE}, the default, transport
#'                  informations are averaged and plotted using errorbars that
#'                  with the standard deviation values. If \code{TRUE}, all
#'                  provided data is plotted in the same graphic.
#' @inheritParams transPlot
#' @return Plot of replicated transport profiles including all provided species
#' @examples
#'   data(seawaterLiNaK)
#'   # Transport data frames and transport NLS regresions must be in lists
#'   lithium <- list(seawaterLiNaK$Lithium.1, seawaterLiNaK$Lithium.2)
#'   sodium <- list(seawaterLiNaK$Sodium.1, seawaterLiNaK$Sodium.2)
#'   potassium <- list(seawaterLiNaK$Potassium.1, seawaterLiNaK$Potassium.2)
#'   trend <- list(transTrend(trans = seawaterLiNaK$Lithium.1),
#'                 transTrend(trans = seawaterLiNaK$Lithium.1))
#'
#'   transPlotWR(trans = lithium, trend = trend, secondary = sodium,
#'               tertiary = potassium, bw = TRUE, srs = 0.75)
#'   transPlotWR(trans = lithium, trend = trend, secondary = sodium,
#'               tertiary = potassium, bw = TRUE, srs = 0.5)
#' @importFrom grDevices hcl
#' @importFrom ggformula geom_spline
#' @import ggplot2 stats graphics
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export

transPlotWR <- function(trans, trend = NULL, secondary = NULL, tertiary = NULL,
                        legend = FALSE, xlab = 'Time (h)',
                        ylab = expression(Phi), xlim = NULL, ylim = NULL,
                        xbreaks = NULL, ybreaks = NULL, lin.secon = FALSE,
                        sec.trend = 'spline', span = 0.75, explicit = FALSE,
                        size = 3, plot = TRUE, bw = FALSE, srs = 0.8){
  #Missing global variables issue correction
  Time <- Fraction <- Phase <- SD <- mtrend <- NULL

  if (!missing(lin.secon)) {
    warning("lin.secon is deprecated. Use sec.trend = 'linear' instead.")
    sec.trend = 'linear'
  }
  eccen = 1

  if (explicit) {
    p <- ggplot(data = trans[[1]],
                         aes(x = Time, y = Fraction, group = Phase)) +
      theme_bw() + #ggsci::scale_color_npg() +
      geom_point(size = size, shape = 15, aes(color = Phase)) +
      labs(y = ylab, x = xlab) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black"))

    for (i in 2:length(trans)) {
      p <- p + geom_point(data = trans[[i]], size = size, shape = 15,
                          aes(color = Phase))
    }

    if (!missing(trend)) {
      if (trend[[1]]$model == 'paredes') {
        e <- trend[[1]]$eccen
        for (i in 1:length(trend)) {
          p <- p + stat_function(fun = AddParTrend(trend, i, 'strip', e),
                                 args = list(i = i), color = "red",
                                 xlim = xlimTrendWR(1, trans))
          p <- p + stat_function(fun = AddParTrend(trend, i, 'feed', e),
                                 args = list(i = i), color = "black",
                                 xlim = xlimTrendWR(1, trans))
        }
      }
    }

    if (!missing(secondary)) {
      for (i in 1:length(secondary)) {
        secondary[[i]]$Phase <- paste0(secondary[[i]]$Phase, ".")
        if (sec.trend == 'linear') {
          p <- p + scale_shape_identity() +
            geom_smooth(method = "lm", data = secondary[[i]], se = FALSE,
                        size = 0.5, aes(x = Time, y = Fraction,
                                        group = Phase, color = Phase))
        }
        if (sec.trend == 'spline') {
          p <- p + scale_shape_identity() +
            geom_spline(data = secondary[[i]], spar = 0.7, size = 0.5,
                        aes(x = Time, y = Fraction, group = Phase,
                            color = Phase))
        }
        if (sec.trend == 'logarithmic') { #Still under implementation
          p <- p + scale_shape_identity() +
            stat_smooth(data = secondary[[i]], method = "lm",
                        formula = y ~ log(x), size = 0.5, se = FALSE,
                        aes(x = Time, y = Fraction, group = Phase,
                            color = Phase))
        }
        if (sec.trend == 'loess') {
          p <- p + scale_shape_identity() +
            stat_smooth(data = secondary[[i]], method = "loess",
                        span = span, size = 0.5, se = FALSE,
                        aes(x = Time, y = Fraction, group = Phase,
                            color = Phase))
        }
        if (bw) {
          p <- p + geom_point(data = secondary[[i]], size = 3,
                              aes(x = Time, y = Fraction,
                                  group = Phase, shape = 17, color = Phase))
        } else {
          p <- p + geom_point(data = secondary[[i]], size = 3,
                              aes(x = Time, y = Fraction,
                                  group = Phase, shape = 17, color = Phase))
        }
      }
    }
  } else {
    mtrans <- transColapse(trans = trans)
    e <- trend[[1]]$eccen
    mtrend <- list(transTrend(trans = mtrans, model = 'paredes',
                              eccen = e))

    if (bw) colbw <- c("black", "black") else colbw <- c("red", "black")

    p <- ggplot(data = mtrans, aes(x = Time, y = Fraction, group = Phase)) +
      theme_bw() + geom_point(size = size, shape = 15, aes(color = Phase)) +
      labs(y = ylab, x = xlab) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black")) +
      stat_function(fun = AddParTrend(mtrend, 1, 'strip', eccen),
                    color = colbw[1], args = (i = 1),
                    xlim = c(0, mtrans$Time[length(mtrans$Time)])) +
      stat_function(fun = AddParTrend(mtrend, 1, 'feed', eccen),
                    color = colbw[2], args = (i = 1),
                    xlim = c(0, mtrans$Time[length(mtrans$Time)]))
      p <- p + geom_errorbar(aes(x = Time, ymin = Fraction - SD,
                                 ymax = Fraction + SD, color = Phase),
                             width = 0.1)

    if (bw) {
      p <- p + geom_point(data = mtrans[which(mtrans$Phase == 'Strip'), ],
                          col = 'white', size = size*srs,
                          aes(x = Time, y = Fraction), shape = 15)
    }

    if (!missing(secondary)) {
      msecon <- transColapse(trans = secondary)
      msecon$Phase <- paste0(msecon$Phase, ".")
      if (sec.trend == 'linear') {
        p <- p + scale_shape_identity() +
          geom_smooth(method = "lm", data = msecon, se = FALSE, size = 0.5,
                      aes(x = Time, y = Fraction, group = Phase,
                          color = Phase))
      }
      if (sec.trend == 'spline') {
        p <- p + scale_shape_identity() +
          ggformula::geom_spline(data = msecon, spar = 0.7, size = 0.5,
                                 aes(x = Time, y = Fraction, group = Phase,
                                     color = Phase))
      }
      if (sec.trend == 'logarithmic') { #Still under implementation
        p <- p + scale_shape_identity() +
          stat_smooth(data = msecon, method = "lm", formula = y ~ log(x),
                      size = 0.5,
                      se = FALSE, aes(x = Time, y = Fraction,
                                      group = Phase, color = Phase))
      }
      if (sec.trend == 'loess') {
        p <- p + scale_shape_identity() +
          stat_smooth(data = msecon, method = "loess", span = span,
                      size = 0.5, se = FALSE,
                      aes(x = Time, y = Fraction, group = Phase,
                          color = Phase))
      }
        p <- p + geom_errorbar(data = msecon,
                             aes(x = Time, ymin = Fraction - SD,
                                 ymax = Fraction + SD, color = Phase),
                             width = 0.1)
      if (bw) {
        p <- p + geom_point(data = msecon, size = size,
                            aes(x = Time, y = Fraction), shape = 17,
                            color = 'black')
        p <- p + geom_point(data = msecon[which(msecon$Phase == 'Strip.'), ],
                            size = size * srs,
                            aes(x = Time, y = Fraction), shape = 17,
                            color = 'white')
      } else {
        p <- p + geom_point(data = msecon, size = 3,
                            aes(x = Time, y = Fraction,
                                group = Phase, shape = 17, color = Phase))
      }
    }
    if (!missing(tertiary)) {
      mterna <- transColapse(trans = tertiary)
      mterna$Phase <- paste0(mterna$Phase, ".")
      if (sec.trend == 'linear') {
        p <- p + scale_shape_identity() +
          geom_smooth(method = "lm", data = mterna, se = FALSE, size = 0.5,
                      aes(x = Time, y = Fraction, group = Phase,
                          color = Phase))
      }
      if (sec.trend == 'spline') {
        p <- p + scale_shape_identity() +
          ggformula::geom_spline(data = mterna, spar = 0.7, size = 0.5,
                                 aes(x = Time, y = Fraction, group = Phase,
                                     color = Phase))
      }
      if (sec.trend == 'logarithmic') { #Still under implementation
        p <- p + scale_shape_identity() +
          stat_smooth(data = mterna, method = "lm", formula = y ~ log(x),
                      size = 0.5, se = FALSE,
                      aes(x = Time, y = Fraction, group = Phase,
                          color = Phase))
      }
      if (sec.trend == 'loess') {
        p <- p + scale_shape_identity() +
          stat_smooth(data = mterna, method = "loess", span = span,
                      size = 0.5, se = FALSE,
                      aes(x = Time, y = Fraction, group = Phase,
                          color = Phase))
      }
      p <- p + geom_errorbar(data = mterna,
                             aes(x = Time, ymin = Fraction - SD,
                                 ymax = Fraction + SD, color = Phase),
                             width = 0.1)
      if (bw) {
        p <- p + geom_point(data = mterna, size = size,
                            aes(x = Time, y = Fraction), shape = 16,
                            color = 'black')
        p <- p + geom_point(data = mterna[which(mterna$Phase == 'Strip.'), ],
                            size = size * srs,
                            aes(x = Time, y = Fraction), shape = 16,
                            color = 'white')
      } else {
        p <- p + geom_point(data = mterna, size = 3,
                            aes(x = Time, y = Fraction,
                                group = Phase, shape = 16, color = Phase))
      }
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
      p <- p + scale_color_manual(values = rep("black", 6))
    }
  } else {
    if (missing(secondary)) {
      p <- p + scale_color_manual(values = c("black", "red"))
    } else {
      p <- p + scale_color_manual(values = c("black", "gray48",
                                             "red", "indianred1"))
    }
  }

  if (plot) print(p)
  return(p)
}
