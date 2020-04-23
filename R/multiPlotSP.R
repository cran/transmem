#' Plots several single-phase transport profiles overlayed
#'
#' Given a list of several complete transport data, the function overlays the
#' transport profiles in a defined phase. The function is useful in membrane
#' reuse experiments as transport profile deterioration is easily visualized.
#'
#' Most \code{transmem} graphical representations are made using the package
#' \code{ggplot2} so the function returns a ggplot2 object that can be
#' assigned to a variable for further modification.
#'
#' @param phase     Phase to be represented in the plot: \code{'strip'},
#'                  the default, or \code{'feed'}.
#' @param arw       Logical default to \code{FALSE}. If \code{TRUE}, a vertical
#'                  arrow is drawn in the plot. Its use is recommended when
#'                  a trend along the profiles is to be indicated.
#' @param arw.pos   Numeric vector of the coordinates of the arrow if
#'                  \code{arw = TRUE}. The format is (x0, x1, y0, y1)
#' @param arw.txt   Text to be (optionally) printed alongside the arrow.
#' @param txt.pos   Numeric vector of the position of the center of the text
#'                  provided in \code{arw.txt}. The format is (x, y). If not
#'                  provided, the text is located close to the arrow but a
#'                  little alignment could be required.
#' @param txt.size  Size of the text accompanying the arrow.
#' @param shape     Shape to use in the points to be plotted.
#' @inheritParams transPlotWR
# #' @inheritParams transPlot
#' @return Plot with the overlayed transport profiles for a single phase
#' @examples
#'   data(reusecycles)
#'   # First step is to get trend lines for each cycle:
#'   trend <- list()
#'   for (i in 1:length(reusecycles)) {
#'     trend[[i]] <- transTrend(trans = reusecycles[[i]])
#'   }
#'   # Default plot using colors:
#'   multiPlotSP(trans = reusecycles, trend = trend, legend = TRUE)
#'
#'   # Black and white plot including an arrow:
#'   multiPlotSP(trans = reusecycles, trend = trend, legend = TRUE, bw = TRUE,
#'               arw = TRUE, arw.pos = c(6.1, 6.1, 0.8, 0.6),
#'               arw.txt = 'Cycle', txt.pos = c(6.15, 0.7))
#' @import ggplot2
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export

multiPlotSP <- function(trans, phase = 'strip', trend = NULL, legend = FALSE,
                        xlab = 'Time (h)', ylab = expression(Phi), xlim = NULL,
                        ylim = NULL, xbreaks = NULL, ybreaks = NULL, size = 3,
                        plot = TRUE, shape = 15, bw = FALSE, arw = FALSE,
                        arw.pos = NULL, arw.txt = NULL,
                        txt.pos = NULL, txt.size = NULL){
  Time <- Fraction <- Cycle <- NULL
  phase <- tolower(phase)
  if (!any(phase == c('strip', 'feed'))) {
    stop("Only 'feed' or 'strip' are allowed in phase parameter")
  }
  if (phase == 'strip') phase <- 'Strip'
  if (phase == 'feed') phase <- 'Feed'

  for (i in 1:length(trans)) {
    trans[[i]] <- trans[[i]][which(trans[[i]]$Phase == phase), ]
  }

  hues = seq(15, 375, length = length(trans) + 1)
  if(bw) {
    cols = rep("black", length(trans))
  } else {
    cols = hcl(h = hues, l = 65, c = 100)[1:length(trans)]
  }


  p <- ggplot(data = trans[[1]], aes(x = Time, y = Fraction)) +
    theme_bw() + #ggsci::scale_color_npg() +
    geom_point(size = size, shape = shape, color = cols[1]) +
    labs(y = ylab, x = xlab) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"))

  for (i in 2:length(trans)) {
    p <- p + geom_point(data = trans[[i]], size = size, shape = shape,
                        color = cols[i])
  }

  if (!missing(trend)) {
    for (i in 1:length(trans)) {
      if (trend[[1]]$model == 'paredes') {
        e <- trend[[1]]$eccen
        if (phase == 'Strip'){
          p <- p + stat_function(fun = AddParTrend(trend, i, 'strip', e),
                                 color = cols[i], args = list(i = i),
                                 xlim = xlimTrendWR(1, trans))
        } else {
          p <- p + stat_function(fun = AddParTrend(trend, i, 'feed', e),
                                 color = cols[i], args = list(i = i),
                                 xlim = xlimTrendWR(1, trans))
        }
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

  if (!bw) {
    x <- y <- vector()
    for (i in 1:length(trans)) {
      x <- c(x, trans[[i]]$Time[1])
      y <- c(y, trans[[i]]$Fraction[1])
    }
    p <- p + geom_point(data = data.frame(Cycle = as.factor(1:length(trans)),
                                          x = x, y = y),
                        aes(x = x, y = y, group = Cycle, color = Cycle))
  }

  if (arw) {
    #x <- 1.25 * layer_scales(plot = p)$x$range$range
    #p <- p + scale_x_continuous(limits = x) +
    if (missing(arw.pos)) {
      if (phase == 'Strip') arw.pos <- c(0.5, 0.5, 0.8, 0.5)
      if (phase == 'Feed') arw.pos <- c(0.5, 0.5, 0.2, 0.5)
    }
    p <- p + annotate("segment", x = arw.pos[1], xend = arw.pos[2],
                      y = arw.pos[3], yend = arw.pos[4],
                      arrow = arrow(angle = 12))
  }
  if (!missing(arw.txt)) {
    if (missing(txt.size)) txt.size <- 3.1
    if (missing(txt.pos)) txt.pos <- c(arw.pos[1] * 1.05,
                                       mean(c(arw.pos[3], arw.pos[4])))
    p <- p + geom_text(x = txt.pos[1], y = txt.pos[2], label = arw.txt,
                       angle = 90, size = txt.size)
  }

  if (plot) print(p)
  return(p)
}
