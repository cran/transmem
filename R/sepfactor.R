#' Calculates separation factors between two transported species
#'
#' Given the transport data frames of two species, the function calculates
#' the separation factors of the main species A against a secondary species B
#' for each sample taken. If the dataset of secondary species is
#' smaller than that of the main species (e.g. if secondary species were
#' determined in only half the aliquots), the transport profile is completed
#' using \code{\link{fixSecondary}} function and a message will be printed.
#'
#' Separation factor for batch systems at any time different from zero is
#' defined as \deqn{SF_{A/B}(t)=\frac{C_a/C_b}{C_a^0/C_b^0}} where \eqn{C_a}
#' and \eqn{C_b} are the concentrations of A and B, respectively, in the
#' strip solution at a time \eqn{t}, and \eqn{C_a^0} and \eqn{C_b^0} are
#' the concentrations of A and B, respectively, in the feed phase at
#' \eqn{t=0} (Chen et al., 2018).
#'
#' For continuous or semicontinuous systems,
#' the separation factor is calculated according to the equation
#' \deqn{SF_{A/B}(t)\frac{C_{a,~s}/C_{b,~s}}{C_{a,~f}/C_{b,~f}}} where
#' \eqn{C_{a,~s}}, \eqn{C_{b,~s}} are A and B concentrations in the
#' strip phase at a time \eqn{t} and \eqn{C_{a,~f}}, \eqn{C_{b,~f}} are
#' the concentrations of A and B in the feed solution at a time \eqn{t}
#' (Koros and Shimidzu, 1996). Separation factor at \eqn{t=0} equals 1
#' indicating that no species separation has occurred yet.
#'
#' @references
#' Q. B. Chen, Z. Y. Ji, J. Liu, Y. Y. Zhao, S. Z. Wang, J. S. Yuan,
#' Development of recovering lithium from brines by selective-electrodialysis:
#' Effect of coexisting cations on the migration of lithium, Journal of
#' Membrane Science 548 (2018) 408-420. doi:10.1016/j.memsci.2017.11.040.505
#'
#' J. Koros, H. Ma, T. Shimidzu, Terminology for membranes and membrane
#' processes (iupac recommendations 1996), Pure and Applied Chemistry 68 (7)
#' (1996) 1479-1489. doi:10.1351/pac199668071479.
#'
#'
#' @param main    Main species transport data. Must be a data frame generated
#'                using \code{conc2frac}, data normalization is indifferent.
#' @param secon   Undesired species transport data. Must be a data frame
#'                generated using \code{conc2frac}, data normalization is
#'                indifferent.
#' @param order   Gives the polinomia order to be used if the secondary species
#'                information needs to be corrected due to missing data.
#' @param mode    Operation mode of the membranse system. Only \code{'batch'}
#'                and \code{'continuous'} allowed. For semicontinuous systems
#'                the separation factor is calculated as for continuous
#'                systems.
#' @inheritParams transPlot
#' @return Data frame with two variables: Time in the same units as provided
#'         data and SF with the separation factors at each time.
#' @import ggplot2
#' @examples
#'   data(seawaterLiNaK)
#'   sepfactor(main = seawaterLiNaK$Lithium.1,
#'             secon = seawaterLiNaK$Sodium.1)
#'   sepfactor(main = seawaterLiNaK$Lithium.1,
#'             secon = seawaterLiNaK$Potassium.1)
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export


sepfactor <- function (main, secon, order = 2, mode = 'batch', plot = TRUE) {
  SF <- NULL
  time_A <- main[which(main$Phase == "Strip"), 1]
  time_B <- secon[which(secon$Phase == "Strip"), 1]
  B_f <- secon[which(secon$Phase == "Feed"), 3]
  B_s <- secon[which(secon$Phase == "Strip"), 3]
  A_s <- main[which(main$Phase == "Strip"), 3]
  A_f <- main[which(main$Phase == "Feed"), 3]

  if (any(B_s < (B_f[1] - B_f))) B_s <- B_f[1] - B_f

  if (length(time_A) > length(time_B)) {
    message('Due to missing secondary species values at some times,
  this information was interpolated and used to produce the
  separation factor results.')
    B_s <- fixSecondary(conc = B_s, time = time_B, compTime = time_A,
                        order = order)
    B_f <- fixSecondary(conc = B_f, time = time_B, compTime = time_A,
                        order = order)
  }

  if (mode == 'batch') {
    Sf <- data.frame(time = time_A, SF = (A_s / B_s) / (A_f[1] / B_f[1]))
  } else {
    Sf <- data.frame(time = time_A, SF = (A_s / B_s) / (A_f / B_f))
  }
  if (plot) {
    p <- ggplot(data = Sf, aes(x = time, y = SF)) + geom_point() +
      geom_smooth(method = 'loess', formula = 'y ~ x',
                  se = FALSE, color = 'black') +
      theme_bw() +  labs(y = 'Separation factor', x = 'Time') +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(color = "black"),
            axis.text.y = element_text(color = "black"))
    print(p)
  }
  return(Sf)
}
