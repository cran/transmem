#' Fits trend equations that model transport profiles
#'
#' Given a transport profile dataset, the results may be studied and
#' compared in terms of empirical functions that describe the transport
#' process in terms of regression parameters that can be asociated with
#' the performance of the membrane system. The parameters are obtained by
#' non-linear regression and are independent for each solution at both sides
#' of the membrane. This is particularly useful when performing system
#' optimizations since the parameters can be used as response variables
#' depending on the optimization goal.
#'
#' Two empirical equations have been implemented in the function. In the
#' \code{'rodriguez'} model (Rodriguez de San Miguel et al., 2014), the
#' fractions (\eqn{\Phi}) in feed or
#' strip phases as a function of time (\eqn{t}) are fitted to
#' \deqn{\Phi(t)=Ae^{-t/d}+y_0} where \eqn{A}, \eqn{d} and \eqn{y_0} are the
#' parameters to be found. In this model, parameter \eqn{d} determines the
#' steepness of the species concentration change in time, \eqn{y_0} reflects
#' the limiting value to which the profiles tend to at long pertraction
#' times and \eqn{A} is not supposed to play an important role in the
#' transport description. The parameters of each phase are summarized in
#' the functions \eqn{G_{feed}} and \eqn{G_{strip}} for the feed and
#' strip phases: \deqn{G_{feed}=\frac{1}{y_0d},\qquad G_{strip}=\frac{y_0}{d}}
#' The bigger each \eqn{G} function, the better the transport process.
#'
#' In the \code{'paredes'} model (Paredes and Rodriguez de San Miguel, 2020),
#' the transported fractions to the strip solution and from the feed solution
#' are adjusted to the equations:
#' \deqn{\Phi_s(t)=\frac{\alpha_s t^\gamma}{\beta_s^{-1}+t^\gamma}}
#' \deqn{\Phi_f(t)=1-\frac{\alpha_f t^\gamma}{\beta_f^{-1}+t^\gamma}}
#' respectively. In those equations, adjustable parameters \eqn{\alpha} and
#' \eqn{\beta} relates the
#' maximum fraction transported at long pertraction times and the steepness
#' of the concentration change, respectively. \eqn{\gamma} is an excentricity
#' factor to improve the adjustment and does not need to be changed for
#' systems under similar conditions. The subscripts \eqn{s} and \eqn{f}
#' means strip and feed phases, respectively.
#'
#' The later model has the disadvantage over the former that the equation
#' to use depends on the phase to be modeled but has the great advantage
#' that if no significant accumulation is presented in the membrane,
#' the parameters \eqn{\alpha} and \eqn{\beta} should be quite similar for
#' both phases and a consensus value can be obtained in various simple ways,
#' while the other model yields quite diferent parameters for each phase.
#' Paredes parameters are combined by using meta-analysis tools that consider
#' the associated uncertainty of each one due to lack of fit to get summarized,
#' lower-uncertainty results. Besides, once the \eqn{\gamma} parameter
#' has been chosen, the later model uses only two parameters and while
#' comparing models with similar performance, the simpler the better.
#' @example
#'   data(seawaterLiNaK)
#'   model1 <- transTrend(trans = seawaterLiNaK$Lithium.1)
#'   model2 <- transTrend(trans = seawaterLiNaK$Lithium.1,
#'                        model = 'rodriguez')
#'   print(model1)
#'   print(model2)
#' @references
#'   E. Rodriguez de San Miguel, X. Vital, J. de Gyves, Cr(vi) transport via a
#'   sup ported ionic liquid membrane containing cyphos il101 as carrier:
#'   System analysis and optimization through experimental design strategies,
#'   Journal of Hazardous Materials 273 (2014) 253 - 262.\cr
#'   doi:10.1016/j.jhazmat.2014.03.052.
#'
#'   C. Paredes, E. Rodriguez de San Miguel, Polymer inclusion membrane for
#'   the recovery and concentration of lithium from seawater. Master thesis,
#'   Universidad Nacional Autónoma de México, México City, México, 2020.
#' @param model  Model to be used in the regression. Default to
#'               \code{'paredes'} but \code{'rodriguez'} also allowed.
#'               See details.
#' @param eccen  Eccentricity factor (\eqn{\gamma}) for the model when
#'               \code{model} is set to \code{'paredes'}.
#' @inheritParams transPlot
#' @return A list of 4 or 5 components (depending on the model chosen) with
#'         the regression information for each phase, the eccentricity factor
#'         (only in Paredes model), the name of the model used, and the
#'         sumarized results of the regression: \eqn{G_{feed}} and
#'         \eqn{G_{strip}} values for the Rodriguez model or summarized
#'         \eqn{\alpha} and \eqn{\beta} parameters with asocciated uncertainty
#'         for the Paredes model.
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Eduardo Rodriguez de San Miguel, \email{erdsmg@@unam.mx}
#' @export

transTrend <- function(trans, model = 'paredes', eccen = 1){
  name <- deparse(substitute(trans))

  if (grepl('Transport', name, ignore.case = TRUE)) {
    name <- gsub('Transport', 'nls', name, ignore.case = TRUE)
  } else {
    name <- paste0('nls', name)
  }

  Strip <- trans$Frac[which(trans$Phase == 'Strip')]
  Feed  <- trans$Frac[which(trans$Phase == 'Feed')]
  Time  <- unique(trans$Time)

  if (model == 'paredes') {
    nlsFeed   <- nls(Feed ~ 1 - (a * Time^eccen) / (1 / b + Time^eccen),
                     start = list(a = 1, b = 0.5))
    nlsStrip  <- nls(Strip ~ (a * Time^eccen) / (1 / b + Time^eccen),
                     start = list(a = 1, b = 0.5))
    nlsModels <- list(feed = nlsFeed, strip = nlsStrip, eccen = eccen,
                      model = 'paredes')
    alpha = (summary(nlsFeed)$coefficients[1, 1] /
               summary(nlsFeed)$coefficients[1, 2]^2 +
               summary(nlsStrip)$coefficients[1, 1] /
               summary(nlsStrip)$coefficients[1, 2]^2) /
               (summary(nlsFeed)$coefficients[1, 2]^-2 +
                 summary(nlsStrip)$coefficients[1, 2]^-2)

    SE_alpha = sqrt(1 / (summary(nlsFeed)$coefficients[1, 2]^-2 +
                           summary(nlsStrip)$coefficients[1, 2]^-2))

    beta  = (summary(nlsFeed)$coefficients[2, 1] /
               summary(nlsFeed)$coefficients[2, 2]^2 +
               summary(nlsStrip)$coefficients[2, 1] /
               summary(nlsStrip)$coefficients[2, 2]^2) /
               (summary(nlsFeed)$coefficients[2, 2]^-2 +
                  summary(nlsStrip)$coefficients[2, 2]^-2)

    SE_beta = sqrt(1 / (summary(nlsFeed)$coefficients[2, 2]^-2 +
                          summary(nlsStrip)$coefficients[2, 2]^-2))

    nlsModels$Result <- c(alpha, SE_alpha, beta, SE_beta)
    names(nlsModels$Result) <- c('alpha', 'SE_alpha', 'beta', 'SE_beta')
  }

  if (model == 'rodriguez') {
    # DOI: 10.1016/j.jhazmat.2014.03.052
    nlsFeed  <- nls(Feed ~ A * exp(- Time / d) + Yo,
                    start = list(A = 0.5, d = 0.5, Yo = 0))
    nlsStrip <- nls(Strip ~ A * exp(- Time / d) + Yo,
                    start = list(A = -0.5, d = 0.5, Yo = 1))
    nlsModels <- list(feed = nlsFeed, strip = nlsStrip, model = 'rodriguez')
    nlsModels$Result <- c(1 / (summary(nlsFeed)$coefficients[3, 1] *
                                 summary(nlsFeed)$coefficients[2, 1]),
                          summary(nlsStrip)$coefficients[3, 1] /
                            summary(nlsStrip)$coefficients[2, 1])
    names(nlsModels$Result) <- c('G_feed', 'G_strip')
  }

  return(nlsModels)
}
