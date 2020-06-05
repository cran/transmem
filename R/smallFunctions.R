# Functions for internal use

# Lists check
dfcheck <- function(df, param, fun){
  for (i in param) {
    if (length(eval(parse(text = paste0("df$", i)))) == 0) {
      stop(paste0("Provided data frame must have vectors ",
                  cat(paste0(param, ", ")),
                  " see ??", fun, " for details"))
    }
  }
}

# Colapse multi transport
transColapse <- function(trans){
  mtrans <- trans[[1]]
  frac <- matrix(ncol = length(trans), nrow = length(trans[[1]]$Time))
  for (i in 1:length(trans)) {
    frac[, i] <- trans[[i]]$Fraction
  }
  mtrans$SD <- apply(frac, 1, sd)
  mtrans$Fraction <- apply(frac, 1, mean)
  return(mtrans)
}


# Add secondary metal
AddSecondary <- function(secondary, p){
  sec.trend <- Time <- Fraction <- Phase <- span <- size <- NULL
  secondary$Phase <- paste0(secondary$Phase, ".")
  if (sec.trend == 'linear') {
    p <- p + scale_shape_identity() +
      geom_smooth(method = "lm", data = secondary, se = FALSE, size = 0.5,
                  aes(x = Time, y = Fraction, group = Phase, color = Phase))
  }
  if (sec.trend == 'spline') {
    p <- p + scale_shape_identity() +
      ggformula::geom_spline(data = secondary, spar = 0.7, size = 0.5,
                             aes(x = Time, y = Fraction, group = Phase,
                                 color = Phase))
  }
  if (sec.trend == 'logaritmic') { #Still under implementation
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
  p <- p + geom_point(data = secondary, size = size,
                      aes(x = Time, y = Fraction,
                          group = Phase, shape = 17, color = Phase))
  return(p)
}

xlimTrendWR <- function(x, trans){
  return(c(0, trans[[x]]$Time[length(trans[[x]]$Time)]))
}

AddParTrend <- function(trend, i, phase, e) {
  if (phase == 'strip') {
    return(
      function(x, i) {
        ((coefficients(trend[[i]]$strip)[1] * x^e)
        / (1/coefficients(trend[[i]]$strip)[2] + x^e))
      }
    )
  } else {
    return(
      function(x, i) {
        ((1 - (coefficients(trend[[i]]$feed)[1] * x^e)
         / (1/coefficients(trend[[i]]$feed)[2] + x^e)))
      }
    )
  }
}

plotCCurve <- function(curve, model = NULL, badpoint = NULL) {
  x <- NULL
  hpc <- rep(19, nrow(curve))

  if (!missing(badpoint)) hpc[badpoint] <- 21

  plot(curve$Signal ~ curve$Conc, pch = hpc)

  if (!missing(model)) {
    if (length(model$coefficients) == 2) {
      abline(model, col = 4)
    }
    if (length(model$coefficients) == 3) {
      curve(model$coefficients[1] + model$coefficients[2] * x +
              model$coefficients[3] * x^2, add = TRUE, col = 4)
    }
    if (length(model$coefficients) == 4) {
      curve(model$coefficients[1] + model$coefficients[2] * x +
              model$coefficients[3] * x^2 + model$coefficients[3] * x^3,
            add = TRUE, col = 4)
    }
  }
}

# Function not exported to the user as it is still under development.
# It is still possibly to use it by transmem:::extrMolRat()
extrMolRat <- function(mass = NULL, ratio = NULL, ex1 = NULL, ex2 = NULL,
                       mw1 = 246, mw2 = 348, rho1 = NULL, rho2 = NULL) {
  if (missing(mass) && missing(ratio)) {
    report <- cbind(TotalMass = ex1 + ex2,
                    MolarRatio <- (ex1 / mw1) / (ex2 / mw2))
  } else {
    MassRatio <- mw1 * ratio / mw2
    report    <- cbind(Extr.1 = (mass * MassRatio) / (1 + MassRatio),
                       Extr.2 = mass / (1 + MassRatio))
  }
  return(report)
}


