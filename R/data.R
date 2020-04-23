#' External standard calibration curve for lithium in water.
#'
#' A dataset containing the concentrations and emission signals of aqueous
#' lithium standards measured by Flame Atomic Emission Spectrometry (FAES) at a
#' Perkin-Elmer 3100 Atomic Absorption Spectrometer.
#'
#' @format A data frame with 8 rows and 2 variables:
#' \describe{
#'   \item{Conc}{lithium concentration in the standards, in mg/kg}
#'   \item{Signal}{emission signal of lithium at 670.8 nm, in arbitrary units}
#' }
#' @source
#'   C. Paredes, E. Rodriguez de San Miguel, Polymer inclusion membrane for
#'   the recovery and concentration of lithium from seawater. Master thesis,
#'   Universidad Nacional Autónoma de México, México City, México, 2020.
"curvelithium"

#' Bivariated calibration plane for lithium in prescence of sodium.
#'
#' A dataset containing the concentrations of lithium and sodium combined
#' standards and absorbance signals measured by Flame Atomic Absorption
#' Spectrometry (FAAS) at a Perkin-Elmer 3100 Atomic Absorption Spectrometer.
#'
#' @format A data frame with 40 rows and 3 variables:
#' \describe{
#'   \item{Conc}{lithium concentration in the standards, in mg/kg}
#'   \item{Signal}{absorbance signal of lithium at 670.8 nm, in absorbance
#'   units}
#'   \item{Conc.S}{sodium concentration in the standards, in mg/kg}
#' }
#' @source
#'   C. Paredes, E. Rodriguez de San Miguel, Polymer inclusion membrane for
#'   the recovery and concentration of lithium from seawater. Master thesis,
#'   Universidad Nacional Autónoma de México, México City, México, 2020.
"planelithium"

#' Membrane reuse capability to transport lithium
#'
#' A list of 10 datasets, each of one with the transport data of each
#' cycle in a reuse capability experiment of a polimeric inclusion membrane
#' selective to lithium ions.
#' @format A list of 10 data frames with 10 rows and 3 variables:
#' \describe{
#'   \item{Time}{Time in hours of each aliquot taken during the cycle}
#'   \item{Phase}{Phase of corresponding aliquot, Feed or Strip}
#'   \item{Fraction}{Remaining lithium fraction in the feed solution or
#'   transported lithium fraction to the strip solution}
#' }
#' @source
#'   C. Paredes, E. Rodriguez de San Miguel, Polymer inclusion membrane for
#'   the recovery and concentration of lithium from seawater. Master thesis,
#'   Universidad Nacional Autónoma de México, México City, México, 2020.
"reusecycles"

#' Lithium concentration results using a membrane
#'
#' A list of 5 datasets, each of one with the transport data of each cycle in
#' a concentration experiment of lithium using a polymer inclusion membrane.
#' @format A list of 5 data frames with 10 rows and 3 variables:
#' \describe{
#'   \item{Time}{Time in hours of each aliquot taken during the experiment}
#'   \item{Phase}{Phase of corresponding aliquot, Feed or Strip}
#'   \item{Fraction}{Remaining lithium concentration in the feed solution or
#'   transported lithium concentration to the strip solution}
#' }
#' @source
#'   C. Paredes, E. Rodriguez de San Miguel, Polymer inclusion membrane for
#'   the recovery and concentration of lithium from seawater. Master thesis,
#'   Universidad Nacional Autónoma de México, México City, México, 2020.
"concentrationcycles"

#' Lithium, sodium and potassium transport profiles across a membrane
#'
#' A list of 6 datasets containing by duplicate the transport profiles for
#' lithium, sodium, and potassium from a synthetic simplified seawater matrix
#' using a polymer inclusion membrane selective to lithium. Lithium samples
#' were taken every 45 minutes during 4.5 hours while sodium and potassium
#' determinations were made in samples taken every 1.5 hours.
#' @format A list of 6 data frames (two for each lithium, sodium, and
#'         potassium) with 14 or 8 rows and 3 variables:
#' \describe{
#'   \item{Time}{Time in hours of each aliquot taken during the experiment}
#'   \item{Phase}{Phase of corresponding aliquot, Feed or Strip}
#'   \item{Fraction}{Remaining lithium concentration in the feed solution or
#'   transported lithium concentration to the strip solution}
#' }
#' @source
#'   C. Paredes, E. Rodriguez de San Miguel, Polymer inclusion membrane for
#'   the recovery and concentration of lithium from seawater. Master thesis,
#'   Universidad Nacional Autónoma de México, México City, México, 2020.
"seawaterLiNaK"

