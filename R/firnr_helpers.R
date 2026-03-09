# R/firnr_helpers.R

#' Rename FirnR::SimProfile output column d18O to a neutral name
#'
#' FirnR::SimProfile always returns the simulated signal in a column called
#' \code{d18O} even if the input represents temperature. This helper renames
#' that column to \code{signal}.
#'
#' @param sim A data.frame returned by \code{FirnR::SimProfile()}.
#' @return The same data.frame with column \code{signal} instead of \code{d18O}.
RenameFirnRSignal <- function(sim) {
  dplyr::rename(sim, signal = d18O)
}