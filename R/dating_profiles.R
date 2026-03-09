#R/dating_profiles.R

#' Date a simulated firn profile using an age–depth model
#'
#' Applies an age–depth model to a FirnR simulation output by interpolating
#' depths from decimal years and attaching sample dates.
#'
#' @param sim A data.frame returned by \code{FirnR::SimProfile()}.
#'            Must contain a \code{time} column.
#' @param agemodel A data.frame with columns \code{year} (decimal years)
#'                 and \code{depth} (m).
#'
#' @return The input simulation data.frame with additional columns:
#' \itemize{
#'   \item \code{depth.agemodel}: depth derived from the age–depth model
#'   \item \code{date}: sample date (copied from \code{time})
#' }
#'
#' @details
#' No binning or aggregation is performed. This function only assigns depths
#' and dates based on the supplied age–depth model.
DateSimByAgeModel <- function(sim, agemodel) {
  dec_years <- lubridate::year(sim$time) +
    (lubridate::yday(sim$time) - 1) /
    (365 + lubridate::leap_year(sim$time))
  
  sim$depth.agemodel <- approx(
    x = agemodel$year,
    y = agemodel$depth,
    xout = dec_years
  )$y
  
  sim$date <- sim$time
  sim
}


#' Date a measured trench isotope profile using an age–depth model
#'
#' This function assigns deposition dates to a measured trench profile
#' (`t4m`) by interpolating its depths into the provided age–depth
#' relationship (`agemodel`). The output is a tibble that matches the
#' structure of simulated profiles from \code{SimAndDateProfile}, i.e.
#' including \code{depth}, \code{d18O}, \code{date}, and \code{source}.
#'
#' @param t4m A data.frame or tibble containing the trench profile with
#'   \code{depth} (bin centers, e.g. 0–3 cm → 1.5) and \code{d18O}.
#' @param agemodel A data.frame with age–depth information, containing
#'   columns \code{year} (decimal year) and \code{depth}.
#' @param source Optional character string naming the data source
#'   (default "T4M").
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{depth}{Numeric vector of depth bin centers (same as \code{t4m$depth})}
#'     \item{d18O}{Numeric vector of measured isotope values}
#'     \item{date}{Calendar date corresponding to each depth (converted from decimal year)}
#'     \item{source}{Character label of the profile ("T4M" by default)}
#'   }
#'
#' @examples
#' \dontrun{
#' t4m_dated <- TrenchWithDates(t4m, agemodel, source = "T4M")
#' ggplot2::ggplot(t4m_dated, ggplot2::aes(x = depth, y = d18O)) +
#'   ggplot2::geom_line()
#' }
TrenchWithDates <- function(t4m, agemodel, source = "T4M") {
  # interpolate decimal years for trench depths
  dec_years <- approx(
    x = agemodel$depth,
    y = agemodel$year,
    xout = t4m$depth
  )$y
  
  # convert decimal year → Date
  trench_dates <- lubridate::date_decimal(dec_years)
  
  tibble::tibble(
    depth  = t4m$depth,
    d18O   = t4m$d18O,
    date   = trench_dates,
    source = source
  )
}

