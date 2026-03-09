#R/binning_profiles.R 
#functions: BinDatedSimToT4M()

#Helper
make_bin_edges_from_midpoints <- function(mid) {
  if (length(mid) < 2) stop("Need at least two depth midpoints.")
  
  half_steps <- diff(mid) / 2
  
  c(
    mid[1] - half_steps[1],
    mid[-length(mid)] + half_steps,
    mid[length(mid)] + half_steps[length(half_steps)]
  )
}


#' Bin a dated simulated profile to T4M trench resolution
#'
#' Aggregates a dated, high-resolution firn simulation to the depth resolution
#' of the measured T4M trench profile by binning using depth-bin midpoints.
#'
#' @param sim_dated A data.frame containing a dated firn simulation.
#'   Must include columns \code{depth.agemodel} (m) and \code{date}.
#' @param t4m A data.frame of measured T4M trench data containing a
#'   \code{depth} column with sample midpoints (m).
#' @param value_col Character. Name of the column in \code{sim_dated}
#'   to be averaged within depth bins (default: \code{"d18O"}).
#'
#' @return A tibble with one row per T4M depth bin containing:
#' \itemize{
#'   \item \code{depth}: T4M sample depth (midpoint, m)
#'   \item \code{proxy}: binned mean simulated proxy value
#'   \item \code{date}: mean deposition date of each bin
#' }
#'
#' @details
#' Depth bin edges are derived from the supplied T4M depth midpoints,
#' assuming contiguous sampling intervals. No padding or fixed layer
#' thickness is assumed.
BinDatedSimToT4M <- function(sim_dated, t4m, value_col = "d18O") {
  
  if (is.null(sim_dated$depth.agemodel))
    stop("sim_dated needs depth.agemodel (run DateSimByAgeModel first).")
  if (is.null(sim_dated$date))
    stop("sim_dated needs date (run DateSimByAgeModel first).")
  
  mid <- t4m$depth
  edges <- make_bin_edges_from_midpoints(mid)
  
  sim_dated %>%
    dplyr::mutate(
      bin = cut(
        depth.agemodel,
        breaks = edges,
        labels = mid,
        right = FALSE,
        include.lowest = TRUE
      )
    ) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      proxy = mean(.data[[value_col]], na.rm = TRUE),
      date  = as.Date(mean(as.numeric(date), na.rm = TRUE), origin = "1970-01-01"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(depth = as.numeric(as.character(bin))) %>%
    dplyr::select(depth, proxy, date)
}