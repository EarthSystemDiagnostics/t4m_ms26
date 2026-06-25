# R/plot_dualaxis_helpers.R
# Reusable plotting helpers for dual-axis snow-profile plots
# Used in Figure 2, Figure 5, etc.
make_plot_dualaxis <- function(df,
                               source_left  = "T19",
                               source_right = "ERA5",
                               colors       = c("T19"="steelblue", "ERA5"="firebrick"),
                               ylab_left    = expression(delta^{18}*O~"\u2030"),
                               ylab_right   = "Proxy",
                               year_ticks   = NULL,
                               show_year_labels = TRUE) {
  
  dat_left  <- df %>% dplyr::filter(source == source_left)
  dat_right <- df %>% dplyr::filter(source == source_right)
  
  # Statistik für lineare Transformation
  m1 <- mean(dat_left$d18O, na.rm=TRUE);   s1 <- sd(dat_left$d18O, na.rm=TRUE)
  m2 <- mean(dat_right$proxy, na.rm=TRUE); s2 <- sd(dat_right$proxy, na.rm=TRUE)
  
  dat_right <- dat_right %>%
    dplyr::mutate(proxy_trans = (proxy - m2) * (s1/s2) + m1)
  
  p <- ggplot() +
    geom_line(data = dat_left,
              aes(x = depth, y = d18O, colour = source_left),
              linewidth = 0.9) +
    geom_line(data = dat_right,
              aes(x = depth, y = proxy_trans, colour = source_right),
              linewidth = 0.9) +
    scale_y_continuous(
      name = ylab_left,
      sec.axis = sec_axis(~ (. - m1) / (s1/s2) + m2, name = ylab_right)
    ) +
    scale_colour_manual(values = colors) +
    labs(x = "Snow depth (m)", colour = "") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "top",
      legend.title     = element_blank(),
      axis.title.y       = element_text(color = colors[[source_left]]),
      axis.text.y        = element_text(color = colors[[source_left]]),
      axis.title.y.right = element_text(color = colors[[source_right]]),
      axis.text.y.right  = element_text(color = colors[[source_right]])
    )
  
  # ==== Year axis emulation ====
  if (!is.null(year_ticks) && "date" %in% names(df)) {
    
    chron <- dplyr::bind_rows(
      dat_left  %>% dplyr::select(depth, date),
      dat_right %>% dplyr::select(depth, date)
    ) %>%
      dplyr::distinct(depth, date) %>%
      dplyr::mutate(year_dec = lubridate::decimal_date(date)) %>%
      dplyr::arrange(year_dec)
    
    # years available -> tick every year
    y_min <- floor(min(chron$year_dec, na.rm = TRUE))
    y_max <- ceiling(max(chron$year_dec, na.rm = TRUE))
    years_all <- seq(y_min, y_max, by = 1)
    
    # positions for all-year ticks
    pos_ticks_all <- approx(x = chron$year_dec, y = chron$depth,
                            xout = years_all, rule = 2, ties = "ordered")$y
    labdat_ticks <- data.frame(depth = pos_ticks_all, year = years_all)
    
    # positions for labeled years only
    pos_ticks_lbl <- approx(x = chron$year_dec, y = chron$depth,
                            xout = year_ticks, rule = 2, ties = "ordered")$y
    labdat_lbl <- data.frame(depth = pos_ticks_lbl, year = year_ticks)
    
    rng_y <- range(c(dat_left$d18O, dat_right$proxy_trans), na.rm = TRUE)
    dy  <- diff(rng_y)
    y0  <- max(rng_y, na.rm = TRUE)
    
    # tick marks: every year
    p <- p +
      geom_segment(data = labdat_ticks,
                   aes(x = depth, xend = depth,
                       y = y0, yend = y0 + 0.04*dy),
                   inherit.aes = FALSE)
    
    # labels only for selected years
    if (show_year_labels) {
      p <- p +
        geom_text(data = labdat_lbl,
                  aes(x = depth, y = y0 + 0.06*dy, label = year),
                  inherit.aes = FALSE, vjust = 0, size = 3) +
        annotate("text",
                 x = mean(range(dat_left$depth, na.rm = TRUE)),
                 y = y0 + 0.11*dy,
                 label = "Year", fontface = "bold", vjust = 0)
    }
    
    p <- p +
      coord_cartesian(ylim = c(rng_y[1], y0 + 0.15*dy), clip = "off") +
      theme(plot.margin = margin(5.5, 5.5, 20, 5.5))
  }
  
  p
}


add_year_axis <- function(p, df_all, source_left, source_right,
                          year_ticks,
                          label_years = TRUE,
                          tick_years = NULL,
                          y_limits) {
  
  dat_left  <- df_all %>% dplyr::filter(source == source_left)
  dat_right <- df_all %>% dplyr::filter(source == source_right)
  
  chron <- dplyr::bind_rows(
    dat_left  %>% dplyr::select(depth, date),
    dat_right %>% dplyr::select(depth, date)
  ) %>%
    dplyr::distinct(depth, date) %>%
    dplyr::mutate(
      date = as.POSIXct(date, tz = "UTC"),
      year_dec = lubridate::decimal_date(date)
    ) %>%
    dplyr::arrange(year_dec)
  
  if (is.null(tick_years)) {
    y_min <- floor(min(chron$year_dec, na.rm = TRUE))
    y_max <- ceiling(max(chron$year_dec, na.rm = TRUE))
    tick_years <- seq(y_min, y_max, by = 1)
  }
  
  pos_ticks_all <- approx(chron$year_dec, chron$depth, xout = tick_years,
                          rule = 2, ties = "ordered")$y
  labdat_ticks <- data.frame(depth = pos_ticks_all, year = tick_years)
  
  pos_ticks_lbl <- approx(chron$year_dec, chron$depth, xout = year_ticks,
                          rule = 2, ties = "ordered")$y
  labdat_lbl <- data.frame(depth = pos_ticks_lbl, year = year_ticks)
  
  # ---- draw INSIDE the y-limits (otherwise removed by scale limits) ----
  dy <- diff(y_limits)
  y_top <- y_limits[2]
  
  y_tick0 <- y_top - 0.06 * dy         # base of tick (inside)
  y_tick1 <- y_top - 0.02 * dy         # top of tick (still inside)
  y_text  <- y_top - 0.10 * dy         # label position (inside)
  y_title <- y_top - 0.16 * dy         # "Year" label (inside)
  
  p <- p +
    geom_segment(
      data = labdat_ticks,
      aes(x = depth, xend = depth, y = y_tick0, yend = y_tick1),
      inherit.aes = FALSE
    )
  
  if (label_years) {
    p <- p +
      geom_text(
        data = labdat_lbl,
        aes(x = depth, y = y_text, label = year),
        inherit.aes = FALSE,
        vjust = 0.5, size = 3
      ) +
      annotate(
        "text",
        x = mean(range(dat_left$depth, na.rm = TRUE)),
        y = y_title,
        label = "Year",
        fontface = "bold"
      )
  }
  
  p
}

make_plot_annual_dualaxis <- function(df,
                                      source_left,
                                      source_right,
                                      colors,
                                      ylab_left,
                                      ylab_right,
                                      STARTYEAR = NULL) {
  
  library(dplyr)
  library(ggplot2)
  
  dat <- df %>%
    filter(source %in% c(source_left, source_right))
  
  if (!is.null(STARTYEAR)) {
    dat <- dat %>% filter(year >= STARTYEAR)
  }
  
  
  left  <- dat %>% filter(source == source_left)
  right <- dat %>% filter(source == source_right)
  
  
  
  m1 <- mean(left$d18O, na.rm = TRUE)
  s1 <- sd(left$d18O, na.rm = TRUE)
  m2 <- mean(right$proxy, na.rm = TRUE)
  s2 <- sd(right$proxy, na.rm = TRUE)
  
  right <- right %>%
    mutate(proxy_scaled = (proxy - m2) * (s1 / s2) + m1)
  
  
  
  ggplot() +
    
    geom_line(data = left,
              aes(x = year, y = d18O, color = source_left),
              linewidth = 1) +
    
    geom_line(data = right,
              aes(x = year, y = proxy_scaled, color = source_right),
              linewidth = 1) +
    
    scale_color_manual(values = colors) +
    
    
    scale_y_continuous(
      name = ylab_left,
      sec.axis = sec_axis(~ (. - m1) / (s1 / s2) + m2, name = ylab_right)
    ) +
    
    labs(x = NULL, color = NULL) +
    
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom"
    )
}