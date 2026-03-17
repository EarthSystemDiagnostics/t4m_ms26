
# scripts/21_fig02.R
source(here::here("scripts", "01_load_inputs.R"))
source(here::here("R", "plot_dualaxis_helpers.R"))
library(dplyr)
library(patchwork)
library(ggplot2)

dir.create(here::here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

df_all <- readRDS(here::here("output", "sims", "fig02_df_all.rds"))

year_ticks <- c(1998, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2019)
ylab_left <- expression("T4M" ~~ delta^{18}*O ~ "(" * "\u2030" * ")")


colors <- c(
  "T4M"                 = "black",
  "AWS9 t2m precip ERA" = "steelblue",
  "ECHAM6 t2m"          = "red",
  "ECHAM6 d18O"         = "firebrick"
)

set_left_y_scale_inplace <- function(p, breaks, limits) {
  ys <- p$scales$get_scales("y")
  if (is.null(ys)) return(p)
  ys$breaks <- breaks
  p + coord_cartesian(ylim = limits)
}

# common left y scale from T4M
y_left_rng    <- range(df_all$d18O[df_all$source == "T4M"], na.rm = TRUE)
y_left_breaks <- scales::pretty_breaks(n = 5)(y_left_rng)
y_left_limits <- range(y_left_breaks)

g.aws <- make_plot_dualaxis(
  df_all,
  source_left  = "T4M",
  source_right = "AWS9 t2m precip ERA",
  colors = colors,
  ylab_left  = ylab_left,
  ylab_right = "based on AWS t2m (°C)",
  year_ticks = NULL,
  show_year_labels = TRUE
)



g.echamT <- make_plot_dualaxis(
  df_all,
  source_left  = "T4M",
  source_right = "ECHAM6 t2m",
  colors = colors,
  ylab_left  = ylab_left,
  ylab_right = "based on ECHAM6 t2m (°C)",
  year_ticks = NULL,
  show_year_labels = FALSE
)

g.echamd18O <- make_plot_dualaxis(
  df_all,
  source_left  = "T4M",
  source_right = "ECHAM6 d18O",
  colors = colors,
  ylab_left  = ylab_left,
  ylab_right = expression("based on ECHAM6" ~~ delta^{18}*O ~ "(" * "\u2030" * ")"),
  year_ticks = year_ticks,
  show_year_labels = FALSE
)

# enforce identical left y axis across panels
g.aws       <- set_left_y_scale_inplace(g.aws,       y_left_breaks, y_left_limits)
g.echamT    <- set_left_y_scale_inplace(g.echamT,    y_left_breaks, y_left_limits)
g.echamd18O <- set_left_y_scale_inplace(g.echamd18O, y_left_breaks, y_left_limits)

g.aws <- add_year_axis(
  g.aws, df_all,
  source_left = "T4M",
  source_right = "AWS9 t2m precip ERA",
  year_ticks = year_ticks,
  label_years = TRUE,
  y_limits = y_left_limits
)
ga <- g.aws + 
  labs(tag = "a") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    plot.margin  = margin(5.5, 12, 0, 5.5)
  )

gt <- g.echamT + 
  labs(tag = "b") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    plot.margin  = margin(0, 12, 0, 5.5)
  )

go <- g.echamd18O + 
  labs(tag = "c") +
  theme(
    plot.margin = margin(0, 12, 5.5, 5.5)
  )

p <- (ga / gt / go) +
  plot_layout(ncol = 1, axes = "collect", guides = "collect") &
  theme(
    legend.position   = "none",
    plot.tag          = element_text(size = 11, face = "plain"),
    plot.tag.position = c(0.01, 0.98)
  )



ggsave(
  here::here("output", "figures", "Figure2.pdf"),
  p, width = 7.2, height = 9.0, units = "in",
  device = cairo_pdf
)
ggsave(
  here::here("output", "figures", "Figure2.svg"),
  p, width = 7.2, height = 9.0, units = "in"
)


### Correlation for the papers:
cor_pair_depth <- function(df, left_source, left_var, right_source, right_var,
                           method = "pearson") {
  
  x <- df %>%
    dplyr::filter(source == left_source) %>%
    dplyr::select(depth, x = dplyr::all_of(left_var))
  
  y <- df %>%
    dplyr::filter(source == right_source) %>%
    dplyr::select(depth, y = dplyr::all_of(right_var))
  
  dat <- dplyr::inner_join(x, y, by = "depth") %>%
    dplyr::filter(is.finite(x), is.finite(y))
  
  if (nrow(dat) < 3) {
    return(tibble::tibble(
      left_source = left_source, left_var = left_var,
      right_source = right_source, right_var = right_var,
      n = nrow(dat), r = NA_real_, p_value = NA_real_
    ))
  }
  
  ct <- stats::cor.test(dat$x, dat$y, method = method)
  
  tibble::tibble(
    left_source  = left_source,
    left_var     = left_var,
    right_source = right_source,
    right_var    = right_var,
    n       = nrow(dat),
    r       = unname(ct$estimate),
    p_value = ct$p.value
  )
}

cor_stats <- dplyr::bind_rows(
  cor_pair_depth(df_all, "T4M", "d18O", "AWS9 t2m precip ERA", "proxy"),
  cor_pair_depth(df_all, "T4M", "d18O", "ECHAM6 t2m",          "proxy"),
  cor_pair_depth(df_all, "T4M", "d18O", "ECHAM6 d18O",         "proxy"),
  cor_pair_depth(df_all, "ECHAM6 t2m", "proxy", "ECHAM6 d18O",         "proxy"),
) %>%
  dplyr::mutate(r = round(r, 3), p_value = signif(p_value, 3))

print(cor_stats)
