# scripts/22_fig03.R
source(here::here("scripts", "01_load_inputs.R"))

set.seed(200) # For the MC intervals

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(MethComp)

dir.create(here::here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# needs df_all from Fig2 sims
df_all <- readRDS(here::here("output", "sims", "fig02_df_all.rds"))

# ---- annual means (interannual only) from binned trench/sims ----
df_annual <- df_all %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(source, year) %>%
  summarise(
    d18O  = mean(d18O,  na.rm = TRUE),
    proxy = mean(proxy, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(year), year < 2019)

# ============================================================
# Figure 3A: annual dual-axis panels
# Keep only:
#   - T19 vs AWS
#   - T19 vs ECHAM6 d18O
# ============================================================

colors <- c(
  "T19"                 = "black",
  "AWS9 t2m precip ERA" = "steelblue",
  "ECHAM6 t2m"          = "red",
  "ECHAM6 d18O"         = "firebrick"
)

ylab_left <- expression("T19" ~~ delta^{18}*O ~ "(" * "\u2030" * ")")


g.aws.annual <- make_plot_annual_dualaxis(
  df_annual,
  source_left  = "T19",
  source_right = "AWS9 t2m precip ERA",
  colors       = colors,
  ylab_left    = ylab_left,
  ylab_right   = "based on AWS t2m (°C)",
  STARTYEAR    = 1998
)

g.echamd18O.annual <- make_plot_annual_dualaxis(
  df_annual,
  source_left  = "T19",
  source_right = "ECHAM6 d18O",
  colors       = colors,
  ylab_left    = ylab_left,
  ylab_right   = expression("based on ECHAM6 " * delta^{18} * O ~ "(" * "\u2030" * ")"),
  STARTYEAR    = 1998
)

ga <- g.aws.annual +
  labs(tag = "a") +
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    plot.margin  = margin(5.5, 12, 0, 5.5),
    axis.title.y.left  = element_text(color = colors["T19"]),
    axis.text.y.left   = element_text(color = colors["T19"]),
    axis.ticks.y.left  = element_line(color = colors["T19"]),
    axis.title.y.right = element_text(color = colors["AWS9 t2m precip ERA"]),
    axis.text.y.right  = element_text(color = colors["AWS9 t2m precip ERA"]),
    axis.ticks.y.right = element_line(color = colors["AWS9 t2m precip ERA"])
  )

go <- g.echamd18O.annual +
  labs(tag = "b") +
  theme(legend.position = "none") +
  theme(
    plot.margin = margin(0, 12, 5.5, 5.5),
    axis.title.y.left  = element_text(color = colors["T19"]),
    axis.text.y.left   = element_text(color = colors["T19"]),
    axis.ticks.y.left  = element_line(color = colors["T19"]),
    axis.title.y.right = element_text(color = colors["ECHAM6 d18O"]),
    axis.text.y.right  = element_text(color = colors["ECHAM6 d18O"]),
    axis.ticks.y.right = element_line(color = colors["ECHAM6 d18O"])
  )


p_left <- (ga / go) +
  plot_layout(ncol = 1, axes = "collect", guides = "collect") &
  theme(legend.position = "bottom")


run_cor <- function(data, x, y, label) {
  ok <- complete.cases(data[[x]], data[[y]])
  
  x_ok <- data[[x]][ok]
  y_ok <- data[[y]][ok]
  
  if (length(x_ok) < 3) {
    cat(sprintf(
      "%s: n = %d (too few data)\n",
      label, length(x_ok)
    ))
    return(invisible(NULL))
  }
  
  ct <- cor.test(x_ok, y_ok)
  
  cat(sprintf(
    "%s: r = %.3f, p = %.4g, n = %d, mean_x = %.2f, mean_y = %.2f\n",
    label,
    unname(ct$estimate),
    ct$p.value,
    length(x_ok),
    mean(x_ok, na.rm = TRUE),
    mean(y_ok, na.rm = TRUE)
  ))
  
  invisible(ct)
}


df_annual_wide <- df_annual %>%
  select(source, year, d18O, proxy) %>%
  tidyr::pivot_wider(
    names_from  = source,
    values_from = c(d18O, proxy)
  )

cor_aws <- run_cor(
  df_annual_wide,
  x = "d18O_T19",
  y = "proxy_AWS9 t2m precip ERA",
  label = "T19 vs AWS9 t2m precip ERA"
)
cor_ech_d18O <- run_cor(
  df_annual_wide,
  x = "d18O_T19",
  y = "proxy_ECHAM6 t2m",
  label = "T19 vs ECHAM6 t2m"
)

cor_ech_d18O <- run_cor(
  df_annual_wide,
  x = "d18O_T19",
  y = "proxy_ECHAM6 d18O",
  label = "T19 vs ECHAM6 d18O"
)

# ============================================================
# Figure 3B/C: scatter (annual)
# ============================================================

ylab_t4m <- expression("T19" ~~ delta^{18}*O ~ "(" * "\u2030" * ")")

xlab_echam_d18O <- expression("based on ECHAM6 " * delta^{18} * O ~ "(" * "\u2030" * ")")
xlab_aws <- "based on AWS t2m (°C)"

# Deming scatter expects a "depth" column -> reuse year as depth
df_annual2 <- df_annual %>% rename(depth = year)

make_scatter_data <- function(df, x_name, y_name,
                              x_var = "d18O", y_var = "proxy") {
  
  dat <- df %>%
    filter(source %in% c(x_name, y_name)) %>%
    mutate(value = case_when(
      source == x_name ~ .data[[x_var]],
      source == y_name ~ .data[[y_var]]
    )) %>%
    select(depth, source, value) %>%
    pivot_wider(names_from = source, values_from = value) %>%
    drop_na()
  
  points <- dat %>%
    rename(x = !!rlang::sym(x_name), y = !!rlang::sym(y_name))
  
  list(points = points)
}

plot_scatter_deming <- function(sc, xlab, ylab, vr = 1,
                                conf_level = 0.90, n_boot = 10000, col = "red",
                                expand_frac = -0.2) {
  
  alpha <- 1 - conf_level
  
  fit <- MethComp::Deming(
    x = sc$points$x,
    y = sc$points$y,
    vr = vr,
    boot = n_boot,
    keep.boot = TRUE,
    alpha = alpha
  )
  
  boots <- as.data.frame(fit)
  colnames(boots) <- c("Intercept", "Slope", "sigma_x", "sigma_y")
  
  intercept <- mean(boots$Intercept, na.rm = TRUE)
  slope     <- mean(boots$Slope, na.rm = TRUE)
  
  pred_x <- seq(min(sc$points$x, na.rm = TRUE),
                max(sc$points$x, na.rm = TRUE),
                length.out = 200)
  
  pred_y <- sapply(seq_len(nrow(boots)), function(i) {
    boots$Intercept[i] + boots$Slope[i] * pred_x
  })
  
  alpha_half <- (1 - conf_level) / 2
  pred_df <- data.frame(
    x = pred_x,
    y = intercept + slope * pred_x,
    ymin = apply(pred_y, 1, quantile, probs = alpha_half, na.rm = TRUE),
    ymax = apply(pred_y, 1, quantile, probs = 1 - alpha_half, na.rm = TRUE)
  )
  
  x_rng <- range(sc$points$x, pred_df$x, na.rm = TRUE)
  y_rng <- range(sc$points$y, pred_df$y, pred_df$ymin, pred_df$ymax, na.rm = TRUE)
  
  span <- max(diff(x_rng), diff(y_rng))
  pad  <- span * expand_frac
  
  x_mid <- mean(x_rng)
  y_mid <- mean(y_rng)
  
  x_lim <- c(x_mid - span / 2 - pad, x_mid + span / 2 + pad)
  y_lim <- c(y_mid - span / 2 - pad, y_mid + span / 2 + pad)
  
  ggplot() +
    theme_minimal(base_size = 12) +
    geom_point(data = sc$points, aes(x = x, y = y), alpha = 1, size = 2) +
    geom_ribbon(
      data = pred_df,
      aes(x = x, ymin = ymin, ymax = ymax),
      fill = col, alpha = 0.15
    ) +
    geom_line(
      data = pred_df,
      aes(x = x, y = y),
      color = col, linewidth = 1
    ) +
    labs(x = xlab, y = ylab) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

# --- T19 vs AWS (annual) ---
sc_aws.ann <- make_scatter_data(
  df_annual2,
  y_name = "T19",
  x_name = "AWS9 t2m precip ERA",
  y_var  = "d18O",
  x_var  = "proxy"
)

r2_aws <- cor(sc_aws.ann$points$x, sc_aws.ann$points$y)^2

p_sc_aws <- plot_scatter_deming(
  sc_aws.ann,
  ylab = ylab_t4m,
  xlab = xlab_aws,
  vr   = 1,
  col  = colors[["AWS9 t2m precip ERA"]]
) +
  annotate("text", x = -Inf, y = Inf,
           label = sprintf("R^2 == %.2f", r2_aws), parse = TRUE,
           hjust = -0.25, vjust = 1.6, size = 3.5) +
  labs(tag = "c") +
  theme(plot.margin = margin(5.5, 5.5, 10, 5.5))


# --- T19 vs ECHAM6 d18O (annual) ---
sc_echam.ann <- make_scatter_data(
  df_annual2,
  y_name = "T19",
  x_name = "ECHAM6 d18O",
  y_var  = "d18O",
  x_var  = "proxy"
)

offset <- mean(sc_echam.ann$points$y) - mean(sc_echam.ann$points$x)

r2_echam <- cor(sc_echam.ann$points$x, sc_echam.ann$points$y)^2

p_sc_echam <- plot_scatter_deming(
  sc_echam.ann,
  ylab = ylab_t4m,
  xlab = xlab_echam_d18O,
  vr   = 1,
  col  = colors[["ECHAM6 d18O"]]
) +
  geom_abline(
    slope = 1, intercept = offset,
    color = "darkred", linetype = "dashed"
  ) +
  annotate("text", x = -Inf, y = Inf,
           label = sprintf("R^2 == %.2f", r2_echam), parse = TRUE,
           hjust = -0.25, vjust = 1.6, size = 3.5) +
  labs(tag = "d") +
  theme(plot.margin = margin(10, 5.5, 5.5, 5.5))

p_right <- p_sc_aws / patchwork::plot_spacer() / p_sc_echam +
  plot_layout(heights = c(1, 0.1, 1))

# ============================================================
# combined figure
# ============================================================

p_left  <- (ga / go) + plot_layout(heights = c(1, 1))
p_right <- (p_sc_aws / p_sc_echam) + plot_layout(heights = c(1, 1))

p_fig3 <- p_left | p_right +
  plot_layout(widths = c(3.25, 3))


ggsave(
  here::here("output", "figures", "Figure3.pdf"),
  p_fig3, width = 7.5, height = 6.5, units = "in",
  device = cairo_pdf
)

ggsave(
  here::here("output", "figures", "Figure3.svg"),
  p_fig3, width = 7.5, height = 6.5, units = "in"
)