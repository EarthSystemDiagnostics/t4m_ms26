# scripts/22_fig03.R
source(here::here("scripts", "01_load_inputs.R"))


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
# Figure 3A: Annual dual-axis panels (T4M vs AWS / ECHAM T / ECHAM d18O)
# ============================================================

colors <- c(
  "T4M"                 = "black",
  "ECHAM6 t2m"           = "firebrick",
  "ECHAM6 d18O"          = "steelblue",
  "AWS9 t2m precip ERA"  = "red"
)

ylab_left <- expression("T4M" * delta^{18} * O ~ "\u2030")

g.aws.annual <- make_plot_annual_dualaxis(
  df_annual,
  source_left  = "T4M",
  source_right = "AWS9 t2m precip ERA",
  colors       = colors,
  ylab_left    = ylab_left,
  ylab_right   = "based on AWS t2m (°C)",
  STARTYEAR    = 1998
)

g.echamT.annual <- make_plot_annual_dualaxis(
  df_annual,
  source_left  = "T4M",
  source_right = "ECHAM6 t2m",
  colors       = colors,
  ylab_left    = ylab_left,
  ylab_right   = "based on ECHAM6 t2m (°C)",
  STARTYEAR    = 1998
)

g.echamd18O.annual <- make_plot_annual_dualaxis(
  df_annual,
  source_left  = "T4M",
  source_right = "ECHAM6 d18O",
  colors       = colors,
  ylab_left    = ylab_left,
  ylab_right   = expression("based on ECHAM6 " * delta^{18} * O ~ "\u2030"),
  STARTYEAR    = 1998
)

ga <- g.aws.annual + theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  plot.margin  = margin(5.5, 12, 0, 5.5)
)
gt <- g.echamT.annual + theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  plot.margin  = margin(0, 12, 0, 5.5)
)
go <- g.echamd18O.annual + theme(
  plot.margin = margin(0, 12, 5.5, 5.5)
)

p_annual <- (ga / gt / go) +
  plot_layout(ncol = 1, axes = "collect", guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  here::here("output", "figures", "Figure3_annual.pdf"),
  p_annual, width = 7.2, height = 9.0, units = "in"
)

# ============================================================
# Figure 3B/C: Scatter (annual) with Deming regression
# Only keep:
#   - T4M vs AWS (annual)
#   - T4M vs ECHAM6 d18O (annual)
# ============================================================

# statt "T4M δ18O (‰)"
ylab_t4m <- expression(T4M~delta^{18}*O~"\u2030")

# statt "ECHAM6 δ18O (‰)"
xlab_echam_d18O <- expression(ECHAM6~delta^{18}*O~"\u2030")

# statt "simulated from AWS t2m (‰)" (hier wolltest du eigentlich auch ‰)
xlab_aws <- expression("simulated from AWS t2m ("*"\u2030"*")")


# Deming scatter expects a "depth" column -> reuse year as depth
df_annual2 <- df_annual %>% rename(depth = year)

make_scatter_data <- function(df, x_name, y_name,
                              x_var = "d18O", y_var = "proxy",
                              bin_size = 1) {
  
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
                                conf_level = 0.90, n_boot = 1000, col = "red") {
  
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
  
  pred_y <- sapply(seq_len(nrow(boots)), function(i) boots$Intercept[i] + boots$Slope[i] * pred_x)
  
  alpha_half <- (1 - conf_level) / 2
  pred_df <- data.frame(
    x = pred_x,
    y = intercept + slope * pred_x,
    ymin = apply(pred_y, 1, quantile, probs = alpha_half, na.rm = TRUE),
    ymax = apply(pred_y, 1, quantile, probs = 1 - alpha_half, na.rm = TRUE)
  )
  
  ggplot() +
    geom_point(data = sc$points, aes(x = x, y = y), alpha = 1, size = 2) +
    geom_ribbon(data = pred_df, aes(x = x, ymin = ymin, ymax = ymax),
                fill = col, alpha = 0.15) +
    geom_line(data = pred_df, aes(x = x, y = y),
              color = col, linewidth = 1) +
    labs(x = xlab, y = ylab) +
    theme_minimal(base_size = 12)
}

# --- T4M vs AWS (annual) ---
sc_aws.ann <- make_scatter_data(
  df_annual2,
  y_name = "T4M",
  x_name = "AWS9 t2m precip ERA",
  y_var  = "d18O",
  x_var  = "proxy"
)

p_sc_aws <- plot_scatter_deming(
  sc_aws.ann,
  ylab = ylab_t4m,
  xlab = xlab_aws,
  vr   = 1,
  col  = "red"
)

ggsave(
  here::here("output", "figures", "Figure3_scatter_T4M_vs_AWS_annual.pdf"),
  p_sc_aws, width = 6.5, height = 5.0, units = "in"
)

# --- T4M vs ECHAM6 d18O (annual) ---
sc_echam.ann <- make_scatter_data(
  df_annual2,
  y_name = "T4M",
  x_name = "ECHAM6 d18O",
  y_var  = "d18O",
  x_var  = "proxy"
)

p_sc_echam <- plot_scatter_deming(
  sc_echam.ann,
  ylab = ylab_t4m,
  xlab = xlab_echam_d18O,
  vr   = 1,
  col  = "steelblue"
) + geom_abline(slope = 1, intercept = -3.4, color = "blue", linetype = "dashed")

ggsave(
  here::here("output", "figures", "Figure3_scatter_T4M_vs_ECHAM_d18O_annual.pdf"),
  p_sc_echam, width = 6.5, height = 5.0, units = "in"
)






##
# ---- annual correlations with p-values ----

corr_test <- function(df_wide, x_col, y_col) {
  dat <- df_wide %>% select({{ x_col }}, {{ y_col }}) %>% drop_na()
  test <- cor.test(dat[[1]], dat[[2]], method = "pearson")
  list(
    r = unname(test$estimate),
    p = test$p.value,
    n = nrow(dat)
  )
}

df_annual_wide <- df_annual %>%
  select(source, year, d18O, proxy) %>%
  pivot_wider(names_from = source, values_from = c(d18O, proxy))

# T4M vs AWS (proxy from AWS t2m + reanalysis precip)
res_aws <- corr_test(
  df_annual_wide,
  d18O_T4M,
  `proxy_AWS9 t2m precip ERA`
)

# T4M vs ECHAM6 t2m (proxy from ECHAM6 t2m + ECHAM6 accum)
res_ech_t <- corr_test(
  df_annual_wide,
  d18O_T4M,
  `proxy_ECHAM6 t2m`
)

# T4M vs ECHAM6 d18O (proxy from ECHAM6 δ18O + ECHAM6 accum)
res_ech_o <- corr_test(
  df_annual_wide,
  d18O_T4M,
  `proxy_ECHAM6 d18O`
)

cat(sprintf("T4M vs AWS:            r = %.3f, p = %.4g, n = %d\n",
            res_aws$r, res_aws$p, res_aws$n))

cat(sprintf("T4M vs ECHAM6 t2m:      r = %.3f, p = %.4g, n = %d\n",
            res_ech_t$r, res_ech_t$p, res_ech_t$n))

cat(sprintf("T4M vs ECHAM6 d18O:     r = %.3f, p = %.4g, n = %d\n",
            res_ech_o$r, res_ech_o$p, res_ech_o$n))