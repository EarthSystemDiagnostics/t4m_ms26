# scripts/20_fig01.R
source(here::here("scripts", "01_load_inputs.R"))
source(here::here("R", "plot_dualaxis_helpers.R"))

library(TrenchR)
library(T4Mdata)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)

dir.create(here::here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# ---- Figure 1 left ----
pdf(here("output", "figures", "Figure1_left.pdf"), width = 8, height = 6)
plot2D(
  T4M, .var = "d18O",
  xlim = c(0, 50), ylim = c(4.2, 0),
  filledContour = TRUE, fill = TRUE,
  rescale.v = 0.01, hadj = 0.02, line.v = 3,
  horizontal = FALSE
)
dev.off()

# ---- Figure 1 right ----
sims <- readRDS(here("output", "sims", "fig01_sims.rds"))

df_fig2 <- readRDS(here::here("output", "sims", "fig02_df_all.rds"))


sim1 <- sims$sim1
sim2 <- sims$sim2
sim3 <- sims$sim3
sim4 <- sims$sim4

df_all <- bind_rows(
  sim1 %>% transmute(depth, value = signal, series = "constant accumulation", lw = 0.3),
  sim2 %>% transmute(depth, value = signal, series = "variable accumulation",  lw = 0.3),
  sim3 %>% transmute(depth, value = signal, series = "+ diffusion",            lw = 0.9),
  sim4 %>% transmute(depth, value = proxy,  series = "+ dating + binning",     lw = 0.9)
) %>%
  mutate(series = factor(series, levels = c(
    "constant accumulation", "variable accumulation", "+ diffusion", "+ dating + binning"
  )))




# ---- inputs ----
# sims: list(sim1, sim2, sim3, sim4) must exist, e.g. loaded from your sim script / RDS
# Example if you saved them:
# sims_raw <- readRDS(here::here("output", "sims", "fig02_sims_raw.rds"))
# ...but your sim1..sim4 seem to come from a different object; adjust if needed.

df_fig2 <- readRDS(here::here("output", "sims", "fig02_df_all.rds"))

colors <- c(
  "T4M"                 = "black",
  "AWS9 t2m precip ERA" = "red",
  "ECHAM6 t2m"          = "firebrick",
  "ECHAM6 d18O"         = "steelblue"
)

# y label like in Fig2 (with a bit more space + (‰))
ylab_left <- expression("T4M" ~~ delta^{18}*O ~ "(" * "\u2030" * ")")

# ---- helper: simple single-series panel for sim1..sim3 ----
panel_sim <- function(sim, title, lw = 0.3, ylab = "Temperature (K)") {
  ggplot(sim, aes(x = depth, y = signal)) +
    geom_line(linewidth = lw,col=colors[2]) +
    labs(x = NULL, y = ylab, title = title) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# ---- top 3 panels (raw sim signal) ----
p1 <- panel_sim(sim1, "constant accumulation", lw = 0.3)
p2 <- panel_sim(sim2, "variable accumulation",  lw = 0.3)
p3 <- panel_sim(sim3, "+ diffusion",            lw = 0.9)

# ---- bottom panel: EXACTLY like Fig2 (top panel), but without year axis if you want ----
df_bottom <- df_fig2 %>% filter(source %in% c("T4M", "AWS9 t2m precip ERA"))




# --- Farben: alles rot (Simulation), T4M schwarz ---
cols_bottom <- c(
  "sim" = "red",
  "t4m" = "black"
)

# ---- bottom panel data ----
df_t4m <- df_fig2 %>% filter(source == "T4M") %>% select(depth, d18O)
df_sim <- df_fig2 %>% filter(source == "AWS9 t2m precip ERA") %>% select(depth, proxy)

# ---- bottom panel: left = simulation (°C), right = T4M δ18O (‰) ----
# linear map T4M onto simulation scale for plotting on left axis:
m_sim <- mean(df_sim$proxy, na.rm = TRUE); s_sim <- sd(df_sim$proxy, na.rm = TRUE)
m_t4m <- mean(df_t4m$d18O, na.rm = TRUE); s_t4m <- sd(df_t4m$d18O, na.rm = TRUE)

df_t4m_plot <- df_t4m %>%
  mutate(d18O_on_sim = (d18O - m_t4m) * (s_sim / s_t4m) + m_sim)

p4 <- ggplot() +
  geom_line(data = df_sim,
            aes(x = depth, y = proxy, colour = "sim"),
            linewidth = 0.9) +
  geom_line(data = df_t4m_plot,
            aes(x = depth, y = d18O_on_sim, colour = "t4m"),
            linewidth = 0.9) +
  scale_colour_manual(values = cols_bottom, breaks = c("sim","t4m"), labels = c("AWS sim", "T4M")) +
  scale_y_continuous(
    name = "based on AWS t2m (°C)",
    sec.axis = sec_axis(~ (. - m_sim) / (s_sim / s_t4m) + m_t4m,
                        name = expression("T4M" ~~ delta^{18}*O ~ "(" * "\u2030" * ")"))
  ) +
  labs(x = "Depth (cm)", colour = "", title = "+ dating + binning (AWS sim + T4M)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title.y      = element_text(color = cols_bottom[["sim"]]),
    axis.text.y       = element_text(color = cols_bottom[["sim"]]),
    axis.title.y.right= element_text(color = cols_bottom[["t4m"]]),
    axis.text.y.right = element_text(color = cols_bottom[["t4m"]])
  )


# ---- align x axis label only on bottom plot ----
p1 <- p1 + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
p2 <- p2 + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
p3 <- p3 + theme(axis.title.x = element_blank(), axis.text.x = element_blank())

# ---- assemble ----
p_fig1 <- (p1 / p2 / p3 / p4) +
  plot_layout(ncol = 1, heights = c(1, 1, 1, 1.35))

ggsave(here::here("output", "figures", "Figure1.pdf"),
       p_fig1, width = 6.5, height = 9.0, units = "in", device = cairo_pdf)

ggsave(here::here("output", "figures", "Figure1.svg"),
       p_fig1, width = 6.5, height = 9.0, units = "in")


