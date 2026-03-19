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
pdf(here("output", "figures", "Figure1_left.pdf"), width = 8/1.5, height = 6/1.5)
plot2D(
  T4M, .var = "d18O",
  xlim = c(0, 50), ylim = c(4.2, 0),
  filledContour = TRUE, fill = TRUE,
  rescale.v = 0.01, hadj = 0.02, line.v = 3,
  horizontal = FALSE
)
dev.off()


# ---- Figure 1 left ----
svglite::svglite(here("output", "figures", "Figure1_left.svg"),
                 width = 8/1.5, height = 6/1.5)
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

aws_ylim <- range(sim1$signal, na.rm = TRUE)
pad <- diff(aws_ylim) * 0.09
aws_ylim <- aws_ylim + c(-pad, pad)

sim1 <- sims$sim1
sim2 <- sims$sim2
sim3 <- sims$sim3


df_all <- bind_rows(
  sim1 %>% transmute(depth, value = signal, series = "constant accumulation", lw = 0.3),
  sim2 %>% transmute(depth, value = signal, series = "variable accumulation",  lw = 0.3),
  sim3 %>% transmute(depth, value = signal, series = "+ diffusion",            lw = 0.9)
) %>%
  mutate(series = factor(series, levels = c(
    "constant accumulation", "variable accumulation", "+ diffusion"
  )))




#For the last panel, we need T4M 
df_fig2 <- readRDS(here::here("output", "sims", "fig02_df_all.rds"))



colors <- c(
  "T4M"                 = "black",
  "AWS9 t2m precip ERA" = "steelblue",
  "ECHAM6 t2m"          = "red",
  "ECHAM6 d18O"         = "firebrick"
)


# y label like in Fig2 (with a bit more space + (‰))
ylab_left <- expression("T4M" ~~ delta^{18}*O ~ "(" * "\u2030" * ")")
panel_sim <- function(sim, title, lw = 0.3,
                      ylab = "based on AWS t2m (°C)",
                      y_limits = aws_ylim) {
  ggplot(sim, aes(x = depth, y = signal)) +
    geom_line(linewidth = lw, col = colors[["AWS9 t2m precip ERA"]]) +
    coord_cartesian(ylim = y_limits) +
    labs(x = NULL, y = ylab, title = title) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "plain",size=9),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(color = colors[["AWS9 t2m precip ERA"]]),
      axis.text.y  = element_text(color = colors[["AWS9 t2m precip ERA"]]),
      axis.ticks.y = element_line(color = colors[["AWS9 t2m precip ERA"]])
    )
}

# ---- top 3 panels (raw sim signal) ----
p1 <- panel_sim(sim1, "constant accumulation", lw = 0.9)
p2 <- panel_sim(sim2, "variable accumulation",  lw = 0.9)
p3 <- panel_sim(sim3, "+ diffusion",            lw = 0.9)

# ---- bottom panel: as Fig2 (top panel) ----
df_bottom <- df_fig2 %>% filter(source %in% c("T4M", "AWS9 t2m precip ERA"))


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
            aes(x = depth, y = proxy, colour = "AWS9 t2m precip ERA"),
            linewidth = 0.9) +
  geom_line(data = df_t4m_plot,
            aes(x = depth, y = d18O_on_sim, colour = "T4M"),
            linewidth = 0.9) +
  scale_colour_manual(values = colors, breaks = c("AWS9 t2m precip ERA","T4M"), labels = c("AWS sim", "T4M")) +
  scale_y_continuous(
    name = "based on AWS t2m (°C)",
    sec.axis = sec_axis(~ (. - m_sim) / (s_sim / s_t4m) + m_t4m,
                        name = expression("T4M" ~~ delta^{18}*O ~ "(" * "\u2030" * ")"))
  ) +
  coord_cartesian(ylim = aws_ylim) +  
  labs(x = "Snow depth (m)", colour = "", title = "+ dating") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "plain",size=9),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title.y      = element_text(color = colors[["AWS9 t2m precip ERA"]]),
    axis.text.y       = element_text(color = colors[["AWS9 t2m precip ERA"]]),
    axis.title.y.right= element_text(color = colors[["T4M"]]),
    axis.text.y.right = element_text(color = colors[["T4M"]])
  )

# ---- align x axis label only on bottom plot ----
p1 <- p1 + theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  plot.margin  = margin(1, 5.5, 0, 5.5)
)

p2 <- p2 + theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  plot.margin  = margin(0, 5.5, 0, 5.5)
)

p3 <- p3 + theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  plot.margin  = margin(0, 5.5, 0, 5.5)
)

p4 <- p4 + theme(
  plot.margin = margin(0, 5.5, 1, 5.5)
)
p1 <- p1 + labs(y = NULL)
p2 <- p2 + labs(y = "based on AWS t2m (°C)")
p3 <- p3 + labs(y = NULL)

p4 <- p4 +
  scale_y_continuous(
    name = NULL,
    sec.axis = sec_axis(
      ~ (. - m_sim) / (s_sim / s_t4m) + m_t4m,
      name = expression("T4M" ~~ delta^{18}*O ~ "(" * "\u2030" * ")")
    )
  )

# ---- assemble ----
p_fig1 <- (p1 / p2 / p3 / p4) +
  plot_layout(ncol = 1, heights = c(1, 1, 1, 1.35)) +
  plot_annotation(tag_levels = list(c("b", "c", "d", "e"))) &
  theme(
    plot.tag = element_text(size = 11),
    plot.tag.position = c(0.01, 0.98)
  )

ggsave(here::here("output", "figures", "Figure1.pdf"),
       p_fig1, width = 4.5, height = 7.0, units = "in", device = cairo_pdf)

ggsave(here::here("output", "figures", "Figure1.svg"),
       p_fig1, width = 4.5, height = 7.0, units = "in")


