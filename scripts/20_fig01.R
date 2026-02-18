# scripts/20_fig01.R
source(here::here("scripts", "01_load_inputs.R"))

library(TrenchR)
library(T4Mdata)

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

sim1 <- sims$sim1
sim2 <- sims$sim2
sim3 <- sims$sim3
sim4 <- sims$sim4

df_all <- bind_rows(
  sim1 %>% transmute(depth_cm = depth * 100, value = signal, series = "constant accumulation"),
  sim2 %>% transmute(depth_cm = depth * 100, value = signal, series = "variable accumulation"),
  sim3 %>% transmute(depth_cm = depth * 100, value = signal, series = "+ diffusion"),
  sim4 %>% transmute(depth_cm = depth * 100, value = proxy,  series = "+ dating + binning")
) %>%
  mutate(series = factor(series, levels = c(
    "constant accumulation", "variable accumulation", "+ diffusion", "+ dating + binning"
  )))

p <- ggplot(df_all, aes(x = depth_cm, y = value)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~series, ncol = 1, scales = "free_y") +
  labs(x = "Depth (cm)", y = "Temperature (K)") +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(here("output", "figures", "Figure1_right.pdf"),
       p, width = 6.5, height = 8, units = "in")
