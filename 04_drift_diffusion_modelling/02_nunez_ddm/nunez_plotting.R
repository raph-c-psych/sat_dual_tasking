# ---- loading packages ----

library (tidyverse)
library(patchwork)



# ---- loading data sets ----

# loading the plotting tim
plottime <- read_csv("04_drift_diffusion_modelling/02_nunez_ddm/exports/plottime.csv")

# loading the random walk data
data_high <- read_csv("04_drift_diffusion_modelling/02_nunez_ddm/exports/data_high.csv") %>%
  mutate(across(everything(), ~ {
    idx <- which(.x >= 1 | .x <= 0)
    if (length(idx) == 0) return(.x) 
    cutoff <- min(idx) 
    replace(.x, (cutoff + 1):length(.x), NA)
  })) %>%
  mutate(across(everything(), ~ replace(.x, row_number() > 170, NA))) %>%
  # Step 2: Add time column
  mutate(time = plottime$plottime) %>%
  # Step 3: Move time to the first column
  relocate(time) %>%
  # Step 4: Pivot to long format
  pivot_longer(-time, names_to = "simulation", values_to = "evidence")
data_middle <- read_csv("04_drift_diffusion_modelling/02_nunez_ddm/exports/data_middle.csv") %>%
  mutate(across(everything(), ~ {
    idx <- which(.x >= 1 | .x <= 0)
    if (length(idx) == 0) return(.x) 
    cutoff <- min(idx) 
    replace(.x, (cutoff + 1):length(.x), NA)
  })) %>%
  mutate(across(everything(), ~ replace(.x, row_number() > 170, NA))) %>%
  # Step 2: Add time column
  mutate(time = plottime$plottime) %>%
  # Step 3: Move time to the first column
  relocate(time) %>%
  # Step 4: Pivot to long format
  pivot_longer(-time, names_to = "simulation", values_to = "evidence")
data_low <- read_csv("04_drift_diffusion_modelling/02_nunez_ddm/exports/data_low.csv") %>%
  mutate(across(everything(), ~ {
    idx <- which(.x >= 1 | .x <= 0)
    if (length(idx) == 0) return(.x) 
    cutoff <- min(idx) 
    replace(.x, (cutoff + 1):length(.x), NA)
  })) %>%
  mutate(across(everything(), ~ replace(.x, row_number() > 170, NA))) %>%
  # Step 2: Add time column
  mutate(time = plottime$plottime) %>%
  # Step 3: Move time to the first column
  relocate(time) %>%
  # Step 4: Pivot to long format
  pivot_longer(-time, names_to = "simulation", values_to = "evidence")

# loading the results
results_high <- read_csv("04_drift_diffusion_modelling/02_nunez_ddm/exports/results_high.csv")
results_middle <- read_csv("04_drift_diffusion_modelling/02_nunez_ddm/exports/results_middle.csv")
results_low <- read_csv("04_drift_diffusion_modelling/02_nunez_ddm/exports/results_low.csv")



# ---- plotting function ----

plot_ddm_trajectories <- function(data, results, title_text, noise_level, a = 1.0) {
  
  # Compute stats
  mean_er <- 1 - mean(results$correct, na.rm = TRUE)
  mean_rt <- mean(results$rt, na.rm = TRUE)
  
  # Plot "bold('Parameters')"
  p <- ggplot(data, aes(x = time, y = evidence, group = simulation)) +
    geom_line(alpha = 0.5, linewidth = 0.6) +
    geom_hline(yintercept = c(0, a), linetype = "dashed", linewidth = 1) +
    annotate("text", x = 1.56, y = a + 0.06, label = "a (richtige Antwort)", size = 8, hjust = 0) +
    annotate("text", x = 1.56, y = -0.06, label = "0 (falsche Antwort)", size = 8, hjust = 0) +
    annotate("text", x = 2.15, y = 0.95, label = "bold('Ergebnisse')", size = 8, parse = TRUE, hjust = 0) + 
    annotate("text", x = 2.15, y = 0.85, label = paste0("bar(RZ) == ", sprintf("%.2f", mean_rt), "*' s'"), size = 8, parse = TRUE, hjust = 0) + 
    annotate("text", x = 2.15, y = 0.75, label = paste0("FR == ", sprintf("%.1f", mean_er * 100), "*'%'"), size = 8, parse = TRUE, hjust = 0) +
    annotate("text", x = 2.15, y = 0.55, label = "bold('Parameter')", size = 8, parse = TRUE, hjust = 0) + 
    annotate("text", x = 2.15, y = 0.45, label = paste0("dc == bold('", sprintf("%.1f", noise_level), "')"), size = 8, parse = TRUE, hjust = 0) + 
    annotate("text", x = 2.15, y = 0.35, label = paste0("d == ", sprintf("%.1f", 0.5)), size = 8, parse = TRUE, hjust = 0) +
    annotate("text", x = 2.15, y = 0.25, label = paste0("Ter == ", sprintf("%.1f", 0.3)), size = 8, parse = TRUE, hjust = 0) +
    annotate("text", x = 2.15, y = 0.15, label = paste0("a == ", sprintf("%.1f", 1.0)), size = 8, parse = TRUE, hjust = 0) +
    annotate("text", x = 2.15, y = 0.05, label = paste0("z == ", sprintf("%.1f", 0.5)), size = 8, parse = TRUE, hjust = 0) +
    labs(title = title_text,
         x = "Zeit (Sekunden)",
         y = "Angehäufte Evidenz") +
    scale_color_viridis_d(option = "viridis", guide = "none") +
    # scale_x_continuous(limits = c(0, 2), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.1, a + 0.1), breaks = seq(0, a, by = 0.2)) +
    theme_minimal(base_size = 14) +
    coord_cartesian(xlim = c(0, 2), ylim = c(-0.1, a + 0.1), clip = "off") +
    theme(
      # background
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.grid        = element_blank(),

      # keep standard axis
      axis.line         = element_line(color = "black", size = 1),
      
      # sizing of elements
      plot.title        = element_text(size = 23, face = "bold", color = "black", hjust = 0.5),
      axis.title.x      = element_text(size = 20, face = "bold", color = "black"),
      axis.title.y      = element_text(size = 20, face = "bold", color = "black"),
      axis.text         = element_text(size = 20, color = "black"),
      
      axis.line.x = element_line(
        color = "black",
        arrow = arrow(length = unit(0.7, "cm"))
      ),
      axis.line.y = element_line(
        color = "black",
        arrow = arrow(length = unit(0.7, "cm"))
      ),
      panel.border = element_blank(),

      # margin
      plot.margin = margin(t = 10, r = 170, b = 10, l = 10)  # top, right, bottom, left
    )
  
  return(p)
}


# ---- creating and exporting plots ----

# create plots per condition
p1 <- plot_ddm_trajectories(data_high, results_high, noise_level = 0.6, "A) DDM mit hohem Rauschen")
p2 <- plot_ddm_trajectories(data_middle, results_middle, noise_level = 0.5, "B) DDM mit mittlerem Rauschen")
p3 <- plot_ddm_trajectories(data_low, results_low, noise_level = 0.4, "C) DDM mit niedrigem Rauschen")

# stack plots vertically into one
combined_plot <- p1 / plot_spacer() / p2 / plot_spacer() / p3 + plot_layout(heights = c(1, 0.075, 1, 0.075, 1))


# export plot
ggsave(
  filename = "04_drift_diffusion_modelling/02_nunez_ddm/nunez_ddm.jpg",
  plot = combined_plot,
  width = 16,     # adjust width and height as needed
  height = 20,
  dpi = 300       # high resolution
)

system("open 04_drift_diffusion_modelling/02_nunez_ddm/nunez_ddm.jpg")


