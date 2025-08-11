# ---- loading packages ----

library (tidyverse)
library(patchwork)



# ---- loading data sets ----

data_high <- read_csv("04_drift_diffusion_modelling/exports/data_A__DDM_mit_hohem_Rauschen.csv")
data_middle <- read_csv("04_drift_diffusion_modelling/exports/data_B__DDM_mit_mittlerem_Rauschen.csv")
data_low <- read_csv("04_drift_diffusion_modelling/exports/data_C__DDM_mit_geringem_Rauschen.csv")



# ---- plotting function ----

plot_ddm_trajectories <- function(data, title_text, a = 1.0) {
  
  # Compute stats
  stats <- data |>
    group_by(simulation) |>
    summarise(rt = first(rt), choice = first(choice)) |>
    summarise(
      mean_rt = mean(rt, na.rm = TRUE),
      error_rate = sum(choice == 0, na.rm = TRUE) / 10
    )
  
  # Plot
  p <- ggplot(data, aes(x = time, y = evidence, group = simulation)) +
    geom_line(alpha = 0.5, linewidth = 0.6) +
    geom_hline(yintercept = c(0, a), linetype = "dashed", linewidth = 1) +
    annotate("text", x = 1.56, y = a + 0.06, label = "a (richtige Antwort)", size = 8, hjust = 0) +
    annotate("text", x = 1.56, y = -0.06, label = "0 (falsche Antwort)", size = 8, hjust = 0) +
    annotate("text", x = 1.7, y = 0.52, label = paste0("bar(RZ) == ", sprintf("%.2f", stats$mean_rt), "*' s'"), size = 8, parse = TRUE, hjust = 0) + 
    annotate("text", x = 1.7, y = 0.42, label = paste0("FR == ", sprintf("%.1f", stats$error_rate * 100), "*'%'"), size = 8, parse = TRUE, hjust = 0) +
    labs(title = title_text,
         x = "Zeit (Sekunden)",
         y = "Angehäufte Evidenz") +
    scale_color_viridis_d(option = "viridis", guide = "none") +
    scale_x_continuous(limits = c(0, 2), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.1, a + 0.1), breaks = seq(0, a, by = 0.2)) +
    theme_minimal(base_size = 14) +
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
      panel.border = element_blank()
    )
  
  return(p)
}


# ---- creating and exporting plots ----

# create plots per condition
p1 <- plot_ddm_trajectories(data_high, "A) DDM mit hohem Rauschen")
p2 <- plot_ddm_trajectories(data_middle, "B) DDM mit mittlerem Rauschen")
p3 <- plot_ddm_trajectories(data_low, "C) DDM mit niedrigem Rauschen")

# stack plots vertically into one
combined_plot <- p1 / plot_spacer() / p2 / plot_spacer() / p3 + plot_layout(heights = c(1, 0.075, 1, 0.075, 1))


# export plot
ggsave(
  filename = "04_drift_diffusion_modelling/r_ddm_noise_models.jpg",
  plot = combined_plot,
  width = 14,     # adjust width and height as needed
  height = 20,
  dpi = 300       # high resolution
)

system("open 04_drift_diffusion_modelling/r_ddm_noise_models.jpg")
