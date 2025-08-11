# ---- load packages ----

library(tidyverse)
library(patchwork)
library(grid)



# ========= recreating values from Lehle et al., 2009 =========

# ---- input data ----

# -- data as read off of Lehle et al., 2009
# confidence intervall serial congruent: RT1 730-705 = 25; ER1 2.3-1.8 = 0.5; RT2 1100 - 1065 = 35; ER2 4.1 - 3.65 = 0.45
# confidence intervall serial incongruent: RT1 840-795 = 45; ER1 3.7 - 3.2 = 0.5 ; RT2 1200 - 1150 = 50; ER2 3 - 2.5 = 0.5
# confidence intervall parallel congruent: RT1 810-785 = 25; ER1 1.3 - 1.2 = 0.1; RT2 1060 - 1025 = 35; ER2 3.5 - 3.1 = 0.4
# confidence intervall parallel incongruent: RT1 965-910 = 55; ER1 6.8 - 5.8 = 1.0; RT2 1260 - 1195 = 65; ER2 7.3 - 6.3 = 1.0

original_data <- tribble(
  ~strategy, ~congruency,   ~RT_1_mean, ~ER_1_mean, ~RT_2_mean, ~ER_2_mean, ~RT_1_hw, ~ER_1_hw, ~RT_2_hw, ~ER_2_hw,
  "seriell",     "congruent",   730, 1.8, 1100, 3.65, 25, 0.5, 35, 0.45,
  "seriell",     "incongruent",   795, 3.2, 1150, 2.5, 45, 0.5, 50, 0.5,
  "parallel",     "congruent",   785, 1.2, 1025, 3.1, 25, 0.1, 50, 0.4,
  "parallel",     "incongruent",   965, 5.8, 1195, 6.3, 55, 1.0, 65, 1.0

) 

# calculating sd (standard deviation) from half-width (hw) of the confidence intervall
# derived from formula of confidence intervall
# x ± t_{1-\alpha / 2} * sd / sqrt(n) -> hw = t_{1-\alpha / 2} * sd / sqrt(n) -> sd = hw * sqrt(n) / t_{1-\alpha / 2}
n_participants <- 28
t_value_95 <- 1.96

original_data <- original_data |>
  mutate(
    RT_1_sd = RT_1_hw * sqrt(n_participants) / t_value_95,
    ER_1_sd = ER_1_hw * sqrt(n_participants) / t_value_95,
    RT_2_sd = RT_2_hw * sqrt(n_participants) / t_value_95,
    ER_2_sd = ER_2_hw * sqrt(n_participants) / t_value_95,
    .keep = "unused"
  )
rm(t_value_95)



# ========= simulating data set =========

# initializing simulated data frame
sim_data <- original_data |>
  slice(rep(row_number(), times = n_participants)) |>
  mutate(
    ID = rep(1:n_participants, each = 4)
  ) |>
  relocate(ID)
rm(n_participants)

# introduce noice from standard deviation, while keeping mean and standard deviation as intended
set.seed(123)

sim_data <- sim_data |>
  group_by(strategy, congruency) |>
  mutate(
    # RT_1
    RT_1_err = rnorm(n(), 0, RT_1_sd),
    RT_1_err = RT_1_err - mean(RT_1_err),                        # centering error vector to 0
    RT_1_err = RT_1_err / sd(RT_1_err) * RT_1_sd,                 # scale error sd to inteded sd
    RT_1     = RT_1_mean + RT_1_err,                              # add error vector to inteded mean

    # RT_2
    RT_2_err = rnorm(n(), 0, RT_2_sd),
    RT_2_err = RT_2_err - mean(RT_2_err),
    RT_2_err = RT_2_err / sd(RT_2_err) * RT_2_sd,
    RT_2     = RT_2_mean + RT_2_err,

    # ER_1
    ER_1_err = rnorm(n(), 0, ER_1_sd),
    ER_1_err = ER_1_err - mean(ER_1_err),
    ER_1_err = ER_1_err / sd(ER_1_err) * ER_1_sd,
    ER_1     = ER_1_mean + ER_1_err,

    # ER_2
    ER_2_err = rnorm(n(), 0, ER_2_sd),
    ER_2_err = ER_2_err - mean(ER_2_err),
    ER_2_err = ER_2_err / sd(ER_2_err) * ER_2_sd,
    ER_2     = ER_2_mean + ER_2_err,

    .keep = "unused",
  ) |>
  ungroup() |>
  select(-contains("err"))

# checking values
sim_data |>
  group_by(strategy, congruency) |>
  summarise(
    across(
      c(RT_1:ER_2),
      mean,                
      .names = "{.col}_mean"
    ),
    across(
      c(RT_1:ER_2),
      sd,                
      .names = "{.col}_sd"
    ),
    .groups = "drop" 
  )




# ========= plot: visualizing scores (BIS & SAT) on task 1 =========

score_plot <- ggplot(data = sim_data) + 
  # scatter plot of data
  geom_point(
    aes(x = scale(RT_1), y = scale(ER_1), shape = strategy),
    color = "black",
    size = 3.5,
    alpha = 0.5
  ) +
  
  # vectors and their annotation
  geom_segment(
    aes(x = 2.8, y = 2.8, xend = -2.8, yend = -2.8),
    arrow = arrow(length = unit(1, "cm")),
    size = 1.3,
    color = "black",
    linetype = "dashed"
  ) +
  geom_segment(
    aes(x = - 2.8, y = 2.8, xend = 2.8, yend =  -2.8),
    arrow = arrow(length = unit(1, "cm")),
    size = 1.3,
    color = "black",
    linetype = "dashed"
  ) +
  annotate("text", x = -1.7, y = -1.1, label = "Leistung (BIS)",  hjust = 1.1, vjust = 1.1, size = 9, angle = 45) +
  annotate("text", x = 3, y =  -2.7, label = "Genauigkeitstendenz (rSATS)",   hjust = 1.1, vjust = -0.1, size = 9, angle = -45) +

  # Labeling of axis and legend
  labs(
    x = "Reaktionszeit (z-standardisiert)", 
    y = "Fehlerrate (z-standardisiert)", 
    shape = "Strategie"
  ) +
  
  # scaling axis to be the same and setting ticks
  coord_fixed(ratio = 1) +
  scale_x_continuous(
  limits = c(-3, 3),
  breaks = seq(-3, 3, by = 1)
  ) +
  scale_y_continuous(
    limits = c(-3, 3),
    breaks = seq(-3, 3, by = 1)
  ) +
  
  # setting theme
  theme_minimal() +
  theme(
    # background
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.grid        = element_blank(),
    
    # keep standard axis
    axis.line         = element_line(color = "black", size = 1),
    
    # sizing of elements
    plot.title = element_text(size = 30, face = "bold", color = "black", hjust = 0.5),
    axis.title.x      = element_text(size = 26, face = "bold", color = "black"),
    axis.title.y      = element_text(size = 26, face = "bold", color = "black"),
    axis.text         = element_text(size = 26, color = "black"),
    legend.title      = element_text(size = 26, face = "bold", color = "black"),
    legend.text       = element_text(size = 26, color = "black"),
    axis.ticks        = element_line(size = 1, color = "black"),

    # add arrow heads to the axis lines
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

# saving plot
plot_path <- paste0("01_Lehle_et_al_2009/plots/01_score_visualization.jpg")
ggsave(
  filename = plot_path,
  plot = score_plot,
  width = 13,    # in inches 
  height = 10,    
  dpi = 300      # resolution
)

# Open the plot (macOS command)
system2("open", args = shQuote(plot_path))



# ========= plot: visualizing changes in accuracy  =========

accuracy_change_plot <- ggplot(data = sim_data) + 
  # scatter plot of data
  geom_point(
    aes(x = scale(RT_1), y = scale(ER_1), shape = strategy),
    color = "black",
    size = 3.5,
    alpha = 0.5
  ) +
  
  # vectors and their annotation
  geom_segment( # BIS
    aes(x = 2.8, y = 2.8, xend = -2.8, yend = -2.8),
    arrow = arrow(length = unit(1, "cm")),
    size = 1.3,
    color = "black",
    linetype = "dashed"
  ) +
  geom_segment( # SAT
    aes(x = - 2.8, y = 2.8, xend = 2.8, yend =  -2.8),
    arrow = arrow(length = unit(1, "cm")),
    size = 1.3,
    color = "black",
    linetype = "dashed"
  ) +
  geom_segment( # left arrow
    aes(x = 0, y = -1, xend = -1, yend = -2),
    arrow = arrow(length = unit(0.5, "cm")),
    size = 1.3,
    color = "black",
    linetype = "twodash"
  ) +
  geom_segment( # middle arrow
    aes(x = 0, y = -1, xend = 0, yend = -2),
    arrow = arrow(length = unit(0.5, "cm")),
    size = 1.3,
    color = "black",
    linetype = "twodash"
  ) +
  geom_segment( # right arrow
    aes(x = 0, y = -1, xend = 1, yend = - 2),
    arrow = arrow(length = unit(0.5, "cm")),
    size = 1.3,
    color = "black",
    linetype = "twodash"
  ) +
  
  annotate( # BIS
    "text", 
    x = -1.7, 
    y = -1.1, 
    label = "Leistung (BIS)",  
    hjust = 1.1, 
    vjust = 1.1, 
    size = 9, 
    angle = 45
  ) +
  annotate("text", x = 3, y =  -2.7, label = "Genauigkeitstendenz (rSATS)",   hjust = 1.1, vjust = -0.1, size = 9, angle = -45) +
  annotate( # C
    "text", 
    x = -0.8, 
    y = -2.1, 
    label = "(C)",  
    hjust = 1.1, 
    vjust = 1.1, 
    size = 9
  ) +
  annotate( # B
    "text", 
    x = 0.2, 
    y = - 2.1, 
    label = "(B)",  
    hjust = 1.1, 
    vjust = 1.1, 
    size = 9
  ) +
  annotate( # A
    "text", 
    x = 1.2, 
    y = -2.1, 
    label = "(A)",  
    hjust = 1.1, 
    vjust = 1.1, 
    size = 9
  ) +
  # Labeling of axis and legend
  labs(
    x = "Reaktionszeit (z-standardisiert)", 
    y = "Fehlerrate (z-standardisiert)", 
    shape = "Strategie"
  ) +
  
  # scaling axis to be the same and setting ticks
  coord_fixed(ratio = 1) +
  scale_x_continuous(
  limits = c(-3, 3),
  breaks = seq(-3, 3, by = 1)
  ) +
  scale_y_continuous(
    limits = c(-3, 3),
    breaks = seq(-3, 3, by = 1)
  ) +
  
  # setting theme
  theme_minimal() +
  theme(
    # background
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.grid        = element_blank(),
    
    # keep standard axis
    axis.line         = element_line(color = "black", size = 1),
    
    # sizing of elements
    plot.title = element_text(size = 30, face = "bold", color = "black", hjust = 0.5),
    axis.title.x      = element_text(size = 26, face = "bold", color = "black"),
    axis.title.y      = element_text(size = 26, face = "bold", color = "black"),
    axis.text         = element_text(size = 26, color = "black"),
    legend.title      = element_text(size = 26, face = "bold", color = "black"),
    legend.text       = element_text(size = 26, color = "black"),
    axis.ticks        = element_line(size = 1, color = "black"),

    # add arrow heads to the axis lines
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

# saving plot
plot_path <- paste0("01_Lehle_et_al_2009/plots/02_accuracy_change_visualization.jpg")
ggsave(
  filename = plot_path,
  plot = accuracy_change_plot,
  width = 13,    # in inches 
  height = 10,    
  dpi = 300      # resolution
)

# Open the plot (macOS command)
system2("open", args = shQuote(plot_path))



# ========== calculating scores (BIS and accuracy bias) ==========

sim_data <- sim_data |>
  mutate(
    z_RT_1 = scale(RT_1),
    z_ER_1 = scale(ER_1),
    z_RT_2 = scale(RT_2),
    z_ER_2 = scale(ER_2),
    BIS_1 = - z_RT_1 - z_ER_1,
    BIS_2 = - z_RT_2 - z_ER_2,
    accuracy_bias_1 = z_RT_1 - z_ER_1,
    accuracy_bias_2 = z_RT_2 - z_ER_2,
  ) |>
  select(-contains("z_"))



# ========== collapsing data ==========

# collapsing data and taking middle of congruency
sim_data_summary <- sim_data |>
  group_by(strategy) |>
  summarise(
    across(
      c(RT_1:accuracy_bias_2),
      mean,                
      .names = "{.col}"
    ),
    .groups = "drop" 
  )



# ========== plots: visualizing different groups ==========

# factoring strategy and instruction to order their levels for plots later
#sim_data_summary <- sim_data_summary |>
#  mutate(
#    strategy = factor(strategy, levels = c("seriell", "parallel"))
#  )


# function for creating plots
make_metric_plot <- function(data, var, title, ylab, ylim_vec) {
  # Basis-Plot
  p <- ggplot(data, aes_string(x = "strategy", y = var, group = 1)) +
    geom_point(position = position_dodge(width = 0.1), size = 3) +
    geom_line(position = position_dodge(width = 0.1), size = 0.8)
  
  # Bedingte y-Skala
  if (ylim_vec[1] == 0) {
    p <- p + scale_y_continuous(
      limits = ylim_vec,
      expand = expansion(mult = c(0, 0), add = c(0, 1))
    )
  } else {
    p <- p + scale_y_continuous(
      limits = ylim_vec
    )
  }
  
  # rest of plot
  p +
    labs(
      title = title,
      x     = "Strategie",
      y     = ylab
    ) +
    theme_minimal() +
    theme(
      # background
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.grid        = element_blank(),
      
      # keep standard axis
      axis.line         = element_line(color = "black", size = 1),
      
      # sizing of elements
      plot.title        = element_text(size = 17, face = "bold", color = "black", hjust = 0.5),
      axis.title.x      = element_text(size = 17, face = "bold", color = "black"),
      axis.title.y      = element_text(size = 17, face = "bold", color = "black"),
      axis.text         = element_text(size = 17, color = "black"),
      axis.ticks        = element_line(size = 1, color = "black"),
      axis.ticks.length = unit(0.2, "cm"),

      # add arrow heads to the axis lines
      axis.line.y = element_line(
        color = "black",
        arrow = arrow(length = unit(0.7, "cm"))
      ),
      panel.border = element_blank()
    )
}


# specifying the Plots
plot_specs <- tribble(
  ~var, ~title, ~ylab, ~ylim_vec,
  "RT_1", "A) Reaktionszeit (Auf. 1)", "RZ (ms)", c(500, 1300),
  "RT_2", "B) Reaktionszeit (Auf. 2)", "RZ (ms)", c(500, 1300),
  "ER_1", "C) Fehlerrate (Auf. 1)", "FR (%)", c(0, 8),
  "ER_2", "D) Fehlerrate (Auf. 2)", "FR (%)", c(0, 8),
  "BIS_1", "E) Balanced Integration Score (Auf. 1)", "BIS", c(-1.2, 1.2),
  "BIS_2", "F) Balanced Integration Score (Auf. 2)", "BIS", c(-1.2,  1.2),
  "accuracy_bias_1", "G) Genauigkeitstendenz (Auf. 1)", "rSATS", c(-1.2, 1.2),
  "accuracy_bias_2", "H) Genauigkeitstendenz (Auf. 2)", "rSATS", c(-1.2, 1.2)
)

# creating all the plots
all_plots <- purrr::pmap(
  plot_specs,
  function(var, title, ylab, ylim_vec) {
    make_metric_plot(sim_data_summary, var, title, ylab, ylim_vec)
  }
)

final_dashboard <- 
  (all_plots[[1]] | all_plots[[2]]) /
  (all_plots[[3]] | all_plots[[4]]) /
  (all_plots[[5]] | all_plots[[6]]) /
  (all_plots[[7]] | all_plots[[8]])

# saving plot
plot_path <- "01_Lehle_et_al_2009/plots/03_lehle_visualization.jpg"
ggsave(
  filename = plot_path,
  plot = final_dashboard,
  width = 14,    # in inches 
  height = 20,    
  dpi = 300      # resolution
)

# Open the plot (macOS command)
system2("open", args = shQuote(plot_path))
