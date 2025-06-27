# ----- load packages -----

library(tidyverse)
library(psych)
library(afex)
library(patchwork)


# other data sets
data_set_all <- read_csv("own_data_analysis/data_sets/all_raw_data.csv")
# data_set_rating <- read_csv("own_data_analysis/data_sets/dataRating_wide.csv")

# reaction times & error rates
data_set_rt_er <- read_csv("own_data_analysis/data_sets/dataRTFehler_wide.csv")

# own reaction times & error rates
data_set_rt_er <- read_csv("data_RT_ER_all_wide.csv")






# ========== processing data set ==========

# creating proper wide format
data_set_rt_er <- data_set_rt_er |>
  select(-matches("FehlerR1|FehlerR2")) |>
  select(-any_of(c("...1", "UV.Instruk.y", "Reihenfolge.y"))) |>
  rename(
    Instruktion = any_of(c("UV.Instruk.x", "UV.Instruk")),
    Reihenfolge = any_of(c("Reihenfolge.x", "Reihenfolge")),
    ID          = any_of(c("participantID"))
  ) |>
  mutate(Instruktion = case_when(
    Instruktion == "ACT" ~ "aktivierend",
    Instruktion == "INH" ~ "inhibierend",
    TRUE ~ Instruktion
  )) |>
  mutate(Reihenfolge = case_when(
    Reihenfolge == "SerPar" ~ "seriell>parallel",
    Reihenfolge == "parSer" ~ "parallel>seriell",
    TRUE ~ Reihenfolge
  )) |>
  mutate(ID = str_sub(ID, 2)) |>
  arrange(ID) |>
  mutate(
    across(contains("RT"), ~ . * 1000),
    across(contains("Fehler"), ~ . * 100)
  )

# convert to long format
data_set_rt_er_long <- data_set_rt_er %>%
  pivot_longer(
    cols = 4:ncol(.),
    names_to = "Variable",
    values_to = "Wert"
  ) %>%
  separate(
    Variable,
    into = c("Typ", "Strategie", "Kongruenz"),
    sep = "\\.",
    fill = "right"
  ) %>%
  mutate(Typ = case_when(
    Typ == "FehlerQuote1" ~ "ER1",
    Typ == "FehlerQuote2" ~ "ER2",
    TRUE ~ Typ
  )) |>
  pivot_wider(
    names_from = Typ,
    values_from = Wert
  ) |>
  mutate(Kongruenz = case_when(
    Kongruenz == "komp" ~ "kongruent",
    Kongruenz == "inkomp" ~ "inkongruent",
    TRUE ~ Kongruenz
  )) |>
  mutate(Instru_Strat = paste(Instruktion, Strategie, sep = "-"))


# ---- collapsing congruency -----

data_set_rt_er_long <- data_set_rt_er_long |>
  group_by(ID, Instruktion, Reihenfolge, Strategie, Instru_Strat) |>
  summarise(
    RT1 = mean(RT1, na.rm = TRUE),
    RT2 = mean(RT2, na.rm = TRUE),
    ER1 = mean(ER1, na.rm = TRUE),
    ER2 = mean(ER2, na.rm = TRUE),
    .groups = "drop"
  )





# ========== checking and removing outliers ==========

# removing 09 from the start (optional)
data_set_rt_er_long <- data_set_rt_er_long |>
 filter(!ID %in% c("09"))

# ---- checking outliers (inside each group): 2 SD for each measure ----

# Check for any value > 2 SD
outliers_table_sd <- data_set_rt_er_long |>
  pivot_longer(cols = c(RT1, RT2, ER1, ER2),
               names_to = "Variable",
               values_to = "Original_Wert") |>
  group_by(Instru_Strat, Variable) |>
  mutate(z_score = as.numeric(scale(Original_Wert))) |>
  ungroup() |>
  filter(abs(z_score) > 2)

# output
print(outliers_table_sd, n = Inf, width = Inf)
# new: 9, 12, 13 // 9, 12, 14
# mean of congruency: 12 (4/8)
# differed by congruency: 12 (7/16), 13 (2/16), 14 (1/16), 15 (2/16)


# ---- checking outliers (over all groups): 2 SD for each measure ----

# Check for any value > 2 SD
outliers_table_sd <- data_set_rt_er_long |>
  pivot_longer(cols = c(RT1, RT2, ER1, ER2),
               names_to = "Variable",
               values_to = "Original_Wert") |>
  group_by(Variable) |>
  mutate(z_score = as.numeric(scale(Original_Wert))) |>
  ungroup() |>
  filter(abs(z_score) > 2)

# output
print(outliers_table_sd, n = Inf, width = Inf)
# new: 9, 12, 14 // 9, 12, 14
# mean of congruency: 12 (6/8)
# differed by congruency: 12 (12/16), 14 (4/16), 16 (1/16)


# ---- checking outliers (inside groups): 1,5 interquartile range from upper and lower quartile for each measure ----

# Check for outliers based on IQR
outliers_table_iqr <- data_set_rt_er_long |>
  pivot_longer(cols = c(RT1, RT2, ER1, ER2),
               names_to = "Variable",
               values_to = "Original_Wert") |>
  group_by(Instru_Strat, Variable) |>
  mutate(
    Q1 = quantile(Original_Wert, 0.25, na.rm = TRUE),
    Q3 = quantile(Original_Wert, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    outlier_flag = case_when(
      Original_Wert < Lower_Bound ~ "Lower",
      Original_Wert > Upper_Bound ~ "Upper",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(outlier_flag)) %>%
  select(ID, Variable, Original_Wert, outlier_flag)

# output
print(outliers_table_iqr, n = Inf, width = Inf)
# new: 9, 12, 13 // 9, 12, 16 (congruency differed)
# mean of congruency: 12 (4/8)
# differed by congruency: 12 (8/16), 13 (1/16), 15 (1/16), 16 (1/16)


# ---- checking outliers (over all groups): 1,5 interquartile range from upper and lower quartile for each measure ----

# Check for outliers based on IQR
outliers_table_iqr <- data_set_rt_er_long |>
  pivot_longer(cols = c(RT1, RT2, ER1, ER2),
               names_to = "Variable",
               values_to = "Original_Wert") |>
  group_by(Variable) |>
  mutate(
    Q1 = quantile(Original_Wert, 0.25, na.rm = TRUE),
    Q3 = quantile(Original_Wert, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    outlier_flag = case_when(
      Original_Wert < Lower_Bound ~ "Lower",
      Original_Wert > Upper_Bound ~ "Upper",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(outlier_flag)) %>%
  select(ID, Variable, Original_Wert, outlier_flag)

# output
print(outliers_table_iqr, n = Inf, width = Inf)

ggplot(data_set_rt_er_long |> 
           pivot_longer(cols = c(RT1, RT2, ER1, ER2),
                        names_to = "Variable",
                        values_to = "Original_Wert"),
       aes(x = Variable, y = Original_Wert)) +
  geom_boxplot(outlier.colour = NA) +  # Keine Standard-Ausreißer
  geom_point(data = outliers_table_iqr,
             aes(x = Variable, y = Original_Wert, color = outlier_flag),
             position = position_jitter(width = 0.1),
             size = 2) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots mit markierten Ausreißern",
       x = "Variable",
       y = "Originaler Wert",
       color = "Ausreißer-Typ") +
  theme(axis.text.x = element_blank(),
        panel.grid = element_blank())

# new: 9,12, 14, 16 // 9, 12, 13, 16
# mean of congruency: 12 (6/8), 14 (2/8), 16 (2/8)
# differed by congruency: 12 (11/16), 13 (1/16), 16 (2/16)


# ---- summary of outlieres in 8 analysis ----

# 12: 8 times (58/96)
# 13: 3 times (4/96)
# 14: 3 times (7/96)
# 15: 2 times (3/96)
# 16: 4 times (6/96)


# ---- removing outlier ----
data_set_rt_er_long_copy <- data_set_rt_er_long
data_set_rt_er_long <- data_set_rt_er_long_copy

data_set_rt_er_long <- data_set_rt_er_long |>
  filter(!ID %in% c("09", "12", "16"))





# ========== calculating new measures ==========

data_set_rt_er_long <- data_set_rt_er_long |>
  mutate(
    z_RT_1 = as.numeric(scale(RT1)),
    z_RT_2 = as.numeric(scale(RT2)),
    z_ER_1 = as.numeric(scale(ER1)),
    z_ER_2 = as.numeric(scale(ER2))
  ) |>
  mutate(
    BIS_1 = - z_RT_1 - z_ER_1,
    BIS_2 = - z_RT_2 - z_ER_2,
    Genauigkeitstendenz_1 = + z_RT_1 - z_ER_1,
    Genauigkeitstendenz_2 = + z_RT_2 - z_ER_2
  )






# ========== creating plots ==========

# ---- visualizing measures ----

# BIS
ggplot(data_set_rt_er_long) +
  geom_point(aes(x = z_RT_1, y = z_ER_1, color = BIS_1, shape = Instru_Strat)) +
  labs(
    title = "BIS in Relation zu RT & ER",
    x = "Reaktionszeit (z-standardisiert)",
    y = "Fehlerrate (z-standardisiert)",
    shape = "Bedingung",
    color = "BIS"
  )

# Genauigkeitstendenz
ggplot(data_set_rt_er_long) +
  geom_point(aes(x = z_RT_1, y = z_ER_1, color = Genauigkeitstendenz_1, shape = Instru_Strat)) +
  labs(
    title = "Genauigkeitstendenz in Relation zu RT & ER",
    x = "Reaktionszeit (z-standardisiert)",
    y = "Fehlerrate (z-standardisiert)",
    shape = "Bedingung",
    color = "Genauigkeitstendenz"
  )



# ---- visualizing comparison of groups -----

# summarizing data
vars_to_summarize <- c(
  "RT1", "RT2", "ER1", "ER2", "BIS_1", "BIS_2", "Genauigkeitstendenz_1", "Genauigkeitstendenz_2"
)

summary_data <- data_set_rt_er_long |>
  select(Strategie, Instruktion, all_of(vars_to_summarize)) |>
  pivot_longer(cols = all_of(vars_to_summarize), 
               names_to = "Variable", 
               values_to = "Wert") |>
  group_by(Strategie, Instruktion, Variable) |>
  summarise(
    mean_wert = mean(Wert, na.rm = TRUE),
    sd_wert = sd(Wert, na.rm = TRUE),
    n = n(),
    se_wert = sd_wert / sqrt(n),
    ci_lower = mean_wert - qt(0.975, df = n-1) * se_wert,
    ci_upper = mean_wert + qt(0.975, df = n-1) * se_wert,
    .groups = "drop"
  )


# function for creating plots
make_metric_plot <- function(data, var, title, ylab, ylim_vec) {
  # Basis-Plot
  p <- data |> 
    filter(Variable == var) |>
    ggplot(aes(x = Strategie,
                         y = mean_wert,
                         shape = Instruktion,
                         group = Instruktion)) +
    geom_point(position = position_dodge(width = 0.1), size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.1),
                width = 0.1) +
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
      y     = ylab,
      shape = "Instruktion"
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
  "RT1", "Reaktionszeit (Auf. 1)", "RT (ms)", c(700, 1600),
  "RT2", "Reaktionszeit (Auf. 2)", "RT (ms)", c(700, 1600),
  "ER1", "Fehlerrate (Auf. 1)", "Error Rate (%)", c(0, 13),
  "ER2", "Fehlerrate (Auf. 2)", "Error Rate (%)", c(0, 13),
  "BIS_1", "Balanced Integration Score (Auf. 1)", "BIS", c(-3.2, 3.2),
  "BIS_2", "Balanced Integration Score (Auf. 2)", "BIS", c(-3.2,  3.2),
  "Genauigkeitstendenz_1", "Genauigkeitstendenz (Auf. 1)", "Genauigkeitstendenz", c(-2, 2),
  "Genauigkeitstendenz_2", "Genauigkeitstendenz (Auf. 2)", "Genauigkeitstendenz", c(-2, 2)
)

# creating all the plots
all_plots <- purrr::pmap(
  plot_specs,
  function(var, title, ylab, ylim_vec) {
    make_metric_plot(summary_data, var, title, ylab, ylim_vec)
  }
)

overview_plot <- 
  (all_plots[[1]] | all_plots[[2]]) /
  (all_plots[[3]] | all_plots[[4]]) /
  (all_plots[[5]] | all_plots[[6]]) /
  (all_plots[[7]] | all_plots[[8]])

# saving plot
plot_path <- "own_data_analysis/overview_plot.jpg"
ggsave(
  filename = plot_path,
  plot = overview_plot,
  width = 14,    # in inches 
  height = 20,    
  dpi = 300      # resolution
)

# Open the plot (macOS command)
system2("open", args = shQuote(plot_path))



# ---- inferenzstatistische Testung ----

# Genauigkeitstendenz Aufg. 1
aov_genauig_1 <- aov_ez(
  id = "ID",
  dv = "Genauigkeitstendenz_1",
  between = "Instruktion",
  within = "Strategie",
  data = data_set_rt_er_long
)


# Genauigkeitstendenz Aufg. 2
aov_genauig_2 <- aov_ez(
  id = "ID",
  dv = "Genauigkeitstendenz_2",
  between = "Instruktion",
  within = "Strategie",
  data = data_set_rt_er_long
)

# Ergebnisausgabe
aov_genauig_1
aov_genauig_2



# ========== a posteriori power analyse ==========

library(effectsize)
library(pwr)
library(knitr)

power_from_aov_ez <- function(aov_result, sig_level = 0.05) {
  # Werte aus ANOVA extrahieren
  aov_table <- as.data.frame(aov_result$anova_table)

  F_value    <- aov_table[, "F"]
  df_effect  <- aov_table[, "num Df"]
  df_error   <- aov_table[, "den Df"]

  eta2_p <- (F_value * df_effect) / (F_value * df_effect + df_error)
  f2     <- eta2_p / (1 - eta2_p)

  pwr_result <- pwr::pwr.f2.test(u = df_effect,
                                  v = df_error,
                                  f2 = f2,
                                  sig.level = sig_level)

  result_table <- data.frame(
    Effect     = rownames(aov_table),
    Power      = round(pwr_result$power * 100, 1),
    Beta       = round((1 - pwr_result$power) * 100, 1),
    Part_eta2  = round(eta2_p, 3)
  )
  
  # Ausgabe
  knitr::kable(result_table,
               col.names = c("Effekt", "Power (%)", "Beta (%)", "Part. eta²"),
               align = "lrrr",
               caption = paste0("Powerberechnung bei Alpha-Niveau ", sig_level * 100, " %"))
}


# Angenommen, dein AOV-Resultat heißt aov_genauig_1
power_from_aov_ez(aov_genauig_1, sig_level = 0.15)
power_from_aov_ez(aov_genauig_2, sig_level = 0.15)




