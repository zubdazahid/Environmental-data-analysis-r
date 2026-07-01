############################################################
# Environmental Data Analysis in R
# Project: CO2 Emissions under Biochar Treatments - Day 1
# Author: Zubda Zahid
#
# Description:
# Reproducible workflow for analysing soil CO2 emission data.
# The script includes data cleaning, ANOVA, Tukey HSD,
# summary statistics, and publication-quality visualisation.
############################################################

# Load required packages
library(dplyr)
library(ggplot2)
library(ggpattern)
library(agricolae)

# 1. Import data
data <- read.csv("data/sample_data.csv")

# 2. Clean and prepare data
treatment_order <- c(
  "Control", "Control+F",
  "WBC", "WBC+F",
  "BBC", "BBC+F",
  "PHBC", "PHBC+F"
)

data$Treatments <- factor(data$Treatments, levels = treatment_order)
data$CO2 <- as.numeric(data$CO2)

# 3. One-way ANOVA
model <- aov(CO2 ~ Treatments, data = data)

# Check normality of residuals
shapiro_result <- shapiro.test(residuals(model))
print(shapiro_result)

# ANOVA summary
anova_result <- summary(model)
print(anova_result)

# 4. Tukey HSD post-hoc test
tukey <- HSD.test(model, "Treatments", group = TRUE)

tukey_letters <- tukey$groups
tukey_letters$Treatments <- rownames(tukey_letters)

# 5. Summary statistics
summary_stats <- data %>%
  group_by(Treatments) %>%
  summarise(
    Mean = mean(CO2, na.rm = TRUE),
    SD = sd(CO2, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Merge statistics with Tukey letters
final_table <- merge(summary_stats, tukey_letters, by = "Treatments")
final_table$Treatments <- factor(final_table$Treatments, levels = treatment_order)
final_table <- final_table[order(final_table$Treatments), ]

# Add pattern for fertilised treatments
final_table$Pattern <- ifelse(
  grepl("\\+F", as.character(final_table$Treatments)),
  "stripe",
  "none"
)

print(final_table)

# 7. Export results
dir.create("results", showWarnings = FALSE)
write.csv(final_table, "results/summary_statistics.csv", row.names = FALSE)

# 8. Create publication-quality figure
CO2_graph <- ggplot(
  final_table,
  aes(
    x = Treatments,
    y = Mean,
    fill = Treatments,
    pattern = Pattern
  )
) +
  geom_col_pattern(
    colour = "black",
    linewidth = 0.8,
    width = 0.75,
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.08,
    pattern_spacing = 0.035,
    pattern_angle = 45
  ) +
  geom_errorbar(
    aes(ymin = Mean, ymax = Mean + SD),
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_text(
    aes(
      y = Mean + SD + 0.10 * max(Mean, na.rm = TRUE),
      label = groups
    ),
    size = 6,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Control" = "#E8B3B3", "Control+F" = "#E8B3B3",
    "WBC" = "#9FC9E2", "WBC+F" = "#9FC9E2",
    "BBC" = "#E6DD8A", "BBC+F" = "#E6DD8A",
    "PHBC" = "#BDBDBD", "PHBC+F" = "#BDBDBD"
  )) +
  scale_pattern_manual(values = c(
    "none" = "none",
    "stripe" = "stripe"
  )) +
  labs(
    x = "Treatments",
    y = expression(bold("CO2 emission (mg kg"^{-1}*" day"^{-1}*")"))
  ) +
  theme_classic(base_size = 15) +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1.2
    ),
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      face = "bold",
      colour = "black",
      size = 12
    ),
    axis.text.y = element_text(
      face = "bold",
      colour = "black",
      size = 12
    ),
    axis.title.x = element_text(
      face = "bold",
      colour = "black",
      size = 16
    ),
    axis.title.y = element_text(
      face = "bold",
      colour = "black",
      size = 16
    ),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Display figure
CO2_graph

# 9. Save figure
dir.create("figures", showWarnings = FALSE)

ggsave(
  "figures/co2_day1_graph.png",
  plot = CO2_graph,
  width = 5.3,
  height = 5.3,
  dpi = 600
)
