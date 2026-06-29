
library(tidyverse)
library(plotly)
library(htmlwidgets)

# Filter data for Machine 1
df_machine1 <- X031.assignment.2 %>% filter(Machine == 1)

# Convert Pressure to a factor for ANOVA
df_machine1$Pressure <- factor(df_machine1$Pressure)

# Perform ANOVA
anova_result <- aov(PartResistance ~ Pressure, data = df_machine1)
cat("
ANOVA Results:
")
print(summary(anova_result))

# Boxplot of PartResistance by Pressure for Machine 1
p <- ggplot(df_machine1, aes(x = Pressure, y = PartResistance, fill = Pressure)) +
  geom_boxplot() +
  labs(
    title = "Part Resistance by Pressure (Machine 1)",
    x = "Pressure",
    y = "Part Resistance"
  ) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", size = 1, alpha = 0.7) + # USL
  annotate("text", x = Inf, y = 10, label = "USL = 10", vjust = -0.5, hjust = 1.1, color = "red", size = 5) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "none",
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white")
  )

# Convert to plotly object
plotly_plot <- ggplotly(p)

# Save as HTML widget
saveWidget(plotly_plot, "media/plots/boxplot_pressure_resistance.html", selfcontained = TRUE)

# Save R code
writeLines(R_CODE_PART_RESISTANCE_BOXPLOT, "media/plots/boxplot_pressure_resistance.md")
print("ANOVA and Boxplot generated and saved.")
