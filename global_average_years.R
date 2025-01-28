# Calculate yearly averages for both metrics
yearly_avg <- combined_data %>%
  group_by(year) %>%
  summarise(
    avg_life_exp = mean(life_expectancy, na.rm = TRUE),
    avg_health_exp = mean(health_expenditure, na.rm = TRUE)
  )

# Create the plot with yearly averages
ggplot(yearly_avg, 
       aes(x = avg_health_exp, y = avg_life_exp)) +
  # Add line
  geom_line(size = 1, color = "steelblue") +
  # Add points
  geom_point(size = 2, color = "steelblue") +
  # Add labels only for first and last year
  geom_text(data = yearly_avg %>% filter(year %in% c(1995, 2010)),
            aes(label = year), 
            vjust = -0.5, 
            hjust = 0.5) +
  # Add correlation coefficient
  annotate("text", 
           x = min(yearly_avg$avg_health_exp), 
           y = max(yearly_avg$avg_life_exp),
           label = paste("Correlation:", 
                         round(cor(yearly_avg$avg_health_exp, 
                                   yearly_avg$avg_life_exp), 3)),
           hjust = 0) +
  # Add arrow annotation
  annotate("text",
           x = mean(yearly_avg$avg_health_exp),
           y = min(yearly_avg$avg_life_exp),
           label = "→ Time",
           hjust = -0.1) +
  # Customize labels and theme
  labs(
    title = "Global Average: Life Expectancy vs Health Expenditure (1995-2010)",
    x = "Average Health Expenditure",
    y = "Average Life Expectancy (years)",
    caption = "Source: Combined health and life expectancy dataset"
  ) +
  theme_minimal()