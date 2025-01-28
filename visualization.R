# Add ggpubr to the package list
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,      
  rio,            
  here,           
  skimr,          
  janitor,        
  epikit,         
  kableExtra,
  ggpubr         
)

# Calculate yearly averages
yearly_averages <- combined_data %>%
  group_by(year) %>%
  summarize(
    avg_life_exp = mean(life_expectancy, na.rm = TRUE),
    avg_health_exp = mean(health_expenditure, na.rm = TRUE)
  )

# Create scatter plot
ggplot(combined_data, 
       aes(x = health_expenditure, y = life_expectancy)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", fill = "pink", alpha = 0.2) +
  annotate("text", 
           x = min(combined_data$health_expenditure), 
           y = max(combined_data$life_expectancy),
           label = paste("Correlation:", 
                         round(cor(combined_data$health_expenditure, 
                                   combined_data$life_expectancy), 3)),
           hjust = 0) +
  labs(
    title = "Global Correlation: Life Expectancy vs Health Expenditure (1995-2010)",
    x = "Health Expenditure",
    y = "Life Expectancy (years)",
    caption = "Source: Combined health and life expectancy dataset"
  ) +
  theme_minimal()

