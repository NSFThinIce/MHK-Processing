# Title - Mohonk Stratification at 0m - 9m
# Description - This script calculates and plots the continuous temperature at different depths from sensor data.
# Author - Waheed Saroyia

# Load necessary libraries
library(tidyverse)
library(lubridate)

# Load the data
data <- read_csv(file.path('01_Data', 'MohonkSensor-AllData-01Apr2016to06Aug2018-ModifiedHeaders.csv'))

# Convert DateTime to POSIXct format
data$DateTime <- ymd_hms(data$DateTime)

# Filter data to start from 2016-11-17 12:15
data <- data %>% filter(DateTime >= ymd_hms("2016-11-17 12:15:00"))

# Reshape the data to long format
data_long <- data %>%
  select(DateTime, starts_with("Temp_")) %>%
  pivot_longer(cols = starts_with("Temp_"), names_to = "Depth", values_to = "Temperature")

# Remove 'Temp_' prefix from Depth names
data_long$Depth <- gsub("Temp_", "", data_long$Depth)

# Filter out negative temperature values
data_long <- data_long %>% filter(Temperature >= 0)

# Plot the data
plot <- ggplot(data_long, aes(x = DateTime, y = Temperature, color = Depth)) +
  geom_line() +
  labs(x = "Date",
       y = "Temperature (°C)",
       color = "Depth") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),        # Increase axis title font size
    axis.text = element_text(size = 12),         # Increase axis text font size
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1), # Increase x-axis text font size and angle
    legend.text = element_text(size = 12),       # Increase legend text (depth labels) font size
    plot.title = element_blank()                 # Remove the plot title
  )

# Display the plot
print(plot)

# Save the plot as JPEG with specified dimensions
ggsave(file.path('03_Graphs','05_continuous_temp_all_depths.jpeg'), plot = plot, width = 12, height = 6, dpi = 300, device = "jpeg")
