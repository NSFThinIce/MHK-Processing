#Temperature and Dissolved Oxygen Data Visualization#
# Libraries
# Load Libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data (adjust the file path as per your actual file location)
data <- read.csv("C:\\Users\\cyrus\\OneDrive\\Documents\\MHK_2024_06_05_profile.csv")

# Subset the data to include Depth._m, temp_degC, and doConcentration_mgpL, filtering for depths up to 12 meters
temp_data <- subset(data, Depth._m < 12, 
                    select = c("Depth._m", "temp_degC", "doConcentration_mgpL"))

# Reshape the data to long format
melted_data <- temp_data %>% 
  pivot_longer(cols = c(temp_degC, doConcentration_mgpL),
               names_to = "Parameter", values_to = "Value") %>%
  # Arrange by Parameter and Depth._m in descending order
  arrange(Parameter, desc(Depth._m))  

# Rename temp_degC as "Temperature (Celsius)" and doConcentration_mgpL as "Dissolved Oxygen (mg/L)"
melted_data$Parameter[melted_data$Parameter == "temp_degC"] <- "Temperature (Celsius)"
melted_data$Parameter[melted_data$Parameter == "doConcentration_mgpL"] <- "Dissolved Oxygen (mg/L)"

# Filter the data to get only the value at Depth._m == 0 for different shape
point_data <- melted_data %>% filter(Depth._m == 0)

# Create the plot
plot <- ggplot(melted_data, aes(x = Value, y = Depth._m, color = Parameter)) +
  # Solid circles for all data points
  geom_point(size = 3, shape = 16) + 
  # Hollow circle for Depth._m == 0
  geom_point(data = point_data, aes(x = Value, y = Depth._m), size = 3, shape = 1) +  
  # Reverse y-axis to have 0 at the top and 12 at the bottom
  scale_y_reverse(limits = c(12, 0)) +  
  labs(x = "Value", y = "Depth (m)", color = "Parameter") +
  theme_minimal() +
  theme(
    text = element_text(size = 1.75 * 12),
    # Set background to white
    panel.background = element_rect(fill = "white", color = "white")) +  
  scale_color_manual(values = c("Temperature (Celsius)" = "red", "Dissolved Oxygen (mg/L)" = "green"))

# Save the plot as a PNG file with dimensions 6 inches by 6 inches
ggsave("C:\\Users\\cyrus\\OneDrive\\Documents\\Temp_DO.png", plot = plot, width = 9.25, height = 5.5, units = "in", dpi = 300)
