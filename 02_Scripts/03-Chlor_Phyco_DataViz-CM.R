#Chlorophyll and Phycocyanin Data Visualization#
# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data
data <- read.csv("C:\\Users\\cyrus\\OneDrive\\Documents\\MHK_2024_06_05_profile.csv")

# Subset the data to include only Depth._m, chlorophyll._RFU, and phycocyaninBGA_RFU.14C102008, filtering for depths up to 12 meters
temp_data <- subset(data, Depth._m < 12, 
                    select = c("Depth._m", "chlorophyll._RFU", "phycocyaninBGA_RFU.14C102008"))

# Reshape the data to long format for chlorophyll
chlorophyll_data <- temp_data %>% 
  select(Depth._m, chlorophyll._RFU) %>%
  rename(Value = chlorophyll._RFU) %>%
  mutate(Parameter = "Chlorophyll")

# Reshape the data to long format for phycocyanin
phycocyanin_data <- temp_data %>% 
  select(Depth._m, phycocyaninBGA_RFU.14C102008) %>%
  rename(Value = phycocyaninBGA_RFU.14C102008) %>%
  mutate(Parameter = "Phycocyanin")

# Combine both datasets
combined_data <- bind_rows(chlorophyll_data, phycocyanin_data)

# Create the plot
p <- ggplot(combined_data, aes(x = Value, y = Depth._m, color = Parameter)) +
  # Solid circles without black outline
  geom_point(size = 3, shape = 16, fill = "white", stroke = 0.5) +  
  # Reverse y-axis to have 0 at the top and 12 at the bottom
  scale_y_reverse(limits = c(12, 0)) + 
  # Set x-axis limits from -1 to 2
  scale_x_continuous(limits = c(-1, 2)) +  
  #labs(x = "Value", y = "Depth (m)", color = "Parameter") +
  theme_minimal() +
  theme(
    text = element_text(size = 1.75 * 12),  # Increase text size by 1.5 times (assuming base size 12)
    #panel.background = element_rect(fill = "white"),  # White background for the plot area
    #strip.text = element_blank()  # Remove strip text from the facets
  ) +
  facet_wrap(~ Parameter, scales = "free_y", nrow = 1)  # One row for both parameters

# Save the plot with dimensions of 8 inches by 6 inches
ggsave("C:\\Users\\cyrus\\OneDrive\\Documents\\Chlor_Phyco.png", plot = p, width = 14, height = 6, units = "in")

