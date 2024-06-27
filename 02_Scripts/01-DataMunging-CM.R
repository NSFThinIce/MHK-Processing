##Mohonk Sensor data####
##Created 20Jan2023 by David Richardson (hereafter DCR)
##Cyrus data efforts goes here####
#Mohonk Lake 1896-2023 Temperature Data
#Load Libraries
library(ggplot2)
library(tidyr)

# Read data from CSV file
data <- read.csv("C:/Users/cyrus/OneDrive/Documents/GitHub/MHK_summer2024/01_Data/MohonkPreserveWeatherData-1896-2023-NOAA-NCEI-metric_allCompiled.csv")

# Convert Date column to Date class
data$Date <- as.Date(data$Date)

# Remove rows with NA or empty values in TempMax_degC, TempMin_degC, and TempMean_degC columns
data <- na.omit(data[, c("Date", "TempMax_degC", "TempMin_degC", "TempMean_degC")])

# Calculate the average TempMax_degC for each year
average_temp_max <- aggregate(TempMax_degC ~ format(Date, "%Y"), data, mean)

# Calculate the average TempMin_degC for each year
average_temp_min <- aggregate(TempMin_degC ~ format(Date, "%Y"), data, mean)

# Calculate the average TempMean_degC for each year
average_temp_mean <- aggregate(TempMean_degC ~ format(Date, "%Y"), data, mean)

# Determine the y-axis limits to include TempMax_degC, TempMin_degC, and TempMean_degC
y_min <- min(average_temp_max$TempMax_degC, average_temp_min$TempMin_degC, average_temp_mean$TempMean_degC)
y_max <- 20  # Extend the Y-axis to 20 degrees Celsius

# Plot TempMax_degC with a box
plot(as.Date(average_temp_max$`format(Date, "%Y")`, format="%Y"), 
     average_temp_max$TempMax_degC, 
     type = "b", 
     xlab = "Year", 
     ylab = "Celsius", 
     col = "red",
     ylim = c(y_min, y_max),  # Set y-axis limits
     axes = FALSE)  # Turn off default axes

# Add a box around the plot
box()

# Calculate years to show tick marks every 10 years starting from 1900
years <- seq(as.Date("1900-01-01"), max(as.Date(average_temp_max$`format(Date, "%Y")`, format="%Y")), by = "10 years")

# Add custom x-axis with 10-year intervals
axis(side = 1, at = as.Date(years), labels = format(as.Date(years), "%Y"))

# Calculate intermediate 5-year intervals for x-axis
intermediate_years <- seq(as.Date("1905-01-01"), max(as.Date(average_temp_max$`format(Date, "%Y")`, format="%Y")), by = "10 years")

# Add tick marks but no labels for intermediate 5-year intervals on x-axis
axis(side = 1, at = as.Date(intermediate_years), labels = FALSE, tck = -0.02)

# Calculate the y-axis tick marks at 5-degree intervals
y_ticks <- seq(floor(y_min / 5) * 5, 20, by = 5)  # Extend the y-ticks to 20 degrees

# Add custom y-axis with 5-degree intervals
axis(side = 2, at = y_ticks, labels = y_ticks)

# Calculate intermediate 2.5-degree intervals for y-axis
# Extend the intermediate y-ticks to 20 degrees
intermediate_y_ticks <- seq(floor(y_min / 2.5) * 2.5, 20, by = 2.5)  

# Add tick marks but no labels for intermediate 2.5-degree intervals on y-axis
axis(side = 2, at = intermediate_y_ticks, labels = FALSE, tck = -0.02)

# Plot TempMin_degC
points(as.Date(average_temp_min$`format(Date, "%Y")`, format="%Y"), 
       average_temp_min$TempMin_degC, 
       type = "b", 
       col = "green")

# Plot TempMean_degC
points(as.Date(average_temp_mean$`format(Date, "%Y")`, format="%Y"), 
       average_temp_mean$TempMean_degC, 
       type = "b", 
       col = "blue")

# Add a legend
legend("top", 
       legend = c("Annual Max Temp", "Annual Min Temp", "Annual Mean Temp"), 
       col = c("red", "green", "blue"), 
       pch = c(1, 1, 1),
       bty = "n",
       # Allows plotting outside of the plot area
       xpd = TRUE,  
       # Adjust the inset to position the legend on the X-axis
       inset = c(0, -0.2),  
       # Display the legend horizontally
       horiz = TRUE)  

# Add lines of best fit for TempMax_degC, TempMin_degC, and TempMean_degC
fit_max <- lm(TempMax_degC ~ as.numeric(as.Date(average_temp_max$`format(Date, "%Y")`, format="%Y")), data = average_temp_max)
fit_min <- lm(TempMin_degC ~ as.numeric(as.Date(average_temp_min$`format(Date, "%Y")`, format="%Y")), data = average_temp_min)
fit_mean <- lm(TempMean_degC ~ as.numeric(as.Date(average_temp_mean$`format(Date, "%Y")`, format="%Y")), data = average_temp_mean)

# Add the lines of best fit to the plot
# Dashed line for TempMax_degC
abline(fit_max, col = "red", lwd = 2, lty = 2)
# Dashed line for TempMin_degC
abline(fit_min, col = "green", lwd = 2, lty = 2) 
# Dashed line for TempMean_degC
abline(fit_mean, col = "blue", lwd = 2, lty = 2)  

ggsave("MohonkLake-TemperatureData-1896-2023.png", width = 12, height = 6, units = "in")
