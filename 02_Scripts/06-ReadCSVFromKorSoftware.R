#Title - ReadCSVFromKorSoftware
# Description - script for formatting kor software
# Author - Waheed Saroyia and Chrisopher Jamieson


# Load necessary library
library(readr)
library(knitr)

# Sets the working directory to .kor_exports because the exported files are stored there
setwd(".kor_exports")

file_name <- file.path("MHK_Everything.csv")

# Read the CSV file with the appropriate encoding
#reads in the csv into tab(table) 
tab <- read_csv(file_name, col_names = FALSE, locale = locale(encoding = "UTF-8"))

#gets rid of the heading in the table
tab <- tab[-1, ]

#if there are 49 rows in the table then make a vector from 12 - 0 with step -.25
#else fill the vector with NA with the vector being how many rows are in tab
if (nrow(tab) == 49) {
  depths_vector <- seq(from = 12, to = 0, by = -0.25)
} else {
  depths_vector <- rep(NA, nrow(tab))
}

# Create a data frame with the headings of the formatted csv
data <- data.frame(
   lakeID = rep("MHK", nrow(tab)),
   Date = tab$X2,
   Time = tab$X1,
   Depth_m = depths_vector,
   temp_degC = tab$X24,
   doConcentration_mgpL = tab$X15,
   doSaturation_percent = tab$X14,
   chlorophyll_RFU = tab$X8,
   phycocyaninBGA_RFU_14C102008 = tab$X6,
   turbidity_Fnu = rep(NA, nrow(tab)),
   pH = tab$X20,
   orp_MV = rep(NA, nrow(tab)),
   specificConductivity_uSpcm = tab$X10,
   salinity_psu = tab$X12,
   tds_mgpL = tab$X11,
   waterPressure_barA = rep(NA, nrow(tab)),
   latitude = tab$X17,
   longitude = tab$X18,
   altitude_m = tab$X19,
   barometerAirHandheld_mbars = tab$X7
)

#flips the rows of the table
data <- data[nrow(data):1, ]

#views the tibble
View(data)

# Format the date to be file name friendly
date <- gsub("[^0-9]", "_", tab$X2[1])  # Remove any non-numeric characters


# Save the data frame to a CSV file with the new file path
write_csv(data, paste("MHK_", date, ".csv", sep = ""))