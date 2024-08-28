# Load necessary library
library(readr)
library(knitr)

# Read the CSV file with the appropriate encoding
tab <- read_csv("KOR_SAMPLING_DAY_8-28-2024_EXPORT.csv",col_names = FALSE, locale = locale(encoding = "UTF-8"))

tab <- tab[-1, ]

if (nrow(tab) == 49) {
  depths_vector <- seq(from = 0, to = 12, by = 0.25)
} else {
  depths_vector <- rep(NA, nrow(tab))
}

# Create a data frame with the specified headings
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

View(data)

write_csv(data,"KOR_SAMPLING_DAY_8-28-2024_EXPORT.csv")



