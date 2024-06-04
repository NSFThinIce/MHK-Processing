# Loads the tidyverse packages which are being utilized below
library(tidyverse)

# Loads the MohonkLake ice data from the CSV file
# On other operating systems
MohonkLake_IceData <- readr::read_csv(file = "./01_Data/IceData/MohonkLake-IceOnIceOff-1932-2024.csv")

# Converts the dates stored in the data set into "date-time objects"
# Type "?POSIXct" for more information on the formatting

MohonkLake_IceData_As_DateTimeObjects <- MohonkLake_IceData |>
    dplyr::mutate(
        ICEIN_1 = strptime(ICEIN_1, format = "%m/%d/%Y"),
        ICEOUT_1 = strptime(ICEOUT_1, format = "%m/%d/%Y"),
        ICEIN_2 = strptime(ICEIN_2, format = "%m/%d/%Y"),
        ICEOUT_2 = strptime(ICEOUT_2, format = "%m/%d/%Y"),
        ICEIN_3 = strptime(ICEIN_3, format = "%m/%d/%Y"),
        ICEOUT_3 = strptime(ICEOUT_3, format = "%m/%d/%Y"),
    )

# This code appends new columns to the MohonkLake_IceData_As_DateTimeObjects data frame
# called "Ice length" which represents the amount of time in days of how long it took for
# the ice to retreat
MohonkLake_IceData_With_TimeLength <- MohonkLake_IceData_As_DateTimeObjects |>
    dplyr::mutate(
        # Ceiling is used to round up to prevent having "fractional" days occur
        ICE_LENGTH1 = ceiling(difftime(ICEOUT_1, ICEIN_1, units = "days")),
        ICE_LENGTH2 = ceiling(difftime(ICEOUT_2, ICEIN_2, units = "days")),
        ICE_LENGTH3 = ceiling(difftime(ICEOUT_3, ICEIN_3, units = "days"))
    )
