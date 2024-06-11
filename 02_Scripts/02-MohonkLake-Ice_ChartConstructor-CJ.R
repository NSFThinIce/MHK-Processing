# Loads the tidyverse packages which are being utilized below
library(tidyverse)
library(lubridate)

# Loads the MohonkLake ice data from the CSV file
raw_ice_data <- readr::read_csv(file = "./01_Data/IceData/MohonkLake-IceOnIceOff-1932-2024.csv")

# Creates a Dataframe with the columns:
# Ice In -> Date of ice appearing on lake
# Ice out -> Date of ice receeding on lake
# Length -> that stores the amount of the days the ice was at the lake before it receded
# round() is used to prevent fractional days from appearing (They occur because of Daylight Savings)

# To do this we must first: Combine all of the data into 2 columns (Ice In & Ice Out)
ice_data_combined <- tibble(
  "Ice In" = c(raw_ice_data$ICEIN_1, raw_ice_data$ICEIN_2, raw_ice_data$ICEIN_3),
  "Ice Out" = c(raw_ice_data$ICEOUT_1, raw_ice_data$ICEOUT_2, raw_ice_data$ICEOUT_3)
)

# Then, filter out data with "NA"'s and "No date"'s
ice_data_filtered <- ice_data_combined |>
  dplyr::filter(!is.na(`Ice In`) & !is.na(`Ice Out`) & `Ice In` != "No date" & `Ice Out` != "No date")

# Now, there are only dates! Let's convert them to valid dates!
ice_data_as_dates <- ice_data_filtered |> dplyr::mutate(
  `Ice In` = lubridate::mdy(`Ice In`),
  `Ice Out` = lubridate::mdy(`Ice Out`)
)

# Now, let's add length
ice_data_with_length <- ice_data_as_dates |> dplyr::mutate(
  Length = round(difftime(`Ice Out`, `Ice In`, units = "days", tz = "America/New_York"))
)

# Then, let's sort the dates!!
ice_data_complete <- ice_data_with_length |>
  arrange(`Ice In`)

# Now we are complete! Let's view the result...
View(ice_data_complete)
