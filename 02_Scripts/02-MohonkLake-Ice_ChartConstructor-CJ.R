# Loads the tidyverse packages which are being utilized below
library(tidyverse)
library(lubridate)

# Loads the MohonkLake ice data from the CSV file
raw_ice_data <- readr::read_csv(file = "./01_Data/IceData/MohonkLake-IceOnIceOff-1932-2024.csv")

# Creates a Dataframe with the columns:
# "Ice In" -> Date of ice appearing on lake
# "Ice Out" -> Date of ice recede on lake
# "Length" -> The amount of days the ice was on the lake before it receded
# round() is used to prevent fractional days from appearing (They occur because of Daylight Savings)
# Winter Year -> The current winter year [DESCRIPTION NEEDS UPDATING 6/12/24]

# To do this we must first: Combine all of the data into 2 columns (Ice In & Ice Out)
# We can use the "c" function which combines its arguments to a vector (array)
# The `` syntax allows us to declare a variable using special characters
ice_data_combined <- tibble(
  `Ice In` = c(raw_ice_data$ICEIN_1, raw_ice_data$ICEIN_2, raw_ice_data$ICEIN_3),
  `Ice Out` = c(raw_ice_data$ICEOUT_1, raw_ice_data$ICEOUT_2, raw_ice_data$ICEOUT_3)
)

# Then, filter out data with "NA"'s and "No date"'s
# dplyr::filter returns a tibble/dataframe where all the expressions passed evaluate to true
ice_data_filtered <- ice_data_combined |>
  dplyr::filter(!is.na(`Ice In`) & !is.na(`Ice Out`) & `Ice In` != "No date" & `Ice Out` != "No date")

# Now, there are only dates! Let's convert them to valid dates!
# dplyr::mutate returns a tibble/dataframe with all of the redefined variables (var = new value)
# set to a new value
ice_data_as_dates <- ice_data_filtered |>
  dplyr::mutate(
    `Ice In` = lubridate::mdy(`Ice In`, tz = "America/New_York"),
    `Ice Out` = lubridate::mdy(`Ice Out`, tz = "America/New_York")
  )

# Now, let's add "Length"
ice_data_with_length <- ice_data_as_dates |>
  dplyr::mutate(
    Length = round(difftime(`Ice Out`, `Ice In`, units = "days", tz = "America/New_York"))
  )

# Then, let's add "Winter Year"
ice_data_with_winter_year <- ice_data_with_length |>
  dplyr::mutate(
    `Winter Year` = as.character(lubridate::year(`Ice Out`))
  )

# Then, let's sort the dates!!
# dplyr::arrange takes a variable name and returns a tibble/dataframe with all of the rows
# sorted by said variable
ice_data_sorted <-  ice_data_with_winter_year |>
  dplyr::arrange(`Ice In`)

# Finally, let's update our column names
# dplyr::rename renames all of the variables on the right with the strings on the left
# (new variable name = current variable name)
ice_data_completed <- ice_data_sorted |>
  dplyr::rename(
    `Ice In (Year)` = `Ice In`,
    `Ice Out (Year)` = `Ice Out`,
    `Length (Days)` = `Length`
  )

# Now we are complete! Let's view the result...
View(ice_data_completed)

# # Now, let's graph it!
# ggplot(ice_data_completed, aes(
#     y = `Years`,
#     x = yday(`Ice In (Year)`),
#     xend = yday(`Ice Out (Year)`),
#     yend = `Years`
#   )
# )
