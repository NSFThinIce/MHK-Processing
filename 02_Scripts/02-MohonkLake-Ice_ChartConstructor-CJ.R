# Loads the tidyverse and lubridate packages which are being utilized below
library(tidyverse)
library(lubridate)

# Reads the MohonkLake ice data from the CSV file and stores it in raw_ice_data
raw_ice_data <- readr::read_csv(file = "./01_Data/IceData/MohonkLake-IceOnIceOff-1932-2024.csv")

# The code below creates a Dataframe with the columns:
# "Ice In" -> Date of ice forming on lake
# "Ice Out" -> Date of ice melting on lake
# "Length" -> The amount of days the ice was on the lake before it melted
# "Winter Year" -> The year winter started
# "Freeze-thaw Cycle" -> Some years have multiple periods where ice forms and melts, this keeps
# track of those periods

# To do this we must first: Combine all of the data into 2 columns (Ice In & Ice Out)
# We can use the "c" function which combines its arguments to a vector (array for CS majors)
# The `` syntax allows us to declare a variable using special characters
# Also, to label each period, data is added in steps starting with ICEIN_1 and ICEOUT_1

# Adds all of the elements from ICEIN_1 and ICEOUT_1
ice_data_combined_phase_1 <- tibble(
  `Ice In` = raw_ice_data$ICEIN_1,
  `Ice Out` = raw_ice_data$ICEOUT_1,
  `Freeze-thaw Cycle` = "First"
)

# Adds all of the elements from ICEIN_2 and ICEOUT_2
ice_data_combined_phase_2 <- ice_data_combined_phase_1 |>
  tibble::add_row(
    `Ice In` = raw_ice_data$ICEIN_2,
    `Ice Out` = raw_ice_data$ICEOUT_2,
    `Freeze-thaw Cycle` = "Second"
  )

# Adds all of the elements from ICEIN_3 and ICEOUT_3
ice_data_combined_phase_3 <- ice_data_combined_phase_2 |>
  tibble::add_row(
    `Ice In` = raw_ice_data$ICEIN_3,
    `Ice Out` = raw_ice_data$ICEOUT_3,
    `Freeze-thaw Cycle` = "Third"
  )

# Now, all of the data has been combined
ice_data_combined <- ice_data_combined_phase_3

# To prevent lubridate from spitting out errors, all of the "No date" text is converted to NA
# However, if the data is not "No date", then it will stay the same
# dplyr::mutate returns a tibble/dataframe with all of the redefined variables (var = new value)
# set to a new value
ice_data_filtered <- ice_data_combined |>
  dplyr::mutate(
    `Ice In` = ifelse(`Ice In` == "No date", NA, `Ice In`),
    `Ice Out` = ifelse(`Ice Out` == "No date", NA, `Ice Out`)
  )

# Converts `Ice In` and `Ice Out` to valid dates using lubridate::mdy
ice_data_as_dates <- ice_data_filtered |>
  dplyr::mutate(
    `Ice In` = lubridate::mdy(`Ice In`, tz = "America/New_York"),
    `Ice Out` = lubridate::mdy(`Ice Out`, tz = "America/New_York")
  )

# The length of a freeze-thaw cycle is equal to the magnitude between when the ice arrived (Ice In)
# and the ice departed (Ice Out)
# round() is used to prevent fractional days from appearing (They occur because of Daylight Savings)
ice_data_with_length <- ice_data_as_dates |>
  dplyr::mutate(
    Length = round(difftime(`Ice Out`, `Ice In`, units = "days", tz = "America/New_York"))
  )

# Winter year is calculated by subtracting 1 year to the year the ice melted (Ice Out), however,
# if the year the ice melted occurred during December, then the winter year would be the same
# year
ice_data_with_winter_year <- ice_data_with_length |>
  dplyr::mutate(
    `Winter Year` = as.character(
      ifelse(
        lubridate::month(`Ice Out`) != 12,
        lubridate::year(`Ice Out` - lubridate::years(1)),
        lubridate::year(`Ice Out`)
      )
   )
  )

# Sorts the dates based on when the ice arrived (Ice In)
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
# View(ice_data_completed)
# Uncomment the above code to view the result

# Creates a graph using the fully processed tibble/dataframe from above and sets:
# The x-axis as "Winter Year"
# The y-axis as "Length (Days)"
# The colors for each bar depending on their "Freeze-thaw Cycle"
# The x scale goes by every 5 years and the y scale goes by every 10 days
ice_chart_graph_completed <- ice_data_completed |> ggplot(
  aes(
    x = `Winter Year`,
    y = `Length (Days)`,
    fill = `Freeze-thaw Cycle`
  )
) + geom_bar(
  stat = "identity",
  width = .7
) + scale_x_discrete(
  breaks = seq(1920, 2025, 5) 
) + scale_y_continuous(
  breaks = seq(0, 100, 10)
) + theme(
  text = element_text(size = 20),
  axis.text.x = element_text(angle = 45)
) + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# Saves the graph as a 12in = width by 6in = height PNG file
ggsave("MohonkLake-IceOnIceOff-1932-2024.png", plot = ice_chart_graph_completed, width = 12, height = 6, units = "in")