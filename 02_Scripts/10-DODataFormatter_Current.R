## Format for the DO Sensor data
##created on 16Dec2025
##Authors Hannah Cane 

library(readr) # Reads data
library(dplyr) # Splits data
library(tidyverse)

#Which KOr export file are you working with - change it here####
Do_profile<-"Minnewaska_FieldData2025.csv"

# Adds a list of global variables into the current environment from 00_Globals.r
# This portion of the code assumes that you are in the root of the repository
# - Root <- This is the root of the repository
#   - 01_Data
#     - MHK_Data
#       - ...
#   - 02_Scripts
#     - ...
#   - 03_Graphs
#     - ...
#   - ...
PATH_TO_DATA <- file.path("01_Data")

# Path to the data directory if the working directory is the scripts (or any of the other sub-directory) directory
# - Root
#   - 01_Data
#     - MHK_Data
#       - ...
#   - 02_Scripts <- Working directory
#     - ...
#   - 03_Graphs
#     - ...
#   - ...
if (!dir.exists(PATH_TO_DATA)) {
  PATH_TO_DATA <- file.path("..", "01_Data")
}

# If the directory fails to exist in both cases, then stop the program because
# the program is not running in a supported working directory
if (!dir.exists(PATH_TO_DATA)) {
  stop(paste("This script is being run from ", getwd(), " which is not supported"))
}

# Directory that holds all of the data pertaining to Mohonk
MOHONK_DATA_DIR <- file.path(PATH_TO_DATA, "MHK_Data")

# Directory containing unformatted data exported from Kor Software
DO_UNFORMATTED_DATA_DIR <- file.path(MOHONK_DATA_DIR, "DOSensor", "DO_profile")

# CSV Containing all of the exported data from Kor Software
DO_UNFORMATTED_DATA_ALL <- file.path(DO_UNFORMATTED_DATA_DIR, Do_profile)

# Directory containing all of the formatted data from Kor Software and formatted with a script
DO_FORMATTED_DATA_DIR <- file.path(MOHONK_DATA_DIR, "DOSensor", "DO_correct_format")


# This is the path to the exported Kor file
DO_file_path <- DO_UNFORMATTED_DATA_ALL



## Guess the encoding of the file so that it can be read properly by R
# The return value is a tibble that is sorted from least probable to most probable
# This code assumes that the encoding with the highest probability is the correct one
possible_encoding <- as.character(guess_encoding(DO_file_path)[1, 1])



## R fails to read files encoded in ASCII for some reason; however, there's a solution!
# Because UTF-8 is a superset of ASCII, all ASCII characters are UTF-8 characters!
# So far, there have been no issues with treating ASCII files like UTF-8 files
if (possible_encoding == "ASCII") {
  possible_encoding <- "UTF-8"
}

## Kor creates "rep=," at the top of the file and it messes up R so this will remove that
# Remember to read the file with the correct encoding!
raw_file <- file(description = DO_file_path, encoding = possible_encoding)

# Read each line and check to see if "rep=" is the first line
lines <- readLines(raw_file)


if (lines[1] == "sep=,") {
  lines <- lines[-1] # Removes the first line
}

# Write the changes
writeLines(lines, DO_file_path)

# WARNING! This function will re-encode and modify the data (it deletes the first line)!

## Update the possible encoding
possible_encoding <- as.character(guess_encoding(DO_file_path)[1, 1])


## R fails to read files encoded in ASCII for some reason; however, there's a solution!
# Because UTF-8 is a superset of ASCII, all ASCII characters are UTF-8 characters!
# So far, there have been no issues with treating ASCII files like UTF-8 files
if (possible_encoding == "ASCII") {
  possible_encoding <- "UTF-8"
}


## Read the data from the modified (or not modified) Kor file
# Because it's a CSV file the delimiter is ","
exported_DO_file_data <- read_delim(DO_file_path, delim = ",", locale = locale(encoding = possible_encoding))

## Now, it's time to split the data by dates
# Using group_by, each element can be grouped according to its date
# Then, using group_split each group will be put into a separate tibble and then they all are stored in a list
split_data <- exported_DO_file_data |> 
  group_by(collect_date, site_no) |>
  group_split()


## Helper function
# Creates the name for the file when it's saved
create_file_name <- function (lakeID,date, error_appended = "") {
  return(paste(lakeID,"_", date, "_DOprobe", error_appended, ".csv", sep = ""))
}

## Now, it's time to export all of the data in the correct format!
# for each dataframe in the split_data dataframe, do
##data.index <- 1
for (data.index in 1:length(split_data)) {
  #Split out the single data frame####
  DO.df <- split_data[[data.index]]
  
  
  # Create a data frame with the headings of the formatted csv
  formatted_data <- DO.df %>%
    dplyr::select(
      Date  = starts_with("collect_da"),
      temp_degC  = starts_with("Temp_C_DOProbe"),
      doConcentration_mgpL = starts_with("DO_mg"),
      doSaturation_percent = starts_with("DO_Sat"), 
      Depth_m= "Depth_m",
      site_no) %>% 
     mutate(
      lakeID = case_when(
        site_no == "Osiris" ~ "OSR",
        site_no == "MOHK_D10" ~ "MHK",
        TRUE ~ NA_character_)) %>% 
    mutate(Date=as.Date(parse_date_time(Date, orders = c("ymd", "mdy", "dmy"))))
  
  #Get out the shortened lake name
  lakeID<-formatted_data$lakeID[1]
  
  # Format the date to be file name friendly
  date <- formatted_data$Date[1]
  date_as_string <- sub("(\\d+)/(\\d+)/(\\d{4})$", "\\3/\\1/\\2", as.character(date))
  
  ## If there are no leading zeros for the month or day, then append a 0 to the string
  date_split <- strsplit(date_as_string, "/")
  
  year <- substr(date_split,1,4)
  month <- substr(date_split,6,7)
  day <- substr(date_split,9,10)
  
  
  if (nchar(day) < 2) day <- paste("0", day, sep = "")
  if (nchar(month) < 2) month <- paste("0", month, sep = "")
  
  # Converts the date to a string
  date_as_string <- paste(year, month, day, sep = "_")
  
  # Path to the current file being created
  current_file_to_save <- file.path(DO_FORMATTED_DATA_DIR, create_file_name(lakeID,date_as_string))
  
  
  # Ensure the file with mistakes does not exist already
  readr::write_csv(formatted_data, current_file_to_save)
  
  
}

