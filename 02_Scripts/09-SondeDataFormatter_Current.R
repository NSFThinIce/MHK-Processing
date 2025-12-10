## This script is the current main script that formats the Kor data into the MHK format
## Authors - Hannah Cane 
## Adapted from Waheed Saroyia and Chrisopher Jamieson

### [INSTRUCTIONS]
# In order to have the data be automatically formatted, the exported CSV file must be renamed to
# "AllExports.csv" and put into the "KorFormat" folder. Then, this script can be exectuted.

## Load libraries being used
library(readr) # Reads data
library(dplyr) # Splits data

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
KOR_UNFORMATTED_DATA_DIR <- file.path(MOHONK_DATA_DIR, "EXO1Sonde", "KorFormat")

# CSV Containing all of the exported data from Kor Software
KOR_UNFORMATTED_DATA_ALL <- file.path(KOR_UNFORMATTED_DATA_DIR, "KorExport_2024_06_05_to_2025_05_07.csv")

# This is the path to the exported Kor file
kor_file_path <- KOR_UNFORMATTED_DATA_ALL


## Guess the encoding of the file so that it can be read properly by R
# The return value is a tibble that is sorted from least probable to most probable
# This code assumes that the encoding with the highest probability is the correct one
possible_encoding <- as.character(guess_encoding(kor_file_path)[1, 1])


## R fails to read files encoded in ASCII for some reason; however, there's a solution!
# Because UTF-8 is a superset of ASCII, all ASCII characters are UTF-8 characters!
# So far, there have been no issues with treating ASCII files like UTF-8 files
if (possible_encoding == "ASCII") {
  possible_encoding <- "UTF-8"
}

## Kor creates "rep=," at the top of the file and it messes up R so this will remove that
# Remember to read the file with the correct encoding!
raw_file <- file(description = kor_file_path, encoding = possible_encoding)

# Read each line and check to see if "rep=" is the first line
lines <- readLines(raw_file)

if (lines[1] == "sep=,") {
  lines <- lines[-1] # Removes the first line
}

# Write the changes
writeLines(lines, kor_file_path)
# WARNING! This function will re-encode and modify the data (it deletes the first line)!

## Update the possible encoding
possible_encoding <- as.character(guess_encoding(kor_file_path)[1, 1])

## R fails to read files encoded in ASCII for some reason; however, there's a solution!
# Because UTF-8 is a superset of ASCII, all ASCII characters are UTF-8 characters!
# So far, there have been no issues with treating ASCII files like UTF-8 files
if (possible_encoding == "ASCII") {
  possible_encoding <- "UTF-8"
}

## Read the data from the modified (or not modified) Kor file
# Because it's a CSV file the delimiter is ","
exported_kor_file_data <- read_delim(kor_file_path, delim = ",", locale = locale(encoding = possible_encoding))

## Now, it's time to split the data by dates
# Using group_by, each element can be grouped according to its date
# Then, using group_split each group will be put into a separate tibble and then they all are stored in a list
split_data <- exported_kor_file_data |> 
  group_by(DATE) |>
  group_split()


## Helper function
# Creates the name for the file when it's saved
create_file_name <- function (date, error_appended = "") {
  return(paste("MHK_", date, "_profile", error_appended, ".csv", sep = ""))
}

## Now, it's time to export all of the data in the correct format!
# for each dataframe in the split_data dataframe, do
for (dataframe in split_data) {
  ## Overwrite the dataframe's column names for convenience 
  # (The names are X1 to XN where N is the number of columns. This was used before these two scripts 
  # were combined. idk why it was done this way but I don't see any issues with it as of now)
  colnames(dataframe) <- paste("X", 1:ncol(dataframe), sep = "")
  
  #if there are 49 rows in the table then make a vector from 12 - 0 with step -.25
  #else fill the vector with NA with the vector being how many rows are in tab
  #and set data_input_error = true
  data_input_error <- FALSE
  
  # The number of rows in the current dataframe
  dataframe_nrow <- nrow(dataframe)
  
  if (dataframe_nrow == 49) {
    # No error in the data
    depths_vector <- seq(from = 12, to = 0, by = -0.25)
  } else {
    data_input_error <- TRUE
    depths_vector <- rep(NA, nrow(dataframe))
    
    if (dataframe_nrow > 49)
      # Too much data
      error_type <- "[TOO MUCH]"
    else
      # Too little data
      error_type <- "[TOO LITTLE]"
  }
  
  # Create a data frame with the headings of the formatted csv
  formatted_data <- dataframe %>% data.frame(
    lakeID = rep("MHK", nrow(dataframe)), 
    Depth_m = depths_vector, 
    turbidity_Fnu = rep(NA, nrow(dataframe)), 
    orp_MV = rep(NA, nrow(dataframe)),)
    rename(Date = starts_with("DATE"), 
           Time = starts_with("TIME"),
           temp_degC = starts_with("TEMP"),
           doConcentration_mgpL = starts_with("DO (MG/L)"),
           doSaturation_percent = starts_with("DO (% SAT)"),
           chlorophyll_RFU = starts_with("CHLOROPHYLL (RFU)"),
           phycocyaninBGA_RFU_14C102008 = starts_with("PHYCOCYANIN (RFU)"),
           pH = starts_with("pH"),
           specificConductivity_uSpcm = starts_with("SP COND (µS/CM)") ,
           salinity_psu = starts_with("SAL (PSU)"),
    tds_mgpL = dataframe$X11,
    waterPressure_barA = rep(NA, nrow(dataframe)),
    latitude = dataframe$X17,
    longitude = dataframe$X18,
    altitude_m = dataframe$X19,
    barometerAirHandheld_mbars = dataframe$X7
  )
  
  #flips the rows of the dataframe
  formatted_data <- formatted_data[nrow(formatted_data):1, ]
  
  # Format the date to be file name friendly
  date <- dataframe$X2[1]
  date_as_string <- sub("(\\d+)/(\\d+)/(\\d{4})$", "\\3/\\1/\\2", as.character(date))
  
  ## If there are no leading zeros for the month or day, then append a 0 to the string
  date_split <- strsplit(date_as_string, "/")
  
  year <- date_split[[1]][1]
  month <- date_split[[1]][2]
  day <- date_split[[1]][3]
  
  if (nchar(day) < 2) day <- paste("0", day, sep = "")
  if (nchar(month) < 2) month <- paste("0", month, sep = "")
  
  # Converts the date to a string
  date_as_string <- paste(year, month, day, sep = "_")
  
  # Path to the current file being created
  current_file_to_save <- file.path(KOR_FORMATTED_DATA_DIR, create_file_name(date_as_string))
  
  # If the file is already there, then DO NOT overwrite it!
  if (!file.exists(current_file_to_save)) {
    # Save the data frame to a CSV file with the new file path
    # If the file has mistakes, append [MISTAKE FOUND] to the file name
    if (data_input_error == TRUE)
      save_file_name <- create_file_name(date_as_string, error_type)
    else
      save_file_name <- create_file_name((date_as_string))
    
    # Since this file has mistakes, its name will be different an therefore current_file_to_save must be overwritten
    current_file_to_save <- file.path(KOR_FORMATTED_DATA_DIR, save_file_name)
    
    # Ensure the file with mistakes does not exist already
    if (!file.exists(current_file_to_save))
      readr::write_csv(formatted_data, current_file_to_save)
    
  }
}