## This script is a combining of both 07-SplitSondeData.R and 06-ReadCSVFromKorSoftware.R 
# which were used to automatically format the data exported from Kor. Now, this script handles
# that
## Authors - Waheed Saroyia and Chrisopher Jamieson

### [INSTRUCTIONS]
# In order to have the data be automatically formatted, the exported CSV file must be renamed to
# "ExportedData" and put into the "KorExports" folder. Then, this script can be exectuted.

## ALSO, ensure that you are executing this either in the folder "On Thin Ice Graphs" or the folder
# "02_Scripts". The script will yell at you if neither of these conditions are true!

## Ensures that the current working directories are supported by the script
# getwd() gets the current working directory; an absolute path to the current folder (C:\Users\John\example.pdf)
# strsplit splits a string at specific patterns. The pattern in this case is ".Platform$file.sep" which
# is a dataframe that has data about the OS that R is running on. For this case, this script is using file.sep
# to get the seperator that the current platform is using and then splitting the string around that seperator
# OS seperators are either '\' on Windows or '/' on Unix based systems (Mac and Linux)

current_folder <- getwd()
folders_to_file <- strsplit(current_folder, .Platform$file.sep)
how_many_folders_deep <- length(folders_to_file[[1]])
root_folder <- folders_to_file[[1]][how_many_folders_deep]

prefixed_text <- FALSE
if (root_folder == "02_Scripts")
  # .. Will be added as a prefix to the path constructed at file.path below
  prefixed_text <- TRUE

## Load libraries being used
library(readr) # Reads data
library(dplyr) # Splits data

## Constructs an OS independent path to the exported Kor file
if (prefixed_text == TRUE) {
  kor_file_path <- file.path("..", "KorExports", "ExportedData.csv")
} else {
  kor_file_path <- file.path("KorExports", "ExportedData.csv")
}

## Ensure the file exists
if (!file.exists(kor_file_path)) {
  stop(paste("The file path '", kor_file_path, "' does not lead to a file.\n Did you put the exported CSV file into the KorExports folder?\n Also, did you name it ExportedData?"))
}

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

## Save each tibble in their own file
# The name of each file is of the format KOR_SAMPLING_DAY_`Date of sampling`_EXPORT.csv

for (dataframe in split_data) {
  #if there are 49 rows in the table then make a vector from 12 - 0 with step -.25
  #else fill the vector with NA with the vector being how many rows are in tab
  if (nrow(dataframe) == 49) {
    depths_vector <- seq(from = 12, to = 0, by = -0.25)
  } else {
    depths_vector <- rep(NA, nrow(dataframe))
  }

  # Create a data frame with the headings of the formatted csv
  data <- data.frame(
     lakeID = rep("MHK", nrow(dataframe)),
     Date = dataframe$X2,
     Time = dataframe$X1,
     Depth_m = depths_vector,
     temp_degC = dataframe$X24,
     doConcentration_mgpL = dataframe$X15,
     doSaturation_percent = dataframe$X14,
     chlorophyll_RFU = dataframe$X8,
     phycocyaninBGA_RFU_14C102008 = dataframe$X6,
     turbidity_Fnu = rep(NA, nrow(dataframe)),
     pH = dataframe$X20,
     orp_MV = rep(NA, nrow(dataframe)),
     specificConductivity_uSpcm = dataframe$X10,
     salinity_psu = dataframe$X12,
     tds_mgpL = dataframe$X11,
     waterPressure_barA = rep(NA, nrow(dataframe)),
     latitude = dataframe$X17,
     longitude = dataframe$X18,
     altitude_m = dataframe$X19,
     barometerAirHandheld_mbars = dataframe$X7
  )
  #flips the rows of the dataframe
  data <- data[nrow(data):1, ]

  #views the tibble
  # View(data)
  stop()

  # Format the date to be file name friendly
  date <- gsub("[^0-9]", "_", dataframe$X2[1])  # Remove any non-numeric characters

  # Save the data frame to a CSV file with the new file path
  write_csv(
    data, 
    file.path("KorExports", 
      paste("MHK_", gsub("/", "_", as.character(date)), "_profile.csv", sep = "")))
}

# write_csv(
#     dataframe,
#     paste( # Concatenates all of the strings
#       sep = "", # The separater between each string will be nothing (by default it's a blank space)
#       "MHK_",
#       gsub("/", "_", as.character(dataframe$DATE[1])), # Replaces the slashes with underscores
#       # and as.character converts the value returned from dataframe$DATE[1] to a string
#       "_EXPORT.csv"
#     )
#   )