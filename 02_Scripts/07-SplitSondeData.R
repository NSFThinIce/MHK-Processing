## Load libraries being used
library(readr) # Reads data
library(dplyr) # Splits data

## Constructs an OS independent path to the exported Kor file
kor_file_path <- file.path("MHK_Everything.csv")

## Ensure the file exists
if (!file.exists(kor_file_path)) {
  stop(paste("The file path '", kor_file_path, "' does not lead to a file"))
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
  write_csv(
    dataframe,
    paste( # Concatenates all of the strings
      sep = "", # The separater between each string will be nothing (by default it's a blank space)
      "KOR_SAMPLING_DAY_",
      gsub("/", "-", as.character(dataframe$DATE[1])), # Replaces the slashes with dashes
      # and as.character converts the value returned from dataframe$DATE[1] to a string
      "_EXPORT.csv"
    )
  )
}
