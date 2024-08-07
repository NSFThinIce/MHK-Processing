# Load libraries being used
library(readr)

# Constructs an OS independent path to the exported Kor file
kor_file_path <- file.path("Kor Export.csv")

# Guess the encoding of the file so that it can be read properly by R
# The return value is a tibble that is sorted from least probable to most probable
# This code assumes that the encoding with the highest probability is the correct one
possible_encoding <- as.character(guess_encoding(kor_file_path)[1, 1])

# R fails to read files encoded in ASCII for some reason; however, there's a solution!
# Because UTF-8 is a superset of ASCII, all ASCII characters are UTF-8 characters!
# So far, there have been no issues with treating ASCII files like UTF-8 files
if (possible_encoding == "ASCII") {
  possible_encoding <- "UTF-8"
}

# Remove rep=, if it exists in the file
raw_file <- readLines(kor_file_path, encoding = possible_encoding)
raw_file
# Read the data from the Kor file using the possible encoding
# Because it's a CSV file the delimiter is ","
exported_kor_file <- read_delim(kor_file_path, delim = ",", locale = locale(encoding = possible_encoding))