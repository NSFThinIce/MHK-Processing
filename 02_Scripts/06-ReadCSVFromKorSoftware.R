# Loads the necessary libraries
library(tidyverse) # provides the tidyverse packages
library(lubridate) # provides useful functions when working with dates
library(assertthat) # provides is.string() function and several others

# This code automatically reads in a CSV file from Kor Software
# and re-formats it to follow the existing specification as defined here: [LINK TO FORMAT NEEDED 7/2/24]
reformat_kor_csv_file <- function(kor_csv_file_path) {
  # Ensure the data-type passed for kor_csv_file_path is a string
  # If it isn't a string, then the code below must not run
  if (!is.string(kor_csv_file_path)) {
    stop("kor_csv_file_path is not a string (vector/array of characters)")
  }

  # If the file does not exist, then the code below it should not run
  if (!file.exists(kor_csv_file_path)) {
    stop(c("The following path: '", kor_csv_file_path, "' does not point to anything"))
  }

  # Read the CSV file
  kor_csv_file <- readr::read_csv(file = kor_csv_file_path)

  # Begin the conversion process
  converted_kor_csv_file <- kor_csv_file |> tibble::add_column(
      `lakeID` = "MHK",
      .before = `TIME`
    ) |> dplyr::relocate(
      `DATE`,
      .before = `TIME`
    ) |> dplyr::rename(
      `Date` = `DATE`,
      `Time` = `TIME`
    ) |> tibble::add_column(
      `Depth_m` = seq(0, 12, 0.25),
      .after = `Time`
    ) |> dplyr::rename(
      `temp_degC` = `TEMP (°C)-15F102368`
    ) |> dplyr::mutate(
      `FAULT CODE` = NULL
    ) |> dplyr::relocate(
      `DO (MG/L)-13C101369`,
      .after = "temp_degC"
    ) |> dplyr::rename(
      `doConcentration_mgpL` = `DO (MG/L)-13C101369`
    ) |> dplyr::relocate(
      `DO (% SAT)-13C101369`,
      .after = `doConcentration_mgpL`
    ) |> dplyr::rename(
      `` = `DO (% SAT)-13C101369`
    )

?rename
}

reformat_kor_csv_file(readr::read_csv(file = file.path("PATH", "TO", "FILE")))

?dplyr::relocate
?ifelse
?stop
