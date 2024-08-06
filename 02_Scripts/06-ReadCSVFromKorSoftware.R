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
  kor_csv_file <- readr::read_csv(kor_csv_file_path, locale = readr::locale(encoding = "UTF-16LE"))

  # Begin the conversion process
  converted_kor_csv_file <- kor_csv_file |>
    tibble::add_column(
      `lakeID` = "MHK",
      .before = `TIME`
    ) |>
    dplyr::relocate(
      `DATE`,
      .before = `TIME`
    ) |>
    dplyr::rename(
      `Date` = `DATE`,
      `Time` = `TIME`
    ) |>
    tibble::add_column(
      `Depth_m` = seq(0, 12, 0.25),
      .after = `Time`
    ) |>
    dplyr::rename(
      `temp_degC` = `TEMP (°C)-15F102368`
    ) |>
    dplyr::mutate(
      `FAULT CODE` = NA
    ) |>
    dplyr::relocate(
      `DO (MG/L)-13C101369`,
      .after = "temp_degC"
    ) |>
    dplyr::rename(
      `doConcentration_mgpL` = `DO (MG/L)-13C101369`
    ) |>
    dplyr::relocate(
      `DO (% SAT)-13C101369`,
      .after = `doConcentration_mgpL`
    ) |>
    dplyr::rename(
      `doSaturation_percent` = `DO (% SAT)-13C101369`
    ) |>
    dplyr::relocate(
      `CHLOROPHYLL (RFU)-14C102008`,
      .after = `doSaturation_percent`
    ) |>
    dplyr::rename(
      `chlorophyll _RFU` = `CHLOROPHYLL (RFU)-14C102008`
    ) |>
    dplyr::relocate(
      `PHYCOCYANIN (RFU)-14C102008`,
      .after = `chlorophyll _RFU`
    ) |>
    dplyr::rename(
      `phycocyaninBGA_RFU-14C102008` = `PHYCOCYANIN (RFU)-14C102008`
    ) |>
    tibble::add_column(
      `turbidity_Fnu` = NA,
      .after = `phycocyaninBGA_RFU-14C102008`
    ) |>
    dplyr::relocate(
      `PH-15H100582`,
      .after = `turbidity_Fnu`
    ) |>
    dplyr::rename(
      `pH` = `PH-15H100582`
    ) |>
    tibble::add_column(
      `orp_MV` = NA,
      .after = `pH`
    ) |>
    dplyr::relocate(
      `SP COND (µS/CM)-15F102368`,
      .after = `orp_MV`
    ) |>
    dplyr::rename(
      `specificConductivity_uSpcm` = `SP COND (µS/CM)-15F102368`
    ) |>
    dplyr::relocate(
      `SAL (PSU)-15F102368`,
      .after = `specificConductivity_uSpcm`
    ) |>
    dplyr::rename(
      `salinity_psu` = `SAL (PSU)-15F102368`
    ) |>
    dplyr::relocate(
      `TDS (MG/L)-15F102368`,
      .after = `salinity_psu`
    ) |>
    dplyr::rename(
      `tds_mgpL` = `TDS (MG/L)-15F102368`
    ) |>
    tibble::add_column(
      `waterPressure_barA` = NA,
      .after = "tds_mgpL"
    ) |>
    dplyr::relocate(
      `GPS LATITUDE (°)-24C102759`,
      .after = `waterPressure_barA`
    ) |>
    dplyr::rename(
      `latitude` = `GPS LATITUDE (°)-24C102759`
    ) |>
    dplyr::relocate(
      `GPS LONGITUDE (°)-24C102759`,
      .after = `latitude`
    ) |>
    dplyr::rename(
      `longitude` = `GPS LONGITUDE (°)-24C102759`
    ) |>
    dplyr::relocate(
      `ALTITUDE (M)-24C102759`,
      .after = `longitude`
    ) |>
    dplyr::rename(
      `altitude_m` = `ALTITUDE (M)-24C102759`
    ) |>
    dplyr::relocate(
      `BAROMETER (MMHG)-24C102759`,
      .after = `altitude_m`
    ) |>
    dplyr::rename(
      `barometerAirHandheld_mbarss` = `BAROMETER (MMHG)-24C102759`
    )

    converted_kor_csv_file <- converted_kor_csv_file <- dplyr::select(
      -(grep("barometerAirHandheld_mbarss", colnames(converted_kor_csv_file)):nrow(converted_kor_csv_file))
    )

  return(converted_kor_csv_file)
}

?rename

View(reformat_kor_csv_file("Kor Measurement File Export - 080624 141005.csv"))

?dplyr::relocate
?ifelse
?stop
