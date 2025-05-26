# This file stores all of the globals used by other scripts

# Path to the data directory if the working directory is the root of the repository
# - Root <- Working directory
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
KOR_UNFORMATTED_DATA_ALL <- file.path(KOR_UNFORMATTED_DATA_DIR, "AllExports.csv")

# Directory containing all of the formatted data from Kor Software and formatted with a script
KOR_FORMATTED_DATA_DIR <- file.path(MOHONK_DATA_DIR, "EXO1Sonde", "DesiredFormat")

# Directory containing all of the data from the DO sensor when collecting data from Mohonk
DO_SENSOR_DATA_DIR <- file.path(MOHONK_DATA_DIR, "DOSensor")