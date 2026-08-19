# MHK Processing
This respository is used to: store data collected from the lake at Mohonk Preserve, automate the processing of collected data, and produce graphs of collected data


Data Flow

Download data from YSI using KOR software following sensor user guide 
Download field data from lab computer

Open "09-SondeDataFormatter_Current" found in scripts to format data 
  **make sure to change the kor_export_file to your new files
Run "10-DODataFormatter_Current" to run field data 
  **make sure to have the correct year
Run "11-DOvsYSI_Compare" to compare the DO and YSI profiles 
Look at profiles found in outputs then update C:\git_hub\MHK-Processing\01_Data\MHK_Data\EXO1Sonde\EvaluationFiles\YEAR to remove rows 
Run "12-Removal_of_BAD_Data" to remove those rows you assigned 
Run "13-CORRECTED_DOvsYSI_Compare" to rerun profiles and double check the updated profiles in output and corrected 
After all these are run profiles can be uploaded to Thin Ice 
