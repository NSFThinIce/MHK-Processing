##Removing incorrect rows from YSI data##
##created on 06Jan2026
##Authors Hannah Cane 

#Libraries####
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(patchwork)){install.packages("patchwork")}
#Load packages####
library(tidyverse) #for dplyr and ggplot
library(patchwork)
library(gridExtra)

##Take each year separately
##read in files as a list
##read in csv that contains which rows to remove for each date 
##removes those rows and saves new csv

#Set year####
yearIndex<-"2024"

YSI_profiles <- list.files(path = paste0("01_Data/MHK_Data/EXO1Sonde/Profile_correct_format/",yearIndex,"/"), pattern = "*.csv", full.names = TRUE)
lines_remove <- read_csv("01_Data/MHK_Data/EXO1Sonde/EvaluationFiles/MohonkYSI_ProfileEvaluation_2024.csv")


# function to extract date 
extract_date <- function(x){
  substr(basename(x), 5, 14)  # assuming date is in positions 5 to 12 in filename
}




ysi_df <- tibble(file = YSI_profiles,
                 date = extract_date(YSI_profiles),
                 type = "YSI")


#Debug date.index<-2
for(date.index in 1:length(ysi_df)){
  
  # files for that date
  todays_files <- ysi_df %>% filter(date == ysi_df[date.index,"date"] %>% pull())
  
  # Format the date to be file name friendly
  date <- todays_files$date[1]
  date_as_string <- sub("(\\d+)/(\\d+)/(\\d{4})$", "\\3/\\1/\\2", as.character(date))
  
  ## If there are no leading zeros for the month or day, then append a 0 to the string
  date_split <- strsplit(date_as_string, "/")
  
  year <- substr(date_split,1,4)
  month <- substr(date_split,6,7)
  day <- substr(date_split,9,10)
  
  if (nchar(day) < 2) day <- paste("0", day, sep = "")
  if (nchar(month) < 2) month <- paste("0", month, sep = "")
  
  # Converts the date to a string
  date_as_string <- paste(year, month, day, sep = "-")
  
  
  
  remove_df <- lines_remove %>%
                filter(Date == date_as_string)
  

 temp.YSI.cleaned <- todays_files %>%
    filter(date == date) %>%     # select row based on date
    pull(file) %>%                        # extract file path
    read_csv(show_col_types = FALSE) %>%
    mutate(date = mdy(Date))

 #debug remove.index<-1
 for(remove.index in 1:10){
   remove_df_begin<-remove_df[1,paste0("BeginRow",remove.index)]%>%pull()
   remove_df_end<-remove_df[1,paste0("EndRow",remove.index)]%>%pull()
   
   
   
      #Checks if the beginning or ending rows are NA and do nothing   
   if(is.na(remove_df_begin)|is.na(remove_df_end)){}else{
     
     #Extract the depths for that row
     depth_begin<-temp.YSI$Depth_m[remove_df_begin]
     depth_end<-temp.YSI$Depth_m[remove_df_end]
     
     #Filter out based on the depths
     temp.YSI.cleaned<-temp.YSI.cleaned%>%filter(Depth_m<depth_begin|Depth_m>depth_end)

     
   } #end of checking NA   
     
 } #End of the for loop

 ##creating a new depth vector 
 depth_vector<-seq(
   from = 0,
   by   = 0.25,
   length.out = nrow(temp.YSI.cleaned)
 ) %>% rev()
 
 ##adding new depths as a column 
 temp.YSI.cleaned <- temp.YSI.cleaned %>%  mutate(  
   Depth_m = depth_vector)
 
# write CSV 
  newfilename<-paste(str_extract(substr(todays_files$file,1,nchar(todays_files$file)-4), "[^/]+$"),"_clean.csv",sep="")
  
  
  write_csv(x=temp.YSI.cleaned,file=paste0("01_Data/MHK_Data/Exo1Sonde/Cleaned_data/",newfilename))
}



