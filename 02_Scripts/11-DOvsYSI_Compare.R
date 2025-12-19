##Comparing YSI profiles to DO profiles##
##created on 19Dec2025
##Authors Hannah Cane 

#Libraries####
`if (!require(tidyverse)) {install.packages("tidyverse")}
if(!require(patchwork)){install.packages("patchwork")}
#Load packages####
library(tidyverse) #for dplyr and ggplot
library(patchwork)
library(gridExtra)

#Pseudo code 
## list both YSI and Do prob profiles 
## choose file by date 
## read in that file as a loop
## plot DO%, DO mg/L, and Temp vs depth for both YSI and DO probe on one graph for each date

#List files####
DO_profiles <- list.files(path = "01_Data/MHK_Data/DOSensor/DO_correct_format", pattern = "*.csv", full.names = TRUE)
YSI_profiles <- list.files(path = "01_Data/MHK_Data/EXO1Sonde/Profile_correct_format", pattern = "*.csv", full.names = TRUE)

yearIndex <- "2025"  # change as needed

# function to extract date 
extract_date <- function(x){
  substr(basename(x), 5, 14)  # assuming date is in positions 5 to 12 in filename
}


pdf(paste0("05_Outputs/YSI_DO_plots_",d,".pdf"), onefile = TRUE,width=8.5,height=11)


ysi_df <- tibble(file = YSI_profiles,
                 date = extract_date(YSI_profiles),
                 type = "YSI")

do_df  <- tibble(file = DO_profiles,
                 date = extract_date(DO_profiles),
                 type = "DOprobe")

# merge lists
all_files <- bind_rows(ysi_df, do_df)

unique_dates <- unique(all_files$date)


for(d in unique_dates){
  
  # files for that date
  todays_files <- all_files %>% filter(date == d)
  
  # read and label
  df <- map_df(1:nrow(todays_files), function(i){
    read_csv(todays_files$file[i]) %>% 
      mutate(Source = todays_files$type[i],
             Date = d)
  })
  
  # plot – all variables vs depth
  temp_degC <- ggplot(df) +
    geom_path(aes(x = temp_degC,       y = Depth_m, color = Source), linewidth = 1) +
    scale_y_reverse() +
    labs(
      title = paste("Depth Profiles:", d),
      y = "Depth (m)",
      color = "Profile"
    ) +
    theme_bw()
  
  Temptbl <- df %>%
    select(Source, Depth_m, temp_degC) %>% 
    tableGrob()
  
  doSaturation_percent <- ggplot(df) +
      geom_path(aes(x = doSaturation_percent, y = Depth_m, color = Source), linetype = 1) +
    scale_y_reverse() +
    labs(
      title = paste("Depth Profiles:", d),
      y = "Depth (m)",
      color = "Profile"
    ) +
    theme_bw()
  DOsattbl <- df %>%
    select(Source, Depth_m, doSaturation_percent) %>% 
    tableGrob()
  
  
  doConcentration_mgpL <- ggplot(df) +
    geom_path(aes(x = doConcentration_mgpL,     y = Depth_m, color = Source), linetype = 1) +
    scale_y_reverse() +
    labs(
      title = paste("Depth Profiles:", d),
      y = "Depth (m)",
      color = "Profile"
    ) +
    theme_bw()
  
  DOcontbl <- df %>%
    select(Source, Depth_m, doConcentration_mgpL) %>% 
    tableGrob()
  
  
  List<-list(temp_degC,Temptbl, doSaturation_percent,DOsattbl, doConcentration_mgpL, DOcontbl)
  
  #Plot them using patchwork####
  gg.4panel<-wrap_plots(List,ncol = 2,nrow = 3)
  print(gg.4panel)

}

dev.off()





