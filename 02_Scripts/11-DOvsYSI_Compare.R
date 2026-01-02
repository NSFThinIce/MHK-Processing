##Comparing YSI profiles to DO profiles##
##created on 19Dec2025
##Authors Hannah Cane 

#Libraries####
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(patchwork)){install.packages("patchwork")}
#Load packages####
library(tidyverse) #for dplyr and ggplot
library(patchwork)
library(gridExtra)

#Set year####
yearIndex<-"2025"

#List files####
DO_profiles <- list.files(path = paste0("01_Data/MHK_Data/DOSensor/DO_correct_format/",yearIndex,"/"), pattern = "*.csv", full.names = TRUE)
YSI_profiles <- list.files(path = paste0("01_Data/MHK_Data/EXO1Sonde/Profile_correct_format/",yearIndex,"/"), pattern = "*.csv", full.names = TRUE)

# function to extract date 
extract_date <- function(x){
  substr(basename(x), 5, 14)  # assuming date is in positions 5 to 12 in filename
}




ysi_df <- tibble(file = YSI_profiles,
                 date = extract_date(YSI_profiles),
                 type = "YSI")

do_df  <- tibble(file = DO_profiles,
                 date = extract_date(DO_profiles),
                 type = "DOprobe")

# merge lists
all_files <- inner_join(ysi_df, do_df,by="date")%>%
              rename(file_YSI=file.x,
                     file_Hach=file.y)%>%
              arrange(date)

#Get out the dates - they should be unique from the inner_join statement
unique_dates <- all_files$date

pdf(paste0("05_Outputs/YSI_DO_plots_",yearIndex,".pdf"), onefile = TRUE,width=8.5,height=15)

#Loop through all the dates for this year####
#debug day.index<-1
for(day.index in 1:length(unique_dates)){
  
  # files for that date
  todays_files <- all_files %>% filter(date == unique_dates[day.index])
  
  #Read in the Hach and YSI data####
  #*suppress the read in messages####
  temp.Hach<-read_csv(all_files$file_Hach[day.index], show_col_types = FALSE)%>%
              dplyr::select(Date,Depth_m,temp_degC,doConcentration_mgpL,doSaturation_percent)%>%
              rename_with(~ paste0(., "_Hach"), .cols = temp_degC:doSaturation_percent)
  temp.YSI<-read_csv(all_files$file_YSI[day.index], show_col_types = FALSE)%>%
              mutate(Date=mdy(Date))%>% #gotta get the date in the correct format
              dplyr::select(Date,Depth_m,temp_degC,doConcentration_mgpL,doSaturation_percent)%>%
              rename_with(~ paste0(., "_YSI"), .cols = temp_degC:doSaturation_percent)

  
  
  #Left_join with YSI
  merged_DF<-left_join(temp.YSI,temp.Hach)
  
  #Plot temperature comparison
  gg.temp_degC<-ggplot(data=merged_DF)+
    geom_path(aes(x=temp_degC_YSI,y=Depth_m),color="red")+
    geom_point(aes(x=temp_degC_YSI,y=Depth_m),color="red")+
    geom_path(aes(x=temp_degC_Hach,y=Depth_m),color="blue")+
    geom_point(aes(x=temp_degC_Hach,y=Depth_m),color="blue")+
    scale_y_reverse()+
    theme_bw()
  
  gg.doConcentration_mgpL<-ggplot(data=merged_DF)+
    geom_path(aes(x=doConcentration_mgpL_YSI,y=Depth_m),color="red")+
    geom_point(aes(x=doConcentration_mgpL_YSI,y=Depth_m),color="red",shape=1)+
    geom_path(aes(x=doConcentration_mgpL_Hach,y=Depth_m),color="blue")+
    geom_point(aes(x=doConcentration_mgpL_Hach,y=Depth_m),color="blue",shape=1)+
    scale_y_reverse()+
    theme_bw()
  
  
  gg.doSaturation_percent<-ggplot(data=merged_DF)+
    geom_path(aes(x=doSaturation_percent_YSI,y=Depth_m),color="red")+
    geom_point(aes(x=doSaturation_percent_YSI,y=Depth_m),color="red",shape=2)+
    geom_path(aes(x=doSaturation_percent_Hach,y=Depth_m),color="blue")+
    geom_point(aes(x=doSaturation_percent_Hach,y=Depth_m),color="blue",shape=2)+
    scale_y_reverse()+
    theme_bw()
  
  # tableGrob converts the data frame into a graphical table object (grob)
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.5)),
    colhead = list(fg_params=list(cex = 0.5)),
    rowhead = list(fg_params=list(cex = 0.5)))
  table_grob <- tableGrob(merged_DF%>%
                            dplyr::select(Date,Depth_m,temp_degC_YSI,temp_degC_Hach,doSaturation_percent_YSI,doSaturation_percent_Hach)%>%
                            mutate(temp_degC_YSI=round(temp_degC_YSI,1))%>%
                            rename(t_ysi=temp_degC_YSI,
                                   t_hach=temp_degC_Hach,
                                   doSat_ysi=doSaturation_percent_YSI,
                                   doSat_Hach=doSaturation_percent_Hach)
                            ,theme=mytheme)
  
  #Get the figures for the left hand side
  List<-list(gg.temp_degC,gg.doSaturation_percent,gg.doConcentration_mgpL)
  
  #Plot them using patchwork####
  gg.left_panel<-wrap_plots(List,ncol = 1,nrow = 3)
  
  List2<-list(gg.left_panel,table_grob)

  gg.both<-wrap_plots(List2,ncol=2,nrow=1)  
  print(gg.both)
}

dev.off()





