#Sediment Pin Plots
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)


#1. Run these:
setwd("~/GitHub/BeccaTransects/data")

RTKpindata_norefs <- read_xlsx("RTKSedimentPinData.xlsx", sheet = "RTKData_thesispins_norefs_pivot")


#prepping the dataframe


##*remember that this df is still in ft NADV88 and isn't converted until the next step
RTKpins_norefs <- RTKpindata_norefs %>% 
  select(-c("midg","midt","upt","upg", "lowt", "lowg", "Date")) %>% #can remove these columns bc the top-ground calculations are already calculated in excel in their own columns
  mutate_at(c('upper', 'middle'), as.numeric) %>% #removed the POSITX (lubridate) version of date
  mutate(Date = paste0(Year, "-", Month)) %>% #created my own date to match the sediment pin code
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  group_by(Name, Shoreline_End) %>% 
  rename(Transect_ID = Name)
  
  


#Now to make a df for EvW:

RTKpinschange_norefs <- RTKpins_norefs %>%
  mutate(UpperDiff = ((last(upper) - first(upper))*-1)*30.48) %>%  # *30.48 converts NADV88 ft to cm
  mutate(MiddleDiff = ((last(middle) - first(middle))*-1)*30.48) %>% 
  mutate(LowerDiff = ((last(lower) - first(lower))*-1)*30.48) %>% 
  select(-c("Date", "Year", "Month", "upper", "middle", "lower")) %>% 
  distinct() %>%  #this removes repeated values for each month's calculation
  mutate(Shoreline_End = factor(Shoreline_End, levels = c("west", "east"))) %>%
  pivot_longer(UpperDiff:LowerDiff, names_to = "Pin_Location", values_to = "Sediment_Change") %>% 
  group_by(Shoreline_End, Pin_Location) %>% 
  summarize(mean_difference = mean(Sediment_Change, na.rm = TRUE),
            sd_difference = sd(Sediment_Change, na.rm = TRUE),
            n_difference = n()) %>% 
  mutate(se_difference = sd_difference/sqrt(n_difference))



 
#RTKpins <- RTKpindata %>% 
  #select(-c("midg","midt","upt","upg", "lowt", "lowg")) %>% 
  #mutate(across(where(is.numeric), ~round(., 2))) %>%  #round values to two decimal point
  #rename(Pin_ID = Name) %>% #change the "Name" column to "Pin_ID"
  #group_by(Pin_ID) %>%
  #arrange(Date) %>%
  #mutate(UpperDiff = upper - lag(upper, default = first(upper))) %>% 
  #mutate(MiddleDiff = middle - lag(middle, default = first(middle))) %>% 
  #mutate(LowerDiff = lower - lag(lower, default = first(lower))) 
  
#RTKpins 2 and 3 below for now...
  
  RTKpins2 <- RTKpins[-c(1:37), ] #the only way I could figure out how to remove the columns with 0's after the subtraction step above
  
  RTKpins3 <- RTKpins2 %>% 
    select(-c("upper","middle","lower")) %>% 
    mutate(Shoreline_End = factor(Shoreline_End, levels = c("west", "east"))) %>% #this puts west first in the legend
    pivot_longer(UpperDiff:LowerDiff, names_to = "Pin_Location", values_to = "Sediment_Change") %>% 
    group_by(Shoreline_End, Pin_Location) %>% 
    summarize(mean_difference = mean(Sediment_Change, na.rm = TRUE),
              sd_difference = sd(Sediment_Change, na.rm = TRUE),
              n_difference = n()) %>% 
    mutate(se_difference = sd_difference/sqrt(n_difference))
  
 
#Plotting:
  cbp <- c("#D55E00", "#009E73", "#56B4E9", "#F0E442" , "#E69F00", "#000000", "#0072B2", "#CC79A7")
  
#EvW Plot WITHOUT REFS


  RTKpinEvWplot_norefs <- ggplot(RTKpinschange_norefs, aes(x= Pin_Location, y= mean_difference, 
                                         group= Shoreline_End, 
                                         fill= Shoreline_End)) +
    geom_hline(yintercept=0, linetype =1, color = "black") +
    geom_col(position = position_dodge(0.87), color = "black", width = 0.85) +
    geom_errorbar(aes(
      ymin= mean_difference-se_difference, ymax= mean_difference+se_difference), size = 0.5, 
      position = position_dodge(0.9), color= "black", width= .1) +
    scale_x_discrete(labels = c(UpperDiff = "Upper", MiddleDiff = "Middle", LowerDiff = "Lower"), limits = c("UpperDiff", "MiddleDiff", "LowerDiff")) +  #labels renames the columns on the x-acis and limits sets the order in which each is laid out on the x axis
    scale_fill_manual(values = cbp) +
    labs(
      title = "RTK Measurement: Average Change in Sediment Pin Length (Jun 2022 to Sept 2023)",
      subtitle = "Positive value = accretion   Negative value = erosion",
      y = "Mean Change (cm)",
      x = "Pin Location Relative to Transect Marsh Zone",
      fill= "End of Shoreline",
      caption = "**Does not include reference pins  *Error bars reflect standard error") +
    theme_bw(base_size = 10) +
    theme(axis.title = element_text(face = "bold", size=17),
          legend.title = element_text(face = "bold", size=17),
          legend.text=element_text(size=16),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(face = "bold", size = 18),
          plot.subtitle = element_text(size = 16),
          plot.caption = element_text(size = 15, hjust = 0.1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text = element_text(size = 18, color = "black")) 
  
  RTKpinEvWplot_norefs 
  
  
  
  
  
  
  
  
### Now to make it  Log v No Log (WITHOUT reference pins included) ###
  

#Now set up df that groups  by log_presence
  
RTKpinsLvNL <- RTKpindata %>% 
    select(-c("midg","midt","upt","upg", "lowt", "lowg", "Date")) %>%
    mutate_at(c('upper', 'middle'), as.numeric) %>%#removed the POSITX (lubridate) version of date
    mutate(Date = paste0(Year, "-", Month)) %>% #created my own date to match the sediment pin code
    mutate(across(where(is.numeric), ~round(., 3))) %>%
    group_by(Name, Shoreline_End, Log_Presence) %>% 
    rename(Transect_ID = Name)
  

RTKpinschangeLvNL <- RTKpinsLvNL %>%
    mutate(UpperDiff = ((last(upper) - first(upper))*-1)*30.48) %>% 
    mutate(MiddleDiff = ((last(middle) - first(middle))*-1)*30.48) %>% 
    mutate(LowerDiff = ((last(lower) - first(lower))*-1)*30.48) %>% 
    select(-c("Date", "Year", "Month", "upper", "middle", "lower")) %>% 
    distinct() %>%  #this removes repeated values for each month's calculation
    mutate(Shoreline_End = factor(Shoreline_End, levels = c("east", "west"))) %>%
    pivot_longer(UpperDiff:LowerDiff, names_to = "Pin_Location", values_to = "Sediment_Change") %>% 
    group_by(Shoreline_End, Pin_Location, Log_Presence) %>% 
    summarize(mean_difference = mean(Sediment_Change, na.rm = TRUE),
              sd_difference = sd(Sediment_Change, na.rm = TRUE),
              n_difference = n()) %>% 
    mutate(se_difference = sd_difference/sqrt(n_difference))
  
  
  
  
  
  
  # PLOT: no REFS#
  
  cbp3 <- c("#E69F00", "#56B4E9", "#0072B2", "#CC79A7", "#D55E00", "#009E73", "#56B4E9", "#F0E442" , "#E69F00", "#000000")
  
  
  RTKpinsNOREFSLvNLplot <- ggplot(RTKpinschangeLvNL, aes(x= Pin_Location, y= mean_difference, 
                                                             group = Log_Presence,
                                                             fill= Log_Presence)) +
    geom_hline(yintercept=0, linetype =1, color = "black") +
    geom_col(position = position_dodge(0.87), color = "black", width = 0.85) +
    geom_errorbar(aes(
      ymin= mean_difference-se_difference, ymax= mean_difference+se_difference), size = 0.5, 
      position = position_dodge(0.9), color= "black", width= .1) +
    scale_x_discrete(labels = c(UpperDiff = "Upper", MiddleDiff = "Middle", LowerDiff = "Lower"), limits = c("UpperDiff", "MiddleDiff", "LowerDiff")) +  #labels renames the columns on the x-acis and limits sets the order in which each is laid out on the x axis
    facet_grid(Shoreline_End ~.,) +
    scale_fill_manual(values = cbp3) +
    labs(
      title = "RTK Measurement: Average Change in Sediment Pin Length (Jun 2022 to Sept 2023)",
      subtitle = "Positive value = accretion   Negative value = erosion",
      y = "Mean Change (cm)",
      x = "Pin Location Relative to Transect Marsh Zone",
      fill= "Log Presence",
      caption = "*Error bars reflect standard error") +
    theme_bw(base_size = 10) +
    theme(axis.title = element_text(face = "bold", size=17),
          legend.title = element_text(face = "bold", size=17),
          legend.text=element_text(size=16),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(face = "bold", size = 18),
          plot.subtitle = element_text(size = 16),
          plot.caption = element_text(size = 15, hjust = 0.1),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text = element_text(size = 18, color = "black"))                           
  
  RTKpinsNOREFSLvNLplot
  
  

  



