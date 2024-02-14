#Sediment Pin Plots
library(readxl)
library(tidyverse)
library(dplyr)
library(ggrepel)


#1. Run these:
setwd("~/GitHub/BeccaTransects/data")

sedpindata <- read_xlsx("Sediment_Pin_Data.xlsx", sheet = "All_Data")





#2. Run this:
#prepping the dataframe

sedpins <- sedpindata %>% 
  mutate(Date = paste0(Year, "-", Month)) %>% #make new column combining year and month into date
  mutate_at(c('Upper', 'Middle','Lower'), as.numeric) %>% #make Upper and Middle columns numeric (have NA's)
  select(-c("Notes")) %>% #remove "Notes" column
  mutate(across(where(is.numeric), ~round(., 1))) %>%  #round values to one decimal point
  group_by(Pin_ID, Shoreline_End) %>% 
  rename(Transect_ID = Pin_ID)
  #arrange(Date, .by_group = TRUE) %>% 
  

sedpinmetadata <- read_xlsx("~/GitHub/BeccaTransects/data/Sediment_Pin_Data.xlsx", sheet = "Metadata")

#Since sedpins df (above already has a "Shoreline_End" column we need to remove this column form the veg meta data sheet so that there aren't issues merging two df's with the same column names

sedpinmetadata <-select(sedpinmetadata, -c("Shoreline_End"))


SedPinDataMerged <- full_join(sedpins, sedpinmetadata, by = "Transect_ID")



## Plotting ##


    
      
 

 
 ### EvW plots ###
 #3 Run this: this way allows me to calc standard error
 

 sedpinchange <- sedpins %>%   
   mutate(UpperDiff = (last(Upper) - first(Upper))*-1) %>% 
   mutate(MiddleDiff = (last(Middle) - first(Middle))*-1) %>% 
   mutate(LowerDiff = (last(Lower) - first(Lower))*-1) %>% 
   #mutate(UpperDiff = ifelse(is.na(UpperDiff) & Pin_ID == "C1-BW6", 3.2, UpperDiff)) %>% 
   #mutate(UpperDiff = ifelse(is.na(UpperDiff) & Pin_ID == "C1-L5", 3.2, UpperDiff)) %>%
   select(-c('Month',`Month_#`, 'Year', 'Date', 'Upper', 'Middle', 'Lower')) %>% #remove all unnecessary columns
   distinct() %>%  #this removes repeated values for each month's calculation
   mutate(Shoreline_End = factor(Shoreline_End, levels = c("west", "east"))) %>%
   pivot_longer(UpperDiff:LowerDiff, names_to = "Pin_Location", values_to = "Sediment_Change") %>% 
   group_by(Shoreline_End, Pin_Location) %>% 
   summarize(mean_difference = mean(Sediment_Change, na.rm = TRUE),
             sd_difference = sd(Sediment_Change, na.rm = TRUE),
             n_difference = n()) %>% #n should be 18 for west UpperDiff
   mutate(se_difference = sd_difference/sqrt(n_difference)) 
 
   
 # PLOT with REFS:
 
#Se=Sd/sqrt(n)

 cbp <- c("#D55E00", "#009E73", "#56B4E9", "#F0E442" , "#E69F00", "#000000", "#0072B2", "#CC79A7")
 
 sedpinplot <- ggplot(sedpinchange, 
                      aes(x= Pin_Location, y= mean_difference, 
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
     title = "Manual Measurement: Average Change in Sediment Pin Length (April 2022 to October 2023)",
     subtitle = "Positive value = accretion   Negative value = erosion",
     y = "Mean Change (cm)",
     x = "Pin Location Relative to Transect Marsh Zone",
     fill= "End of Shoreline",
     caption = "**Includes reference pins  *Error bars reflect standard error") +
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

 sedpinplot   
   
 
 
   
### Now to do the same plot as above WITHOUT refs:
 
#1.
 
 sedpindatanorefs <- read_xlsx("Sediment_Pin_Data.xlsx", sheet = "All_Data_no_REFS_RTKstartmatch")

#2. 
 
 sedpins_norefs <- sedpindatanorefs %>% 
   mutate(Date = paste0(Year, "-", Month)) %>% #make new column combining year and month into date
   mutate_at(c('Upper', 'Middle','Lower'), as.numeric) %>% #make Upper and Middle columns numeric (have NA's)
   select(-c("Notes")) %>% #remove "Notes" column
   mutate(across(where(is.numeric), ~round(., 1))) %>%  #round values to one decimal point
   group_by(Pin_ID, Shoreline_End) %>% 
   rename(Transect_ID = Pin_ID)
#3. 
 
 sedpinchange_norefs <- sedpins_norefs %>%   
   mutate(UpperDiff = (last(Upper) - first(Upper))*-1) %>% 
   mutate(MiddleDiff = (last(Middle) - first(Middle))*-1) %>% 
   mutate(LowerDiff = (last(Lower) - first(Lower))*-1) %>% 
   #mutate(UpperDiff = ifelse(is.na(UpperDiff) & Pin_ID == "C1-BW6", 3.2, UpperDiff)) %>% 
   #mutate(UpperDiff = ifelse(is.na(UpperDiff) & Pin_ID == "C1-L5", 3.2, UpperDiff)) %>%
   select(-c('Month',`Month_#`, 'Year', 'Date', 'Upper', 'Middle', 'Lower')) %>% #remove all unnecessary columns
   distinct() %>%  #this removes repeated values for each month's calculation
   mutate(Shoreline_End = factor(Shoreline_End, levels = c("west", "east"))) %>%
   pivot_longer(UpperDiff:LowerDiff, names_to = "Pin_Location", values_to = "Sediment_Change") %>% 
   group_by(Shoreline_End, Pin_Location) %>% 
   summarize(mean_difference = mean(Sediment_Change, na.rm = TRUE),
             sd_difference = sd(Sediment_Change, na.rm = TRUE),
             n_difference = n()) %>% #n should be 18 for west UpperDiff
   mutate(se_difference = sd_difference/sqrt(n_difference)) 
 
#4. Plot
 
 sedpinplot_NOREFS <- ggplot(sedpinchange_norefs, aes(x= Pin_Location, y= mean_difference, 
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
     title = "Manual Measurement: Average Change in Sediment Pin Length (Jun/Jul 2022-Sep 2023)",
     subtitle = "Positive value = accretion   Negative value = erosion",
     y = "Mean Change (cm)",
     x = "Pin Location Relative to Transect Marsh Zone",
     fill= "End of Shoreline",
     caption = "   **Does not include reference pins  *Error bars reflect standard error") +
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
 
 sedpinplot_NOREFS
   
 
 
 
 
 
 
 ### Now to make it  Log v No Log (with REFS included):
 
 sedpinmetadata <- read_xlsx("~/GitHub/BeccaTransects/data/Sediment_Pin_Data.xlsx", sheet = "Metadata")
 
#Since sedpins df (above already has a "Shoreline_End" column we need to remove this column form the veg meta data sheet so that there aren't issues merging two df's with the same column names
 
 sedpinmetadata <-select(sedpinmetadata, -c("Shoreline_End"))
 
 
 SedPinDataMerged <- full_join(sedpins, sedpinmetadata, by = "Transect_ID")  



 sedpinchangeLvNL <- SedPinDataMerged %>%   
   select(Transect_ID, Shoreline_End, Upper, Middle, Lower, Date, Log_Presence) %>% 
   mutate(UpperDiff = (last(Upper) - first(Upper))*-1) %>% 
   mutate(MiddleDiff = (last(Middle) - first(Middle))*-1) %>% 
   mutate(LowerDiff = (last(Lower) - first(Lower))*-1) %>% 
   select(-c('Date', 'Upper', 'Middle', 'Lower')) %>% #remove all unnecessary columns
   distinct() %>%  #this removes repeated values for each month's calculation
   mutate(Shoreline_End = factor(Shoreline_End, levels = c("west", "east"))) %>% #this puts west first in the legend
   pivot_longer(UpperDiff:LowerDiff, names_to = "Pin_Location", values_to = "Sediment_Change") %>% 
   group_by(Shoreline_End, Pin_Location, Log_Presence) %>% 
   summarize(mean_difference = mean(Sediment_Change, na.rm = TRUE),
             sd_difference = sd(Sediment_Change, na.rm = TRUE),
             n_difference = n()) %>% 
   mutate(se_difference = sd_difference/sqrt(n_difference))
   
   

 
 
 
 ### PLOT LvNL with REFS ###
 #this includes reference pins!!! To make plot without reference pins see below
 
 sedpinLvNLplot <- ggplot(sedpinchangeLvNL, aes(x= Pin_Location, y= mean_difference, 
                                        group= Shoreline_End, 
                                        fill= Shoreline_End)) +
   geom_hline(yintercept=0, linetype =1, color = "black") +
   geom_col(position = position_dodge(0.87), color = "black", width = 0.85) +
   geom_errorbar(aes(
     ymin= mean_difference-se_difference, ymax= mean_difference+se_difference), size = 0.5, 
     position = position_dodge(0.9), color= "black", width= .1) +
   scale_x_discrete(labels = c(UpperDiff = "Upper", MiddleDiff = "Middle", LowerDiff = "Lower"), limits = c("UpperDiff", "MiddleDiff", "LowerDiff")) +  #labels renames the columns on the x-acis and limits sets the order in which each is laid out on the x axis
   facet_wrap(Log_Presence ~.,) +
   scale_fill_manual(values = cbp) +
   labs(
     title = "Average Change in Sediment Pin Length from April 2022 to October 2023",
     subtitle = "Positive value = accretion   Negative value = erosion",
     y = "Mean Change (cm)",
     x = "Pin Location Relative to Transect Marsh Zone",
     fill= "End of Shoreline",
     caption = "**Sediment pin measurements have a lot of variability and still need to be verified with RTK measurements         
  *Error bars reflect standard error") +
   theme_bw(base_size = 10) +
   theme(axis.title = element_text(face = "bold", size=16),
         legend.title = element_text(face = "bold", size=15),
         legend.text=element_text(size=15),
         axis.text.x = element_text(size = 15),
         axis.text.y = element_text(size = 13),
         plot.title = element_text(face = "bold", size = 18),
         plot.subtitle = element_text(size = 12),
         plot.caption = element_text(size = 13),
         panel.border = element_rect(color = "black", fill = NA, size = 0.5),
         strip.text = element_text(size = 18, color = "black"))                           
 
 sedpinLvNLplot



 
 
 
 
 
 ### Now to make it  Log v No Log (WITHOUT reference pins included) ###
 
 
 sedpindatanorefs <- read_xlsx("Sediment_Pin_Data.xlsx", sheet = "All_Data_no_REFS_RTKstartmatch")
 sedpinmetadatanorefs <- read_xlsx("~/GitHub/BeccaTransects/data/Sediment_Pin_Data.xlsx", sheet = "Metadata_no_REFS")
 
 
 #set up the sed pin data frame how you want it:
 
 sedpinsnorefs <- sedpindatanorefs %>% 
   mutate(Date = paste0(Year, "-", Month)) %>% #make new column combining year and month into date
   mutate_at(c('Upper', 'Middle','Lower'), as.numeric) %>% #make Upper and Middle columns numeric (have NA's)
   select(-c("Notes")) %>% #remove "Notes" column
   mutate(across(where(is.numeric), ~round(., 1))) %>%  #round values to one decimal point
   group_by(Pin_ID, Shoreline_End) %>% 
   rename(Transect_ID = Pin_ID)

 
 #Since sedpinsnorefs df (above already has a "Shoreline_End" column we need to remove this column form the veg meta data sheet so that there aren't issues merging two df's with the same column names
 
 sedpinmetadatanorefs <-select(sedpinmetadatanorefs, -c("Shoreline_End"))
 
 #join the data and metadata df's:
 SedPinDatanorefsMerged <- full_join(sedpinsnorefs, sedpinmetadatanorefs, by = "Transect_ID")  
 
 
 #now create the df that you'll plot:
 
 sedpinchangenorefsLvNL <- SedPinDatanorefsMerged %>% 
   select(Transect_ID, Shoreline_End, Upper, Middle, Lower, Date, Log_Presence) %>% 
   mutate(UpperDiff = (last(Upper) - first(Upper))*-1) %>% 
   mutate(MiddleDiff = (last(Middle) - first(Middle))*-1) %>% 
   mutate(LowerDiff = (last(Lower) - first(Lower))*-1) %>% 
   select(-c('Date', 'Upper', 'Middle', 'Lower')) %>% #remove all unnecessary columns
   distinct() %>%  #this removes repeated values for each month's calculation
   #mutate(Shoreline_End = factor(Shoreline_End, levels = c("west", "east"))) %>% #this puts west first in the legend
   pivot_longer(UpperDiff:LowerDiff, names_to = "Pin_Location", values_to = "Sediment_Change") %>% 
   group_by(Log_Presence, Shoreline_End, Pin_Location) %>% 
   summarize(mean_difference = mean(Sediment_Change, na.rm = TRUE),
             sd_difference = sd(Sediment_Change, na.rm = TRUE),
             n_difference = n()) %>% 
   mutate(se_difference = sd_difference/sqrt(n_difference)) %>% 
   mutate(across(where(is.numeric), ~round(., 2)))
 
 
 # PLOT: no REFS#
 
 cbp3 <- c("#E69F00", "#56B4E9", "#0072B2", "#CC79A7", "#D55E00", "#009E73", "#56B4E9", "#F0E442" , "#E69F00", "#000000")
 
 
 sedpinNOREFSLvNLplot <- ggplot(sedpinchangenorefsLvNL, aes(x= Pin_Location, y= mean_difference, 
                                                            group = Log_Presence,
                                                            fill= Log_Presence,
                                                            label= mean_difference)) +
   geom_hline(yintercept=0, linetype =1, color = "black") +
   geom_col(position = position_dodge(0.87), color = "black", width = 0.85) +
   geom_errorbar(aes(
     ymin= mean_difference-se_difference, ymax= mean_difference+se_difference), size = 0.5, 
     position = position_dodge(0.9), color= "black", width= .1) +
   scale_x_discrete(labels = c(UpperDiff = "Upper", MiddleDiff = "Middle", LowerDiff = "Lower"), limits = c("UpperDiff", "MiddleDiff", "LowerDiff")) +  #labels renames the columns on the x-acis and limits sets the order in which each is laid out on the x axis
   #geom_text_repel(aes(label = mean_difference, force = 1)) + #not sure why the "force" aes isnt working
   facet_grid(Shoreline_End ~.,) +
   scale_fill_manual(values = cbp3) +
   labs(
     title = "Manual Measurement: Average Change in Sediment Pin Length (Jun/Jul 2022 to Sep 2023)",
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
 
 sedpinNOREFSLvNLplot 


 
###Now I want to make a plot that shows mean change for L v NL for both manual and RTK data sets on one graph.Need to join the MERGED data sets for both 
 #and make a new column for the type of measurement (RTK or manual):
 
#manual merged data: sedpinchangenorefsLvNL from this code
#RTK merged data: RTKpinschangeLvNL from RTKsedimentpins.R code

 
#First, make new data frames for each data set with a new column that describes which measurement type (data set) it comes from:
 
 NEWsedpinchangenorefsLvNL <- sedpinchangenorefsLvNL %>% 
   mutate(Meas_Type = 'manual') 
 
 NEWRTKpinschangeLvNL <- RTKpinschangeLvNL %>% 
   mutate(Meas_Type = 'rtk')

 
 cbp4 <- c("#CC79A7", "#0072B2", "#E69F00", "#56B4E9", "#D55E00", "#009E73", "#56B4E9", "#F0E442" , "#E69F00", "#000000")
 
 
 
 testplot <- ggplot() +
   geom_col(data = NEWRTKpinschangeLvNL, 
            aes(x=Log_Presence, y= mean_difference, fill= Meas_Type),
            position = position_dodge(.85), #have tried making this one 0.1 and the one below 0.9, but no results
            width = .8) +
   #geom_errorbar(data = NEWRTKpinschangeLvNL,
                 #aes(ymin= mean_difference-se_difference, ymax= mean_difference+se_difference), size = 0.5, 
     #position = position_dodge(0.9), color= "black", width= .1) +
   geom_col(data = NEWsedpinchangenorefsLvNL, 
            aes(x=Log_Presence, y= mean_difference, fill= Meas_Type),
            position = position_dodge(.85),
            width = 0.5) +
   facet_wrap(Pin_Location ~.,) +
   #scale_x_discrete(data = NEWRTKpinschangeLvNL, 
                    #labels = c(UpperDiff = "Upper", MiddleDiff = "Middle", LowerDiff = "Lower"), limits = c("UpperDiff", "MiddleDiff", "LowerDiff")) +
   scale_fill_manual(values = cbp4) +
   labs(
     title = "Average Change in Sediment Pin Length (Jun/Jul 2022 to Sep 2023)",
     subtitle = "Manual vs RTK Measurements        *Positive value = accretion   Negative value = erosion",
     y = "Mean Change (cm)",
     x = "Log Presence",
     fill= "Log Presence",
     caption = "*Error bars reflect standard error") +
   theme_bw(base_size = 10) +
   theme(axis.title = element_text(face = "bold", size=17),
         legend.title = element_text(face = "bold", size=17),
         legend.text=element_text(size=16),
         axis.text.x = element_text(size = 14),
         axis.text.y = element_text(size = 14),
         plot.title = element_text(face = "bold", size = 18),
         plot.subtitle = element_text(size = 16),
         plot.caption = element_text(size = 15, hjust = 0.1),
         panel.border = element_rect(color = "black", fill = NA, size = 0.5),
         strip.text = element_text(size = 18, color = "black"))
 
 
 testplot
 
 

 
 
 
 
 
 
 
 
 #Now to combine the two df's: (found this here: https://stackoverflow.com/questions/70451925/merge-multiple-bar-plots-using-ggplot2)
 
 ALLSedPinDataMERGED <- list(NEWsedpinchangenorefsLvNL, NEWRTKpinschangeLvNL)
 
 ALLSedPinDataMERGED <- lapply(ALLSedPinDataMERGED, function(dat) {
   dat$type <- colnames(dat)[1]
   colnames(dat)[1] <- "variable"
   dat
 })
 
 ALLSedPinDataMERGED <- do.call(rbind, ALLSedPinDataMERGED)
 
 
 ggplot(ALLSedPinDataMERGED, aes(variable, percentage)) +
  geom_col() +
  facet_wrap(~ type, scales = "free_x"), aes(variable, percentage)) +
   geom_col() +
   facet_wrap(~ type, scales = "free_x")
 all <- do.call(rbind, all)