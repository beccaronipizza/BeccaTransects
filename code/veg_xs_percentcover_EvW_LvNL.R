#### Calculating Mean Change East v West and Log v No Log ####
library(readxl)
library(tidyverse)
library(dplyr)
library(viridisLite)


setwd("~/GitHub/BeccaTransects/data")

vegmetadata <- read_xlsx("~/GitHub/BeccaTransects/data/VegData_raw.xlsx", sheet = "Metadata")
vegdata <- read_xlsx("~/GitHub/BeccaTransects/data/VegData_raw.xlsx", sheet = "Data_2_new")


# 1. creating the main data frame:

VegDataNew <- vegdata %>% 
  mutate_at(c('MeIn', 'DiGr', 'RaSa', 'CaMa', 'SaSo', 'Lolium', 'FrSa', 'Fabiaceae','Vicia', 'Sonc', 'Xant', 'LeTr', 'Ranunculus', 
              'Taraxacum', 'Coytote_Bush','BoMa'), as.numeric) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>%
  select(-BaGr_Type_1, -BaGr_Type_2, -Coytote_Bush, -Wrack, -Tot_Num_Species, -Shoot_Density, -Length_centimeters, -Patch_Type) %>% 
  mutate(BaGr = rowSums(across(BaGr_1:BaGr_2), na.rm = T)) %>% 
  select(-BaGr_1, -BaGr_2) %>% 
  mutate(Total_Veg = 25-BaGr) %>%  #Total_veg is simply what is remaining after subtracting the bare ground
  mutate(across(SpFo:Total_Veg, ~ .x*4)) %>% 
  mutate(Date = paste0(Year, "-", Month))



# ALL Veg Transects Over Time:

#A1. creating a new data frame before making a summary table

VegDataSumm <- VegDataNew %>% 
  select(Date, Zone, SpFo:Total_Veg) %>%   #selecting the columns you know you want
  pivot_longer(cols = SpFo:Total_Veg,
               names_to = "Species_code",
               values_to = "Percent_cover") %>% 
  group_by(Date, Zone, Species_code) %>% 
  summarize(mean_cover = mean(Percent_cover, na.rm = TRUE),
            sd_cover = sd(Percent_cover, na.rm = TRUE),
            n_cover = n()) %>% 
  mutate(se_cover = sd_cover/sqrt(n_cover)) %>%
  mutate(Date = factor(Date,levels = c("2022-June", "2022-July", "2022-August", "2022-September", "2022-October", "2023-February","2023-April", "2023-June", "2023-August", "2023-October")),
         Zone = factor(Zone, levels = c("U", "M", "L"))) %>%  
  filter(Date != "2023-October")




### A1. PLOTTING  ####



cbp <- c("#D55E00", "#009E73", "#0072B2", "#56B4E9", "#F0E442" , "#E69F00", "#000000", "#CC79A7")


#Percent Vegetation Cover Over Time By Marsh Zone


AllVegTransectsplot <- ggplot(data = VegDataSumm[VegDataSumm$Species_code == "Total_Veg",],   
                mapping = aes(x = Date,
                              y = mean_cover,
                              group = Zone,
                              color = Zone)) +
                              #shape = Zone)) +
  geom_line(size = 0.8, position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = mean_cover - se_cover,
                    ymax = mean_cover + se_cover),
                size = 0.8, height = 0.2, width = 0.4, color = "black", position = position_dodge(0.3)) +
  geom_point(aes(shape = Zone, size = Zone), position = position_dodge(0.3)) + 
  #geom_vline(xintercept= "2022-October",linetype =2, color = "red") +
  labs( 
    title = "Percent Vegetation Cover Over Time By Marsh Zone",
    subtitle = "Represents all transects over the course of two growing seasons (June 2022 to October 2023)",
    x = "Date of Survey",
    y ="Total Vegetated Cover (%)",
    caption = "*Error bars reflect standard error",
    #fill = "Shoreline End",
    tag = "veg_xs_percentcover_EvW_LvNL.R-AllVegTransectsplot") +
  scale_size_manual(values = c(5,5,5),
                    labels = c("Upper", "Middle", "Lower"),
                    name = "Marsh Zone") +
  scale_color_manual(values = cbp, 
                     labels = c("Upper", "Middle", "Lower"),
                     name = "Marsh Zone") +
  scale_shape_manual(values = c(15, 16, 17),
                     labels = c("Upper", "Middle", "Lower"),
                     name = "Marsh Zone") +
  theme_bw(base_size = 10) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  guides(color = guide_legend(title = "Marsh Zone")) +
  theme(axis.title = element_text(face = "bold", size=12),
        legend.title = element_text(face = "bold", size=09),
        legend.text=element_text(size=09),
        legend.position = "top",   #for no legend type: "none"
        legend.background = element_rect(fill="lightgrey",
                                         size=0.5, linetype="solid", 
                                         colour ="darkgrey"),
        plot.tag = element_text(size = 6, angle = 90),
        plot.tag.position = "right",
        axis.text.x = element_text(size = 09),
        axis.text.y = element_text(size = 09),
        plot.title = element_text(face = "bold", size = 11),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(size = 10),
        strip.text = element_text(size = 13, color = "black"))


AllVegTransectsplot  


##### DONT NEED THE NO OCTOBER DATA BELOW. FILTERED OUT OCTOBER ABOVE!!!


###All BUT October 2023 Veg Transects Over Time (Ok, so Kathy suggested I leave off the data from October 2023 just to avoid showing the big drop in veg in the upper zone due to dead veg = bare ground)


vegdata_NoOctober2023 <- read_xlsx("~/GitHub/BeccaTransects/data/VegData_raw.xlsx", sheet = "Data_NO_October2023")


# A2a. creating the main data frame:

VegDataNew_NoOctober2023 <- vegdata_NoOctober2023 %>% 
  mutate_at(c('RaSa', 'CaMa', 'SaSo', 'Lolium', 'DiSp', 'FrSa', 'Fabiaceae','Vicia', 'Sonc', 'Xant', 'LeTr', 'Ranunculus', 
              'Taraxacum', 'Coytote_Bush','BoMa'), as.numeric) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>%
  select(-BaGr_Type_1, -BaGr_Type_2, -Coytote_Bush, -Wrack, -Tot_Num_Species, -Shoot_Density, -Length_centimeters, -Patch_Type) %>% 
  mutate(BaGr = rowSums(across(BaGr_1:BaGr_2), na.rm = T)) %>% 
  select(-BaGr_1, -BaGr_2) %>% 
  mutate(Total_Veg = 25-BaGr) %>%  #Total_veg is simply what is remaining after subtracting the bare ground
  mutate(across(SpFo:Total_Veg, ~ .x*4)) %>% 
  mutate(Date = paste0(Year, "-", Month))



# A2b. creating a new data frame before making a summary table

VegDataSumm_NoOctober2023 <- VegDataNew_NoOctober2023 %>% 
  select(Date, Zone, SpFo:Total_Veg) %>%   #selecting the columns you know you want
  pivot_longer(cols = SpFo:Total_Veg,
               names_to = "Species_code",
               values_to = "Percent_cover") %>% 
  group_by(Date, Zone, Species_code) %>% 
  summarize(mean_cover = mean(Percent_cover, na.rm = TRUE),
            sd_cover = sd(Percent_cover, na.rm = TRUE),
            n_cover = n()) %>% 
  mutate(se_cover = sd_cover/sqrt(n_cover)) %>%
  mutate(Date = factor(Date,levels = c("2022-June", "2022-July", "2022-August", "2022-September", "2022-October", "2023-February","2023-April", "2023-June", "2023-August", "2023-October")),
         Zone = factor(Zone, levels = c("U", "M", "L"))) 




### A2. PLOTTING  ####



cbp <- c("#D55E00", "#009E73", "#56B4E9", "#F0E442" , "#E69F00", "#000000", "#0072B2", "#CC79A7")


#Percent Vegetation Cover Over Time By Marsh Zone


NoOctober2023_VegTransectsplot <- ggplot(data = VegDataSumm_NoOctober2023[VegDataSumm_NoOctober2023$Species_code == "Total_Veg",],   
                              mapping = aes(x = Date,
                                            y = mean_cover,
                                            group = Zone,
                                            color = Zone,
                                            shape = Zone)) +
  geom_line(size = 0.8, position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = mean_cover - se_cover,
                    ymax = mean_cover + se_cover),
                size = 0.8, width = 0.1, color = "black", position = position_dodge(0.3)) +
  geom_point(size = 3, position = position_dodge(0.3)) + 
  #geom_vline(xintercept= "2022-October",linetype =2, color = "red") +
  labs( 
    title = "Percent Vegetation Cover Over Time By Marsh Zone",
    subtitle = "Represents all transects over the course of two growing seasons (June 2022 to August 2023)",
    x = "Date of Survey",
    y ="Total Vegetated Cover (%)",
    caption = "*Error bars reflect standard error") +
  scale_color_manual(
    name = "Marsh Zone",
    values = c("#D55E00","#009E73", "#0072B2"), 
    breaks = c("U","M","L"),
    labels = c("Upper", "Middle", "Lower")) +
  theme_bw(base_size = 10) +
  guides(shape = FALSE) + #turns off legend for shape
  theme(axis.title = element_text(face = "bold", size=17),
        legend.title = element_text(face = "bold", size=17),
        legend.text=element_text(size=16),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(face = "bold", size = 18),
        plot.caption = element_text(size = 15),
        plot.subtitle = element_text(size = 16))


NoOctober2023_VegTransectsplot






#### B. Using Full_Join to combine main data sheet and metadata sheet ####

#B1a. First I want to clean up the metadata sheet:

vegmetadata %>% 
  select(Transect_ID, Shoreline_End, Log_Presence)

#B1b. Next I want my VegDataSumm df to include Transect_ID since this is what will be used to join the two df's:

VegDataSumm2 <- VegDataNew %>% 
  select(Year, Date, Zone, Transect_ID, SpFo:Total_Veg) %>%   #selecting the columns you know you want
  pivot_longer(cols = SpFo:Total_Veg,
               names_to = "Species_code",
               values_to = "Percent_cover")




#B1c. Now to merge the two data frames together:

VegDataMerged <- full_join(VegDataSumm2, vegmetadata, by = "Transect_ID")




## B1d. EAST v WEST: Now organize it to make the data frame needed to plot:

VegDataEW <- VegDataMerged %>% 
  select(Year, Date, Zone, Transect_ID, Species_code, Percent_cover, Shoreline_End) %>%   
  group_by(Year, Shoreline_End, Zone, Species_code) %>% 
  summarize(mean_cover = mean(Percent_cover, na.rm = TRUE),
            sd_cover = sd(Percent_cover, na.rm = TRUE),
            n_cover = n()) %>% 
  mutate(se_cover = sd_cover/sqrt(n_cover)) %>%
  mutate(Shoreline_End = factor(Shoreline_End, levels = c("west", "east")),
         Zone = factor(Zone, levels = c("U", "M", "L"))) 
  




# B1e. Now to plot:

EvWplot <- ggplot(data = VegDataEW[VegDataEW$Species_code == "Total_Veg",],   
                  mapping = aes(x = Zone,
                                y = mean_cover,
                                fill= Shoreline_End)) +
  geom_col(position = position_dodge(0.85),color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_cover - se_cover,
                    ymax = mean_cover + se_cover),
                    width = 0.1, color = "black", position = position_dodge(0.9)) +
  scale_x_discrete(labels = c(U = "Upper", M = "Middle", L = "Lower")) +  #labels renames the columns on the x-axis and limits sets the order in which each is laid   out on the x axis
  facet_wrap(Year ~.,) +
  coord_cartesian(ylim = c(40, 85))+ #sets the limit of the y-axis to show a more dramatic difference bw variables
  scale_fill_manual(values = cbp) +
  labs( 
    title = "Mean Percent Vegetation Cover On Each End of Shoreline per Marsh Zone",
    subtitle = "Represents all transects over the course of two growing seasons (June 2022 to October 2023)",
    x = "Marsh Zone",
    y ="Total Vegetated Cover (%)",
    fill = "End of Shoreline",
    caption = "*Error bars reflect standard error") +
  theme_bw(base_size = 10) +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(face = "bold", size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 18),
        plot.caption = element_text(size = 13),
        plot.subtitle = element_text(size = 13),
        strip.text = element_text(size = 18, color = "black"))


EvWplot 









######B2a. OK, so Kathy recommended I make the same plot but using ONLY June 2022 and 2023 or August 2022 and 2023 data for comparison

vegdata_August <- read_xlsx("~/GitHub/BeccaTransects/data/VegData_raw.xlsx", sheet = "Data_August_ONLY")



#B2b. Next, I want to clean up the metadata sheet:

vegmetadata %>% 
  select(Transect_ID, Shoreline_End, Log_Presence)


#B2c. Make the main data frame:

VegDataNew_August <- vegdata_August %>% 
  mutate_at(c('MeIn', 'DiGr', 'RaSa', 'CaMa', 'SaSo', 'Lolium', 'FrSa', 'DiSp', 'MixedSeedlings', 'Melilotus', 'MePo', 'PicklePups', 'Brassicacea', 'Fabiaceae','Vicia', 'Sonc', 'Xant', 'LeTr', 'Ranunculus', 'Taraxacum', 'Coytote_Bush','BoMa'), as.numeric) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>%
  select(-BaGr_Type_1, -BaGr_Type_2, -Coytote_Bush, -Wrack, -Tot_Num_Species, -Shoot_Density, -Length_centimeters, -Patch_Type) %>% 
  mutate(BaGr = rowSums(across(BaGr_1:BaGr_2), na.rm = T)) %>% 
  select(-BaGr_1, -BaGr_2) %>% 
  mutate(Total_Veg = 25-BaGr) %>%  #Total_veg is simply what is remaining after subtracting the bare ground
  mutate(across(SpFo:Total_Veg, ~ .x*4)) %>% 
  mutate(Date = paste0(Year, "-", Month))



#B2d. Then I want my VegDataSumm df to include Transect_ID since this is what will be used to join the two df's:

VegDataSumm_August <- VegDataNew_August %>% 
  select(Year, Date, Zone, Transect_ID, SpFo:Total_Veg) %>%   #selecting the columns you know you want
  pivot_longer(cols = SpFo:Total_Veg,
               names_to = "Species_code",
               values_to = "Percent_cover")

#B2e. Now to merge the two data frames together:
  
VegDataMerged_August <- full_join(VegDataSumm_August, vegmetadata, by = "Transect_ID")




## B2f. EAST v WEST: Now organize it to make the data frame needed to plot:

VegDataEW_August <- VegDataMerged_August %>% 
  select(Year, Date, Zone, Transect_ID, Species_code, Percent_cover, Shoreline_End) %>%   
  group_by(Year, Shoreline_End, Zone, Species_code) %>% 
  summarize(mean_cover = mean(Percent_cover, na.rm = TRUE),
            sd_cover = sd(Percent_cover, na.rm = TRUE),
            n_cover = n()) %>% 
  mutate(se_cover = sd_cover/sqrt(n_cover)) %>%
  mutate(Shoreline_End = factor(Shoreline_End, levels = c("west", "east")),
         Zone = factor(Zone, levels = c("U", "M", "L"))) %>% 
  drop_na() #for some reason, two rows would end up with NA values despite the previous data frames not having NA's.  Not sure what is going on. Used: which(is.na(data)) to find out which rows had NA's.


#B2g. PLOTTING EvW August ONLY:

EvWplot_August <- ggplot(data = VegDataEW_August[VegDataEW_August$Species_code == "Total_Veg",],   
                  mapping = aes(x = Zone,
                                y = mean_cover,
                                fill= Shoreline_End)) +
  geom_col(position = position_dodge(0.85),color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_cover - se_cover,
                    ymax = mean_cover + se_cover),
                width = 0.1, color = "black", position = position_dodge(0.9)) +
  scale_x_discrete(labels = c(U = "Upper", M = "Middle", L = "Lower")) +  #labels renames the columns on the x-axis and limits sets the order in which each is laid   out on the x axis
  facet_wrap(Year ~.,) +
  coord_cartesian(ylim = c(30, 100))+ #sets the limit of the y-axis to show a more dramatic difference bw variables
  scale_fill_manual(values = cbp) +
  labs( 
    title = "Mean Percent Vegetation Cover On Each End of Shoreline per Marsh Zone",
    subtitle = "Represents all transects over the course of two growing seasons (June 2022 to October 2023)",
    x = "Marsh Zone",
    y ="Total Vegetated Cover (%)",
    fill = "End of Shoreline",
    caption = "*Error bars reflect standard error") +
  theme_bw(base_size = 10) +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(face = "bold", size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 18),
        plot.caption = element_text(size = 13),
        plot.subtitle = element_text(size = 13),
        strip.text = element_text(size = 18, color = "black"))


EvWplot_August




######B3a. JUNE ONLY-- OK, so Kathy recommended I make the same plot but using ONLY June 2022 and 2023 or August 2022 and 2023 data for comparison

vegdata_June <- read_xlsx("~/GitHub/BeccaTransects/data/VegData_raw.xlsx", sheet = "Data_June_ONLY")



#B3b. Next, I want to clean up the metadata sheet:

vegmetadata %>% 
  select(Transect_ID, Shoreline_End, Log_Presence)


#B3c. Make the main data frame:

VegDataNew_June <- vegdata_June %>% 
  mutate_at(c('MeIn', 'DiGr', 'RaSa', 'CaMa', 'SaSo', 'Lolium', 'FrSa', 'DiSp', 'MixedSeedlings', 'Melilotus', 'MePo', 'PicklePups', 'Brassicacea', 'Fabiaceae','Vicia', 'Sonc', 'Xant', 'LeTr', 'Ranunculus', 'Taraxacum', 'Coytote_Bush','BoMa'), as.numeric) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>%
  select(-BaGr_Type_1, -BaGr_Type_2, -Coytote_Bush, -Wrack, -Tot_Num_Species, -Shoot_Density, -Length_centimeters, -Patch_Type) %>% 
  mutate(BaGr = rowSums(across(BaGr_1:BaGr_2), na.rm = T)) %>% 
  select(-BaGr_1, -BaGr_2) %>% 
  mutate(Total_Veg = 25-BaGr) %>%  #Total_veg is simply what is remaining after subtracting the bare ground
  mutate(across(SpFo:Total_Veg, ~ .x*4)) %>% 
  mutate(Date = paste0(Year, "-", Month))



#B3d. Then I want my VegDataSumm df to include Transect_ID since this is what will be used to join the two df's:

VegDataSumm_June <- VegDataNew_June %>% 
  select(Year, Date, Zone, Transect_ID, SpFo:Total_Veg) %>%   #selecting the columns you know you want
  pivot_longer(cols = SpFo:Total_Veg,
               names_to = "Species_code",
               values_to = "Percent_cover")

#B3e. Now to merge the two data frames together:

VegDataMerged_June <- full_join(VegDataSumm_June, vegmetadata, by = "Transect_ID")




## B3f. EAST v WEST: Now organize it to make the data frame needed to plot:

VegDataEW_June <- VegDataMerged_June %>% 
  select(Year, Date, Zone, Transect_ID, Species_code, Percent_cover, Shoreline_End) %>%   
  group_by(Year, Date, Shoreline_End, Zone, Species_code) %>% 
  summarize(mean_cover = mean(Percent_cover, na.rm = TRUE),
            sd_cover = sd(Percent_cover, na.rm = TRUE),
            n_cover = n()) %>% 
  mutate(se_cover = sd_cover/sqrt(n_cover)) %>%
  mutate(Shoreline_End = factor(Shoreline_End, levels = c("west", "east")),
         Zone = factor(Zone, levels = c("U", "M", "L"))) %>% 
  drop_na() #for some reason, two rows would end up with NA values despite the previous data frames not having NA's.  Not sure what is going on. Used: which(is.na(data)) to find out which rows had NA's.
  


#B3g. PLOTTING EvW JUNE ONLY:

EvWplot_June <- ggplot(data = VegDataEW_June[VegDataEW_June$Species_code == "Total_Veg",],   
                         mapping = aes(x = Date,
                                       y = mean_cover,
                                       fill = Shoreline_End
                                       )) +
  geom_col(position = position_dodge(0.85),color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_cover - se_cover,
                    ymax = mean_cover + se_cover),
                width = 0.1, color = "black", position = position_dodge(0.9)) +
  scale_x_discrete(labels = c(U = "Upper", M = "Middle", L = "Lower")) +  #labels renames the columns on the x-axis and limits sets the order in which each is laid   out on the x axis
  facet_wrap(Zone ~.,) + 
  coord_cartesian(ylim = c(30, 100))+ #sets the limit of the y-axis to show a more dramatic difference bw variables
  scale_fill_manual(values = cbp) +
  labs( 
    title = "Mean Percent Vegetation Cover On Each End of Shoreline per Marsh Zone",
    subtitle = "Data represents June transect surveys for the 2022 and 2023 growing seasons",
    x = "Marsh Zone",
    y ="Total Vegetated Cover (%)",
    fill = "End of Shoreline",
    caption = "*Error bars reflect standard error") +
  theme_bw(base_size = 10) +
  theme(axis.title = element_text(face = "bold", size=17),
        legend.title = element_text(face = "bold", size=17),
        legend.text=element_text(size=16),
        axis.text.x = element_text(size = 15, angle = 0),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(face = "bold", size = 18),
        plot.caption = element_text(size = 15),
        plot.subtitle = element_text(size = 16),
        strip.text = element_text(size = 18, color = "black"))


EvWplot_June









#C1a. LOG vs NO LOG:

VegDataLNL <- VegDataMerged %>% 
  select(Date, Zone, Shoreline_End, Transect_ID, Species_code, Percent_cover, Log_Presence) %>%   
  group_by(Log_Presence, Shoreline_End, Zone, Species_code) %>% 
  summarize(mean_cover = mean(Percent_cover, na.rm = TRUE),
            sd_cover = sd(Percent_cover, na.rm = TRUE),
            n_cover = n()) %>% 
  mutate(se_cover = sd_cover/sqrt(n_cover)) %>%
  mutate(Zone = factor(Zone, levels = c("U", "M", "L"))) 



#C1b. Plotting: 

cbp2 <- c("#0072B2", "#CC79A7", "#D55E00", "#009E73", "#56B4E9", "#F0E442" , "#E69F00", "#000000")


LvNLplot <- ggplot(data = VegDataLNL[VegDataLNL$Species_code == "Total_Veg",],   
                  mapping = aes(x = Zone,
                                y = mean_cover,
                                group = Log_Presence,
                                fill = Log_Presence)) +
  geom_col(position = position_dodge(0.85),color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_cover - se_cover,
                    ymax = mean_cover + se_cover),
                size = 0.5, width = 0.1, color = "black", position = position_dodge(0.9)) +
  scale_fill_manual(values = cbp2) +
  facet_grid(Shoreline_End ~.,) +
  scale_x_discrete(labels = c(U = "Upper", M = "Middle", L = "Lower")) +
  coord_cartesian(ylim = c(40, 85)) + #sets the limit of the y-axis to show a more dramatic difference bw variables
  labs( 
    title = "Mean Percent Vegetation Cover Based On Presence of Log per Marsh Zone",
    subtitle = "Represents all transects over the course of two growing seasons (June 2022 to October 2023)",
    x = "Marsh Zone",
    y ="Total Vegetated Cover (%)",
    fill = "Log Presence",
    caption = "*Error bars reflect standard error") +
  theme_bw(base_size = 10) +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(face = "bold", size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 18),
        plot.caption = element_text(size = 13),
        plot.subtitle = element_text(size = 13),
        strip.text = element_text(size = 18, color = "black"))


LvNLplot



#C2a. LOG vs NO LOG-- JUNE ONLY!:

VegDataLNL_June <- VegDataMerged_June %>% 
  select(Date, Zone, Shoreline_End, Transect_ID, Species_code, Percent_cover, Log_Presence) %>%   
  group_by(Date,Log_Presence, Shoreline_End, Zone, Species_code) %>% 
  summarize(mean_cover = mean(Percent_cover, na.rm = TRUE),
            sd_cover = sd(Percent_cover, na.rm = TRUE),
            n_cover = n()) %>% 
  mutate(se_cover = sd_cover/sqrt(n_cover)) %>%
  mutate(Zone = factor(Zone, levels = c("U", "M", "L")))%>% 
  drop_na() #for some reason, two rows would end up with NA values despite the previous data frames not having NA's.  Not sure what is going on. Used: which(is.na(data)) to find out which rows had NA's.



#C2b. Plotting: 

cbp2 <- c("#0072B2", "#CC79A7", "#D55E00", "#009E73", "#56B4E9", "#F0E442" , "#E69F00", "#000000")


LvNLplot_June <- ggplot(data = VegDataLNL_June[VegDataLNL_June$Species_code == "Total_Veg",],   
                   mapping = aes(x = Zone,
                                 y = mean_cover,
                                 group = Log_Presence,
                                 fill = Log_Presence)) +
  geom_col(position = position_dodge(0.85),color = "black", width = 0.85) +
  geom_errorbar(aes(ymin = mean_cover - se_cover,
                    ymax = mean_cover + se_cover),
                size = 0.5, width = 0.1, color = "black", position = position_dodge(0.9)) +
  scale_fill_manual(values = cbp2) +
  facet_grid(Shoreline_End ~ Date) +
  scale_x_discrete(labels = c(U = "Upper", M = "Middle", L = "Lower")) +
  coord_cartesian(ylim = c(30, 100)) + #sets the limit of the y-axis to show a more dramatic difference bw variables
  labs( 
    title = "Mean Percent Vegetation Cover On Each End of Shoreline per Marsh Zone",
    subtitle = "Data represents June transect surveys for the 2022 and 2023 growing seasons",
    x = "Marsh Zone",
    y ="Total Vegetated Cover (%)",
    fill = "Log Presence",
    caption = "*Error bars reflect standard error") +
  theme_bw(base_size = 10) +
  theme(axis.title = element_text(face = "bold", size=17),
        legend.title = element_text(face = "bold", size=17),
        legend.text=element_text(size=16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(face = "bold", size = 18),
        plot.caption = element_text(size = 15),
        plot.subtitle = element_text(size = 16),
        strip.text = element_text(size = 18, color = "black"))


LvNLplot_June