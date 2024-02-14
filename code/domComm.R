library(readxl)
library(tidyverse)
library(dplyr)
library(viridisLite)


### Original code chunk. Superseded below ###

vegdata <- read_xlsx("data/VegData.xlsx", sheet = "Data")
  
  
DominantCommunity <- vegdata %>% 
  select(Month:Percent_Bare_Ground) %>% 
  mutate(Date = paste0(Month, "_", Year)) %>% 
  pivot_longer(cols = c("Percent_Spartina", "Percent_Pickleweed", "Percent_Upland", "Percent_Bare_Ground"),
               names_to = "Community") %>%
  nest(data = c("Community", "value")) %>%
  mutate(domComm = map(.x = data, ~slice_max(.x, order_by = value, n = 1)),
         .keep = "unused") %>%
  unnest(domComm) %>% 
  mutate(Community = str_remove(Community, "Percent_"))

# nest-mutate-map: use when you want to summarize on groups of rows *watch video: https://www.youtube.com/watch?v=nXQDiCTLTgU&t=1395s*






###  9/19/2023 Starting here I am using the raw veg data without any calculations. This code chunk DOES NOT calculate veg cover relative to bare ground. 
# Code is superseded below.   


vegdata <- read_xlsx("data/VegData_raw.xlsx", sheet = "Data")



DominantCommunity <- vegdata %>% 
  mutate_at(c('MeIn', 'DiGr', 'RaSa', 'CaMa', 'SaSo', 'Lolium', 'FrSa', 'Fabiaceae','Vicia', 'Sonc', 'Xant', 'LeTr', 'Coytote_Bush','BoMa'), as.numeric) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  mutate(Total_Veg_Quad = select(.,SpFo:SpMa, MeIn:Lolium, GrSt:LeTr) %>% rowSums(na.rm = TRUE)) %>% 
  mutate(Bare_Ground = ((BaGr_1 + BaGr_2)/25)*100) %>% 
  mutate(Spartina = ((SpFo/Total_Veg_Quad))*100) %>% 
  mutate(Pickleweed = ((select(.,SaPa:BoMa, SaSo, PicklePups) %>% rowSums(na.rm = TRUE))/Total_Veg_Quad)*100) %>% 
  mutate(Transition = ((select(.,AtPx:SpMa,MeIn:CaMa,Lolium,GrSt:MePo, Vicia:LeTr) %>% rowSums(na.rm = TRUE))/Total_Veg_Quad)*100) %>%
  mutate(across(where(is.numeric), ~round(., 0))) %>% 
  mutate(Date = paste0(Year, "-", Month)) %>% 
  pivot_longer(cols = c("Spartina", "Pickleweed", "Transition", "Bare_Ground"),
               names_to = "Community") %>%
  nest(data = c("Community", "value")) %>%
  mutate(DominantCommunity = map(.x = data, ~slice_max(.x, order_by = value, n = 1)),
         .keep = "unused") %>%
  unnest(DominantCommunity) 

#To list in order of date:

DominantCommunity$Date <- factor(DominantCommunity$Date, levels = c("2022-June", "2022-July", "2022-August", "2022-September", "2022-October", "2023-February","2023-April", "2023-June", "2023-August"))







### 9/26/2023 New code chunk that calculates absolute veg cover relative to bare ground (i.e. what percent of the vegetation present in each quadrat that each community makes up)

vegdata <- read_xlsx("data/VegData_raw.xlsx", sheet = "Data")

DominantCommunityAbsolute <- vegdata %>% 
  mutate_at(c('MeIn', 'DiGr', 'RaSa', 'CaMa', 'SaSo', 'Lolium', 'FrSa', 'Fabiaceae','Vicia', 'Sonc', 'Xant', 'LeTr', 'Coytote_Bush','BoMa'), as.numeric) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  mutate(Total_Veg = 25-(BaGr_1 + BaGr_2)) %>% 
  mutate(Bare_Ground = ((BaGr_1 + BaGr_2)/25)*100) %>% 
  mutate(Total_Veg_Quad = select(.,SpFo:SpMa, MeIn:Lolium, GrSt:LeTr) %>% rowSums(na.rm = TRUE)) %>%
  mutate(Spartina = ((SpFo/Total_Veg))*100) %>% 
  mutate(Pickleweed = ((select(.,SaPa:BoMa, SaSo, PicklePups) %>% rowSums(na.rm = TRUE))/Total_Veg)*100) %>% 
  mutate(Transition = ((select(.,AtPx:SpMa,MeIn:CaMa,Lolium,GrSt:MePo, Vicia:LeTr) %>% rowSums(na.rm = TRUE))/Total_Veg)*100) %>% 
  mutate(across(where(is.numeric), ~round(., 0))) %>% 
  mutate(Date = paste0(Year, "-", Month)) %>% 
  mutate(Bare_Ground = replace(Bare_Ground, Bare_Ground != 100, 0)) %>%
  pivot_longer(cols = c("Spartina", "Pickleweed", "Transition", "Bare_Ground"),
               names_to = "Community") %>%
  nest(data = c("Community", "value")) %>%
  mutate(DominantCommunityAbsolute = map(.x = data, ~slice_max(.x, order_by = value, n = 1)),
         .keep = "unused") %>%
  unnest(DominantCommunityAbsolute) 

#And finally, to list in order of date:

DominantCommunityAbsolute$Date <- factor(DominantCommunityAbsolute$Date, levels = c("2022-June", "2022-July", "2022-August", "2022-September", "2022-October", "2023-February","2023-April", "2023-June", "2023-August"))







### 9/28/2023  This way calculates the veg percent cover simply by how many quadrants each species takes up in the quadrat.

vegdata <- read_xlsx("data/VegData_raw.xlsx", sheet = "Data")

DominantCommunity25 <- vegdata %>% 
  mutate_at(c('MeIn', 'DiGr', 'RaSa', 'CaMa', 'SaSo', 'Lolium', 'FrSa', 'Fabiaceae','Vicia', 'Sonc', 'Xant', 'LeTr', 'Coytote_Bush','BoMa'), as.numeric) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  mutate(Total_Veg_Quad = select(.,SpFo:SpMa, MeIn:Lolium, GrSt:LeTr) %>% rowSums(na.rm = TRUE)) %>% 
  mutate(Bare_Ground = ((BaGr_1 + BaGr_2)/25)*100) %>% 
  mutate(Spartina = ((SpFo/25))*100) %>% 
  mutate(Pickleweed = ((select(.,SaPa:BoMa, SaSo, PicklePups) %>% rowSums(na.rm = TRUE))/25)*100) %>% 
  mutate(Transition = ((select(.,AtPx:SpMa,MeIn:CaMa,Lolium,GrSt:MePo, Vicia:LeTr) %>% rowSums(na.rm = TRUE))/25)*100) %>%
  mutate(across(where(is.numeric), ~round(., 0))) %>% 
  mutate(Date = paste0(Year, "-", Month)) %>% 
  mutate(Bare_Ground = replace(Bare_Ground, Bare_Ground != 100, 0)) %>%
  pivot_longer(cols = c("Spartina", "Pickleweed", "Transition", "Bare_Ground"),
               names_to = "Community") %>%
  nest(data = c("Community", "value")) %>%
  mutate(DominantCommunity = map(.x = data, ~slice_max(.x, order_by = value, n = 1)),
         .keep = "unused") %>%
  unnest(DominantCommunity) 

#To list in order of date:

DominantCommunity25$Date <- factor(DominantCommunity25$Date, levels = c("2022-June", "2022-July", "2022-August", "2022-September", "2022-October", "2023-February","2023-April", "2023-June", "2023-August"))









#### 9/29/2023  This code chunk attempts to "bin" the values into ranges from 0-10, 10-80, and anything greater than 80. Additionally, drop_na() was used to remove these random rows with na values across them.  Not sure what to make of that yet.

setwd("~/GitHub/BeccaTransects/data")
vegdata <- read_xlsx("~/GitHub/BeccaTransects/data/VegData_raw.xlsx", sheet = "Data")

DominantCommunity25 <- vegdata %>% 
  mutate_at(c('MeIn', 'DiGr', 'RaSa', 'CaMa', 'SaSo', 'Lolium', 'FrSa', 'Fabiaceae','Ranunculus','Taraxacum', 'Vicia', 'Sonc', 'Xant', 'LeTr', 'Coytote_Bush','BoMa'), as.numeric) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  mutate(Total_Veg_Quad = select(.,SpFo:SpMa, MeIn:Lolium, GrSt:LeTr) %>% rowSums(na.rm = TRUE)) %>% 
  mutate(Bare_Ground = ((BaGr_1 + BaGr_2)/25)*100) %>% 
  mutate(Spartina = ((SpFo/25))*100) %>% 
  mutate(Pickleweed = ((select(.,SaPa:BoMa, SaSo, PicklePups) %>% rowSums(na.rm = TRUE))/25)*100) %>% 
  mutate(Transition = ((select(.,AtPx:SpMa,MeIn:CaMa,Lolium,GrSt:MePo, Vicia:LeTr) %>% rowSums())/25)*100) %>%
  mutate(across(where(is.numeric), ~round(., 0))) %>% 
  mutate(Date = paste0(Year, "-", Month)) %>% 
  mutate(Bare_Ground = replace(Bare_Ground, Bare_Ground != 100, 0)) %>%
  pivot_longer(cols = c("Spartina", "Pickleweed", "Transition", "Bare_Ground"),
               names_to = "Community") %>%
  nest(data = c("Community", "value")) %>%
  mutate(DominantCommunity = map(.x = data, ~slice_max(.x, order_by = value, n = 1)),
         .keep = "unused") %>%
  unnest(DominantCommunity) %>% 
  mutate(Percent_Cover_Range = cut(value, breaks=c(0, 10, 80, 700), labels = c("<10%", "10-80% Colonizing", ">80% Dense"))) %>% 
  mutate(Date = factor(Date, levels = c("2023-October", "2023-August", "2023-June", "2023-April", "2023-February", "2022-October", "2022-September", "2022-August", "2022-July", "2022-June"))) %>% #so that ggplot will list in order of date
  drop_na() 
  


  
  
  








### PLOTTING ###



#Now I will attempt lollipop plots:

#General Notes: Need to figure out how to tell it to ID 50/50 dominance bw plant communities !!!!!!



#DON'T NEED THE CODE BETWEEN HERE:
ggplot(subset(DominantCommunityAbsolute, Transect_ID %in% "C7-L211"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, alpha= value, color= Community), shape= 15, size= 10) +
  geom_text(aes(label= value)) +
  labs(
    title = "Transect ID:   C1-L5") +
  xlab("Distance Along Transect") +
  ylab("Date of Survey")
#Plot notes: Light patch (4) SpFo detected in April 2023, 0 SpFo counted in June 2023, but 25 SpFo at q-17 in August 2023 Hmmmm


ggplot(subset(DominantCommunity, Transect_ID %in% "C7-L211"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, alpha= value, color= Community), shape= 15, size= 10) +
  geom_text(aes(label= value)) +
  labs(
    title = "Transect ID:   C1-L211") +
  xlab("Distance Along Transect") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L211"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, alpha= value, color= Community), shape= 15, size= 10) +
  geom_text(aes(label= value)) +
  labs(
    title = "Transect ID:   C1-L211") +
  xlab("Distance Along Transect") +
  ylab("Date of Survey")
#AND HERE!







#setting up a color-blind friendly palette
cbp <- c("#D55E00", "#009E73", "#56B4E9", "#F0E442" , "#E69F00", "#000000", "#0072B2", "#CC79A7")

names(cbp) <- levels(DominantCommunity25$Community)

col_scale <- scale_colour_manual(name = "Community", values = cbp,
                                 limits = force)

#WORKING OUT THE CODE FOR ANALYSIS:
#quadrats are all squares/opacity is percent cover range
ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW195"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, alpha = Percent_Cover_Range, color= Community), shape= 15, size=10) +
  geom_text(aes(label= value)) +
  col_scale +
  labs(
    title = "Transect ID:   C7-BW195") +
  xlab("Distance Along Transect") +
  ylab("Date of Survey") 

#quadrats are shaped based on percent cover
ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW195"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, shape= Percent_Cover_Range), size= 10) +
  geom_text(aes(label= value), size= 4) +
  col_scale +
  labs(
    title = "Transect ID:   C7-BW195") +
  xlab("Distance Along Transect") +
  ylab("Date of Survey") 

#qudrats are colore dbased on community type and sized based on percent cover
ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW195"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  scale_size_manual(values=c(2,5,10)) +
  col_scale +
  labs(
    title = "Transect ID:   C7-BW195") +
  xlab("Distance Along Transect") +
  ylab("Date of Survey") 





##Dominant Community per Plot Over Time:



#WEST:
ggplot(subset(DominantCommunity25, Transect_ID %in% "C1-L5"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C1-L5") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")


ggplot(subset(DominantCommunity25, Transect_ID %in% "C1-BW4"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C1-BW4") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")


ggplot(subset(DominantCommunity25, Transect_ID %in% "C1-BW6"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C1-BW6") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")


#since there is no bare ground in this transect I need to adjust col_scale so that the orange color will be skipped 
cbp3 <- c("#009E73", "#56B4E9", "#F0E442" , "#E69F00", "#000000", "#0072B2", "#CC79A7")

names(cbp3) <- levels(DominantCommunity25$Community)

col_scale2 <- scale_colour_manual(name = "Community", values = cbp3,
                                 limits = force)

ggplot(subset(DominantCommunity25, Transect_ID %in% "C1-BW12"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale2 +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C1-BW12") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")


ggplot(subset(DominantCommunity25, Transect_ID %in% "C1-L11"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale2 +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C1-L11") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C2-L22"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C2-L22") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C2-L27"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C2-L27") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C2-BW27"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C2-BW27") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")

ggplot(subset(DominantCommunity25, Transect_ID %in% "C2-BW31"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C2-BW31") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")


ggplot(subset(DominantCommunity25, Transect_ID %in% "C2-L32"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C2-L32") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")


ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-L42"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-L42") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-L44"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-L44") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-L47"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-L47") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")


#August 2022 17 m quadrat skipped for some reason :(
ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-L48"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-L48") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")





ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-L52"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-L52") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



#No quads for 18 m for months June-August 2022. Extended length of shoreline in Sept 2022.
ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-L55"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-L55") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



#No quads for 18 m for months June-August 2022. Extended length of shoreline in Sept 2022.
ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-L58"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-L58") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




#No quads for 18 m for months June-August 2022. Extended length of shoreline in Sept 2022.
ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-L61"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-L61") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")
       
       
       

#No quads for 18 m for months June-August 2022. Extended length of shoreline in Sept 2022.
ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-L65"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-L65") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")
       



#No quads for 17 m for months June-Sept 2022. Extended length of shoreline in Oct 2022.
ggplot(subset(DominantCommunity25, Transect_ID %in% "C4-L78"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C4-L78") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")





#No data for 14 m for June and July 2022.  Want to put in an NA but don't know how
ggplot(subset(DominantCommunity25, Transect_ID %in% "C2-BW34"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C2-BW34") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-BW43"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-BW43") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



#October 2022 19 m quadrat skipped for some reason :(
ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-BW45"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-BW45") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




#Feb 2023 19 m skipped for some reason :( Also, June 2022 19 m quad just didnt exist yet
ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-BW47"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-BW47") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")


#17 m quad didn't exist June-Aug 2022
ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-BW52"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-BW52") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-BW55"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-BW55") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-BW58"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-BW58") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-BW63"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-BW63") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C3-BW66"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C3-BW66") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C4-BW77"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1, 4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C4-BW77") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




#EAST:

ggplot(subset(DominantCommunity25, Transect_ID %in% "C6-L164"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C6-L164") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C6-BW164"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C6-BW164") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C6-BW166"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C6-BW166") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



#**August 2023 quadrats are off bc I sampled the wrong distances :(
#no Spartina in this transect so need to manually adjust col_scale again:
cbp4 <- c("#D55E00", "#009E73", "#F0E442" , "#E69F00", "#000000", "#0072B2", "#CC79A7")

names(cbp4) <- levels(DominantCommunity25$Community)

col_scale3 <- scale_colour_manual(name = "Community", values = cbp4,
                                 limits = force)

ggplot(subset(DominantCommunity25, Transect_ID %in% "C6-BW168"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale3 +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C6-BW168") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




#there's  a lot going on with this transect. Used wrong lower distances for April 2023 and October 2023. October distances are weird bw 7-14 m but I hit 15-17 m correctly. Must have been racing the tides and couldn't go back?

ggplot(subset(DominantCommunity25, Transect_ID %in% "C6-L169"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", 
    size = "Percent Cover Range",
    title = "Transect ID:  C6-L169") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C6-L171"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C6-L171") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C6-BW171"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C6-BW171") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C6-BW174"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C6-BW174") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C6-L177"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C6-L177") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L188"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L188") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW188"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-BW188") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L190"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L190") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L196"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L196") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW195"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-BW195") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW198"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-BW198") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L200"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L200") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L207"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L207") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW202"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-BW202") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW204"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-BW204") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW207"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-BW207") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")





ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L207"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L207") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L211"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L211") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L213"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L213") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L217"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L217") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW217"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-BW217") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")



ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-BW220"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-BW220") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")


#October 2022 17m quadrat was skipped for some reason :(
ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L220"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L220") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")




ggplot(subset(DominantCommunity25, Transect_ID %in% "C7-L222"), aes(x= Distance_meters, y= Date)) +
  geom_point(aes(x= Distance_meters, color= Community, size= Percent_Cover_Range), shape= 15) +
  guides(color = guide_legend(override.aes = list(size = 10))) + #to manually adjust font size of legend labels
  scale_size_manual(values=c(1,4,10)) +
  scale_x_continuous(breaks = unique(DominantCommunity25$Distance_meters)) +
  theme(panel.grid.minor = element_blank()) +
  #would like to enter a horizontal line between the two growing seasons.  Need to use the after_scale mapping to nudge line up or down.
  #geom_hline(yintercept= "2022-October") +
  theme(axis.title = element_text(face = "bold", size=16),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 20)) +
  col_scale +
  labs(
    #not working: color = "Dominant Community", #used guides() above instead
    size = "Percent Cover Range",
    title = "Transect ID:  C7-L222") +
  xlab("Distance Along Transect (m)") +
  ylab("Date of Survey")
