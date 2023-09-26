library(readxl)
library(tidyverse)
library(dplyr)



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






###  Starting here I am using the raw veg data without any calculations  9/19/2023. This code chunk does not calculate veg cover relative to bare ground. 
# Code is superseded below.   


vegdata <- read_xlsx("data/VegData_raw.xlsx", sheet = "Data")



DominantCommunity <- vegdata %>% 
  mutate_at(c('MeIn', 'DiGr', 'RaSa', 'CaMa', 'SaSo', 'Lolium', 'FrSa', 'Fabiaceae','Vicia', 'Sonc', 'Xant', 'LeTr', 'Coytote_Bush','BoMa'), as.numeric) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  mutate(Total_Veg_Quad = select(.,SpFo:SpMa, MeIn:Lolium, GrSt:LeTr) %>% rowSums(na.rm = TRUE)) %>% 
  mutate(Bare_Ground = ((BaGr_1 + BaGr_2)/25)*100) %>% 
  mutate(Spartina = ((SpFo/Total_Veg_Quad))*100) %>% 
  mutate(Pickleweed = ((select(.,SaPa:BoMa, SaSo, PicklePups) %>% rowSums(na.rm = TRUE))/Total_Veg_Quad)*100) %>% 
  mutate(Transition = ((select(.,AtPx:SpMa,MeIn:Lolium,GrSt:MePo, Vicia:LeTr) %>% rowSums(na.rm = TRUE))/Total_Veg_Quad)*100) %>% 
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







### New code chunk that calculates absolute veg cover relative to bare ground (i.e. what percent of the vegetation present in each quadrat each community makes up)

communities <- vegdata %>% 
  mutate_at(c('MeIn', 'DiGr', 'RaSa', 'CaMa', 'SaSo', 'Lolium', 'FrSa', 'Fabiaceae','Vicia', 'Sonc', 'Xant', 'LeTr', 'Coytote_Bush','BoMa'), as.numeric) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  mutate(Total_Veg = 25-(BaGr_1 + BaGr_2)) %>% 
  mutate(Bare_Ground = ((BaGr_1 + BaGr_2)/25)*100) %>% 
  mutate(Total_Veg_Quad = select(.,SpFo:SpMa, MeIn:Lolium, GrSt:LeTr) %>% rowSums(na.rm = TRUE)) %>%
  mutate(Spartina = ((SpFo/Total_Veg))*100) %>% 
  mutate(Pickleweed = ((select(.,SaPa:BoMa, SaSo, PicklePups) %>% rowSums(na.rm = TRUE))/Total_Veg)*100) %>% 
  mutate(Transition = ((select(.,AtPx:SpMa,MeIn:Lolium,GrSt:MePo, Vicia:LeTr) %>% rowSums(na.rm = TRUE))/Total_Veg)*100) %>% 
  mutate(across(where(is.numeric), ~round(., 0))) %>% 
  mutate(Date = paste0(Year, "-", Month)) %>% 
  select(Date, Transect_ID, Distance_meters, Spartina, Pickleweed, Transition, Bare_Ground)

#is this a problem? The values range from 4 - 900. I thought this was on a percent scale.
unique(communities$Bare_Ground)

#create a subset of the data with just the bare ground quadrats
bare_ground_quadrats <- communities %>%
  #used >= since there are some quadrats with greater than 100% -- not sure how to interpret this
  filter(Bare_Ground >= 100) %>%
  select(-c("Spartina", "Pickleweed", "Transition")) %>%
  pivot_longer(cols = "Bare_Ground", names_to = "Community")
  

#for quadrats that have veg, determine which community type is dominant
veg_quadrats <- communities %>%
  filter(Bare_Ground < 100) %>%
  select(-Bare_Ground) %>% 
  pivot_longer(cols = c("Spartina", "Pickleweed", "Transition"),
               names_to = "Community") %>%
  nest(data = c("Community", "value")) %>% 
  mutate(dom = map(.x = data, ~slice_max(.x, order_by = value, n = 1)),
         .keep = "unused") %>% 
  unnest(dom) 

#join the bare_ground quadrats back with the vegetated quadrats to get the complete data set back together for plotting
all_quadrats <- rbind(bare_ground_quadrats,
                      veg_quadrats,
                      by = c("Date", "Transect_ID", "Distance_meters")) %>%
  arrange(Transect_ID, Date)

#And finally, to list in order of date:

DominantCommunityAbsolute$Date <- factor(DominantCommunityAbsolute$Date, levels = c("2022-June", "2022-July", "2022-August", "2022-September", "2022-October", "2023-February","2023-April", "2023-June", "2023-August"))

DominantCommunityAbsolute %>%
  select(Date, Transect_ID, Distance_meters, Community, value) %>%
  filter(Community == 'Bare_Ground')







### PLOTTING ###



#Now I will attempt lollipop plots:

#General Notes: Need to figure out how to tell it to ID 50/50 dominance bw plant communities !!!!!!


ggplot(subset(DominantCommunityAbsolute, Transect_ID %in% "C1-L5"), aes(x= Distance_meters, y= Date)) +
  geom_line() +
  geom_point(aes(x= Distance_meters, alpha= value, color= Community), shape= 15, size= 10) +
  geom_text(aes(label= value)) +
  labs(
    title = "Transect ID:   C1-L5") +
  xlab("Distance Along Transect") +
  ylab("Date of Survey")
#Plot notes: Light patch (4) SpFo detected in April 2023, 0 SpFo counted in June 2023, but 25 SpFo at q-17 in August 2023 Hmmmm


