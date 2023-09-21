library(readxl)
library(tidyverse)
library(dplyr)

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






###  Starting here I am using the raw veg data without any calculations  9/19/2023   ###

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
  mutate(Date = paste0(Month, "_", Year)) %>% 
  pivot_longer(cols = c("Spartina", "Pickleweed", "Transition", "Bare_Ground"),
               names_to = "Community") %>%
  nest(data = c("Community", "value")) %>%
  mutate(DominantCommunity = map(.x = data, ~slice_max(.x, order_by = value, n = 1)),
         .keep = "unused") %>%
  unnest(DominantCommunity) 
  

#Now I will attempt lollipop plots:

ggplot(data = DominantCommunity,aes(x= Distance_meters, y= Transect_ID)) +
  geom_segment(aes(x=Distance_meters, xend=Distance_meters, y=Date, yend=Date), linewidth = 2)
 
  
  
