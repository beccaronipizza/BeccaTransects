library(readxl)
library(tidyverse)

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

#nest-mutate-map: use when you want to summarize on groups of rows *watch video*
#remember to assign a variable name like domComm at the beginning "name <- ..."