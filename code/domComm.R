library(readxl)
library(tidyverse)

domComm <- read_xlsx("data/VegData.xlsx", sheet = "Data") %>%  
  select(Month:Percent_Bare_Ground) %>% 
  pivot_longer(cols = Percent_Spartina:Percent_Bare_Ground,
               names_to = "Community") %>%
  nest(data = c("Community", "value")) %>%
  mutate(domComm = map(.x = data, ~slice_max(.x, order_by = value, n = 1)),
         .keep = "unused") %>%
  unnest(domComm)

domComm