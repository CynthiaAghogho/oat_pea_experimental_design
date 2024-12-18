
library(readr)
library(tidyverse)
library(readxl)



pea_oat_experiment_design <- read_csv("data/pea_oat_experiment_design.csv")


B4I_FINAL_pea_seed_list <- read_excel("data/B4I_FINAL_pea_seed_list.xlsx")


final_data_count1 <- read_csv("data/final_data_count1.csv")



pea_seed_available <- B4I_FINAL_pea_seed_list %>% 
  select(NAME,`SEED_AVAILABLE(g)`) %>% 
  mutate(pea_g_available = as.numeric(if_else(`SEED_AVAILABLE(g)` ==">1000","1500",`SEED_AVAILABLE(g)`))) %>% 
  select(NAME,pea_g_available)




pea_oat_experiment_design %>% 
  select(Pea,Location,PlotNumber) %>% 
  group_by(Pea,Location) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(g_plot = 60) %>% 
  mutate(g_location = (n*g_plot))



pea_oat_experiment_design %>% 
  select(Oat,Location,PlotNumber) %>% 
  group_by(Oat,Location) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(g_plot = 24) %>% 
  mutate(g_location = (n*g_plot))







