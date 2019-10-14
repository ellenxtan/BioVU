library(tidyverse)
library(readxl)
library(scales)

infections <- read_csv("../output/sepsis3_all_infections_20190927.csv")
daily_sofa <- read_csv("../output/daily_sofa_score_20191010.csv")



infections %>% 
  count(onset_day)
## we decided to limit onset date 1 day within hospital admission date
infections_w1d <- infections %>% 
  filter(onset_day %in% 0:2)



infection_sofa <- sqldf::sqldf('SELECT * 
                               FROM infections_w1d as t1
                               LEFT JOIN daily_sofa as t2 
                               ON t1.grid = t2.grid AND t1.adm_id = t2.adm_id AND lab_date BETWEEN onset_date-2 AND onset_date+1') %>% 
  as_tibble() %>% 
  select(-grid..7, -adm_id..8, -day) 

infection_sofa %>% 
  select(sofa_coagulation, sofa_liver, sofa_cardio, sofa_cns, sofa_respiration, sofa_renal) %>% 
  sapply(function(x) percent(sum(is.na(x))/length(x))) 
infection_sofa %>% 
  filter(is.na(sofa_respiration)) %>% 
  select(grid, adm_id, lab_date, pf_ratio, sf_ratio, vent)
infection_sofa %>% 
  filter(is.na(sofa_respiration), vent == 1) %>% 
  select(grid, adm_id, lab_date, pf_ratio, sf_ratio, vent)

## keep the first worst SOFA for each encounter
sepsis3 <- infection_sofa %>% 
  group_by(grid, adm_id, adm_date, dc_date, onset_date, onset_day) %>% 
  slice(which.max(sofa)) %>% 
  ungroup() %>% 
  mutate(sepsis3 = if_else(sofa >= 2, 1, 0))
sepsis3 %>% Hmisc::describe()
sepsis3 %>% count(sofa) %>% print(n = 30)
sepsis3 %>% count(sepsis3)
sepsis3 %>% count(data_type)
sepsis3 %>% 
  select(sofa_coagulation, sofa_liver, sofa_cardio, sofa_cns, sofa_respiration, sofa_renal) %>% 
  sapply(function(x) percent(sum(is.na(x))/length(x))) 

sepsis3 %>% 
  filter(sofa < 2) %>% 
  select(sofa_coagulation, sofa_liver, sofa_cardio, sofa_cns, sofa_respiration, sofa_renal) %>% 
  sapply(function(x) percent(sum(is.na(x))/length(x))) 
sepsis3 %>% 
  filter(sofa < 2) %>% 
  count(data_type)

write_csv(sepsis3, "../output/sepsis3_20191014.csv")


