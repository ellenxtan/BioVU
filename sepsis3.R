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

## check values here and for sofa < 2 found missing
sepsis1 <- infection_sofa %>% 
  group_by(grid, adm_id, adm_date, dc_date, onset_date, onset_day) %>% 
  summarise(sofa = max(sofa, na.rm = T),
            n_days = n(),
            na_coag = sum(is.na(sofa_coagulation)),
            na_liver = sum(is.na(sofa_liver)),
            na_cardio = sum(is.na(sofa_cardio)),
            na_cns = sum(is.na(sofa_cns)),
            na_resp = sum(is.na(sofa_respiration)),
            na_renal = sum(is.na(sofa_renal))) %>% 
  ungroup() 
sepsis1 %>% Hmisc::describe()
sepsis1 %>% count(sofa) %>% print(n = 30)


sepsis1 %>% 
  select(na_coag, na_liver, na_cardio, na_cns, na_resp, na_renal) %>% 
  sapply(function(x) percent(sum(x == sepsis1$n_days)/nrow(sepsis1))) 
sepsis1 %>% 
  filter(sofa < 2) %>% 
  select(na_coag, na_liver, na_cardio, na_cns, na_resp, na_renal) %>% 
  sapply(function(x) percent(sum(x == sepsis1$n_days[sepsis1$sofa < 2])/sum(sepsis1$sofa < 2))) 
#' it's mostly missing respiration SOFA.


infection_sofa_l2 <- infection_sofa %>% 
  semi_join(
    sepsis1 %>%
      filter(sofa < 2),
    by = c("grid", "adm_id")
  )
infection_sofa_l2 %>% 
  select(sofa_coagulation, sofa_liver, sofa_cardio, sofa_cns, sofa_respiration, sofa_renal) %>% 
  sapply(function(x) percent(sum(is.na(x))/length(x))) 
infection_sofa_l2  %>% 
  select(grid, adm_id, onset_date, lab_date, starts_with('sofa'))

write_csv(sepsis1, "sepsis3_20191011.csv")


# ## keep organ system sofa scores
# sepsis2 <- infection_sofa %>% 
#   group_by(grid, adm_id) %>% 
#   filter(sofa == max(sofa, na.rm = T)) %>% 
#   ungroup()
# sepsis2 %>% 
#   group_by(grid, adm_id) %>% 
#   filter(n()> 1) %>% 
#   select(grid, adm_id, onset_date, lab_date, starts_with('sofa'))
