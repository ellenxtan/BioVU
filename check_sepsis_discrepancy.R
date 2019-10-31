library(tidyverse)
library(magrittr)
library(lubridate)

dob_dis <- read_csv("../output/dob_discrepancy.csv") 
cam_visits <- read_csv("../output/cam_stay_20190925.csv") 
sepsis3 <- read_csv("../output/sepsis3_20191014.csv")
rhee_sepsis <- read_csv("../output/sepsis_rhee_20191015.csv")

sepsis_comp <- cam_visits %>% 
  select(grid, adm_id) %>% 
  left_join(sepsis3) %>% 
  left_join(rhee_sepsis) %>%  
  mutate(sepsis3_infection = if_else(is.na(sepsis3), 0, 1),
         sepsis3 = if_else(is.na(sepsis3), 0, sepsis3),
         rhee_infection = if_else(is.na(rhee), 0, 1),
         rhee = if_else(is.na(rhee), 0, rhee))

cam_visits %<>% anti_join(dob_dis, by = "grid")
sepsis3 %<>% anti_join(dob_dis, by = "grid")
rhee_sepsis %<>% anti_join(dob_dis, by = "grid")
sepsis_comp %<>% anti_join(dob_dis, by = "grid")
rhee_infection %<>% anti_join(dob_dis, by = "grid")

sepsis_comp %>% 
  filter(rhee == 1, sepsis3 == 0) %>% 
  select(new_pressor:lactate_ge2)
sepsis_comp %>% 
  filter(rhee == 1, sepsis3 == 0) %>% 
  select(new_pressor:lactate_ge2) %>% 
  Hmisc::describe()
sepsis_comp %>% 
  filter(rhee == 1, sepsis3 == 0, lactate_ge2 == 1) %>% 
  select(grid, adm_id, bilirubin:sofa)
sepsis_comp %>% 
  filter(rhee == 1, sepsis3 == 0, bil_dbl == 1) %>% 
  select(grid, adm_id, bilirubin:sofa)  
sepsis_comp %>% 
  filter(rhee == 1, sepsis3 == 0, creat_dbl == 1) %>% 
  select(grid, adm_id, creatinine, sofa_renal, starts_with("sofa"))  
sepsis_comp %>% 
  filter(rhee == 1, sepsis3 == 0, new_pressor == 1) %>% 
  select(grid, adm_id, onset_date, lab_date, pressor, map, sofa_cardio, blood_date)  
sepsis_comp %>% 
  filter(rhee == 1, sepsis3 == 0, new_vent == 1) %>% 
  select(grid, adm_id, vent, pf_ratio, sf_ratio, starts_with("sofa"))  


#> check pressor data -----------------------------------------
rhee_infection <- read_csv("../output/rhee_infection_20191015.csv")
changed_grid <- read_csv("../output/changed_grid_dob_20190924.csv")

med_raw <- NULL
for (i in 1:3) {
  med_raw %<>% 
    bind_rows(
      read_csv(
        paste0("../../Mito Delirium BioVU Data/Data/grid_date_med", i, ".csv"),
        col_names = c("grid", "drug_date", "drug_name", "drug_class", 
                      "drug_route1", "drug_route2", "drug_route3"),
        skip = 1
      ) 
    ) 
}
med_raw %>% count(drug_class)
pressor_raw <- med_raw %>% 
  filter(drug_class == "pressor") %>% 
  select(-drug_class) 
pressor_raw %>% 
  select(drug_date, drug_name, starts_with('drug_route')) %>% 
  Hmisc::describe()


pressor_raw1 <- pressor_raw %>% 
  left_join(changed_grid, by = c("grid" = "old_grid")) %>% 
  mutate(
    drug_date = if_else(!is.na(updated_grid),
                        mdy(drug_date) - old_dob + updated_dob,
                        mdy(drug_date)),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, drug_date, drug_name, drug_route1, drug_route2, drug_route3) %>% 
  arrange(grid, drug_date) # one duplicate
# keep IV vasopressors only, since the med data was updated by Kaveh on 10/19, use only the 4 pressor names as before
pressor_raw1 %<>% 
  filter(drug_route1 == "IV" | drug_route2 == "IV" | drug_route3 == "IV", 
         drug_name %in% c("DOBUTAMINE", "DOPAMINE", "EPINEPHRINE", "NOREPINEPHRINE"))


#> find initiation of new vasopressors (no same vasopressor on the prior calendar day) ---
pressor_new_date <- sqldf::sqldf('SELECT *
        FROM pressor_raw1 as t1
        left join pressor_raw1 as t2
        ON t1.grid = t2.grid AND t1.drug_name == t2.drug_name AND t2.drug_date == t1.drug_date - 1') %>% 
  as_tibble() %>% 
  filter(is.na(grid..7)) %>% 
  distinct(grid, drug_date) 

pressor_new <- sqldf::sqldf('SELECT * 
              FROM rhee_infection as t1
             INNER JOIN pressor_new_date as t2 
             ON t1.grid = t2.grid AND drug_date <= dc_date AND drug_date BETWEEN blood_date-2 AND blood_date+2') %>% 
  as_tibble() %>% 
  select(-grid..6) %>% 
  distinct(grid, adm_id, adm_date, dc_date, blood_date, drug_date) %>% 
  mutate(new_pressor = 1)

pressor_new %>% 
  semi_join(sepsis_comp %>% 
              filter(rhee == 1, sepsis3 == 0, new_pressor == 1),
            by = c("grid", "adm_id")
    
  ) %>% 
  arrange(grid, adm_id, blood_date)
sepsis_comp %>% 
  filter(rhee == 1, sepsis3 == 0, new_pressor == 1) %>% 
  select(grid, adm_id, onset_date, lab_date, pressor, map, sofa_cardio, blood_date, new_pressor)  
pressor_raw1 %>% 
  filter(grid == "R202002632")
rhee_sepsis %>% 
  filter(grid == "R202002632")
