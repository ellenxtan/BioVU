library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
library(scales)

load("../output/data_raw.RData")
rm(cam_raw)
rm(discharge_raw)
static_raw %<>% select(grid)

changed_grid <- read_csv("../output/changed_grid_dob_20190924.csv")
cam_visits <- read_csv("../output/cam_stay_20190925.csv") 

#. liver-blirubin ---------------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/Bilirubin",
                         full.names = T)

#> out of range value ---
bilirubin_oor <- NULL
for (file in file_names) {
  bilirubin_oor <- bilirubin_oor %>% 
    bind_rows(read_excel(file, sheet = "out of range"))
}
Hmisc::describe(bilirubin_oor) #857 rows
bilirubin_oor %>% 
  distinct(`Tbil (mg/dL)`) %>% 
  pull(`Tbil (mg/dL)`) %>% 
  str_view_all( "[[:digit:]]*\\.*[[:digit:]]+")
bilirubin_raw <- bilirubin_oor %>% 
  rename(oor_value = `Tbil (mg/dL)`) %>% 
  mutate(`Tbil (mg/dL)` = if_else(
    str_detect(oor_value, "-"),
    str_extract_all(oor_value, "[[:digit:]]*\\.*[[:digit:]]+") %>% sapply(function(x) mean(as.numeric(x))),
    as.numeric(str_extract(oor_value, "[[:digit:]]*\\.*[[:digit:]]+"))
  )) 
bilirubin_raw %>% 
  distinct(oor_value, `Tbil (mg/dL)`)

#> normal value ---
for (file in file_names) {
  bilirubin_raw <- bilirubin_raw %>% 
    bind_rows(read_excel(file, sheet = 1))
}
Hmisc::describe(bilirubin_raw)
names(bilirubin_raw) <- tolower(names(bilirubin_raw))

#> convert messed-up GRIDs and dates ---
length(unique(bilirubin_raw$grid))
sum(unique(bilirubin_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(bilirubin_raw$grid) %in% unique(changed_grid$old_grid)) # no OLD GRID
sum(unique(bilirubin_raw$grid) %in% unique(changed_grid$updated_grid))
bilirubin_raw %>% 
  filter(!grid %in% static_raw$grid, !grid %in% changed_grid$updated_grid)



#> Only use those values falling in CAM-ICU encounters and take worst/maximum bilirubin for each day ---
bilirubin_raw %<>% 
  filter(!is.na(`tbil (mg/dl)`)) %>% 
  mutate(lab_date = as_date(lab_date))
bilirubin_daily <- sqldf::sqldf('SELECT * 
                                FROM cam_visits as t1
                                INNER JOIN bilirubin_raw as t2 
                                ON t1.grid = t2.grid AND lab_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, lab_date, `tbil (mg/dl)`) %>% 
  mutate(day = as.numeric(lab_date - adm_date) + 1) %>% 
  group_by(grid, adm_id, day, lab_date) %>% 
  summarise(bilirubin = max(`tbil (mg/dl)`)) %>% 
  ungroup() %>% 
  mutate(sofa_liver = case_when(
    bilirubin < 1.2 ~ 0,
    bilirubin < 2.0 ~ 1,
    bilirubin < 6.0 ~ 2,
    bilirubin <= 12.0 ~ 3,
    bilirubin > 12.0 ~ 4
  ))
bilirubin_daily %>% 
  count(sofa_liver)
ggplot(bilirubin_daily) +
  geom_histogram(aes(x = bilirubin))
ggplot(bilirubin_daily) +
  geom_boxplot(aes(y = bilirubin)) +
  scale_y_log10()

#. coagulation: platelets --------------------------------------------------
#> out of range value ---
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/Platelet",
                         pattern = "range.xlsx$",
                         full.names = T)
platelet_oor <- NULL
for (file in file_names) {
  platelet_oor <- platelet_oor %>% 
    bind_rows(read_excel(file))
}
Hmisc::describe(platelet_oor) # 1462 rows
platelet_oor %>% 
  distinct(`Plt-Ct (thou/uL)`) %>% 
  pull(`Plt-Ct (thou/uL)`) %>% 
  str_view_all( "[[:digit:]]+") 
platelet_raw <- platelet_oor %>% 
  rename(oor_value = `Plt-Ct (thou/uL)`) %>% 
  mutate(`Plt-Ct (thou/uL)` = if_else(
    str_detect(oor_value, "-"),
    str_extract_all(oor_value, "[[:digit:]]+") %>% sapply(function(x) mean(as.numeric(x))),
    as.numeric(str_extract(oor_value, "[[:digit:]]+"))
  )) 
platelet_raw %>% 
  distinct(oor_value, `Plt-Ct (thou/uL)`)

#> normal value ---
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/Platelet",
                         pattern = "labs.xlsx$",
                         full.names = T)
for (file in file_names) {
  platelet_raw <- platelet_raw %>% 
    bind_rows(read_excel(file))
}
Hmisc::describe(platelet_raw)
names(platelet_raw) <- tolower(names(platelet_raw))

#> convert messed-up GRIDs and dates ---
length(unique(platelet_raw$grid))
sum(unique(platelet_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(platelet_raw$grid) %in% unique(changed_grid$old_grid)) # No OLD GRIDS
sum(unique(platelet_raw$grid) %in% unique(changed_grid$updated_grid))
platelet_raw %>% 
  filter(!grid %in% static_raw$grid, !grid %in% changed_grid$updated_grid)




#> Only use those values falling in CAM-ICU encounters and take worst/minimum platelet for each day ---
platelet_raw %<>% 
  filter(!is.na(`plt-ct (thou/ul)`)) %>% 
  mutate(lab_date = as_date(lab_date))
platelet_daily <- sqldf::sqldf('SELECT * 
                               FROM cam_visits as t1
                               INNER JOIN platelet_raw as t2 
                               ON t1.grid = t2.grid AND lab_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, lab_date, `plt-ct (thou/ul)`) %>% 
  mutate(day = as.numeric(lab_date - adm_date) + 1) %>% 
  group_by(grid, adm_id, day, lab_date) %>% 
  summarise(platelet = min(`plt-ct (thou/ul)`)) %>% 
  ungroup() %>% 
  mutate(
    sofa_coagulation = case_when(
      platelet < 20 ~ 4,
      platelet < 50 ~ 3,
      platelet < 100 ~ 2,
      platelet < 150 ~ 1,
      platelet >= 150 ~ 0
    )
  )
platelet_daily %>% 
  count(sofa_coagulation)
ggplot(platelet_daily) +
  geom_histogram(aes(x = platelet))
ggplot(platelet_daily) +
  geom_boxplot(aes(y = platelet)) +
  scale_y_log10()



#. central nervous system: RASS -----------------------------------
#> convert messed-up GRIDs and dates ---
length(unique(rass_raw$grid))
sum(unique(rass_raw$grid) %in% unique(changed_grid$old_grid))
sum(unique(rass_raw$grid) %in% unique(changed_grid$updated_grid))
rass_raw1 <- rass_raw %>% 
  filter(rass_score %in% -5:4) %>% 
  left_join(changed_grid, by = c("grid" = "old_grid")) %>% 
  rename(dttm = rass_date) %>% 
  mutate(
    rass_date0 = str_sub(dttm, 1, 10),
    rass_date = if_else(!is.na(updated_grid),
                        mdy(rass_date0) - old_dob + updated_dob,
                        mdy(rass_date0)),
    rass_time = str_sub(dttm, -8, -1),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, rass_date, rass_time, rass_score) 
sum(rass_raw1$grid %in% changed_grid$old_grid)

#> Only use those values falling in CAM-ICU encounters and take worst/minimum RASS for each day ---
## can ignore duplicated RASS assessed at the same time, becasue we are taking worst daily value
rass_daily <- sqldf::sqldf('SELECT * 
                           FROM cam_visits as t1
                           INNER JOIN rass_raw1 as t2 
                           ON t1.grid = t2.grid AND rass_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, rass_date, rass_score) %>% 
  mutate(day = as.numeric(rass_date - adm_date) + 1) %>% 
  group_by(grid, adm_id, day, rass_date) %>% 
  summarise(rass = min(rass_score)) %>% 
  ungroup() %>% 
  mutate(
    ## use RASS method C in the paper b/c it had the best predictive validity based it's associaiton with mortality
    sofa_cns = case_when(
      rass >= 0 ~ 0,
      rass == -1 ~ 1,
      rass == -2 ~ 2,
      rass == -3 ~ 3,
      rass %in% -4:-5 ~ 4
    )
  )
rass_daily %>% 
  count(sofa_cns)
rass_daily %>% 
  count(rass)

#. Renal: Creatinine ----------------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/Creatinine",
                         full.names = T)

#> out of range value ---
creatinine_oor <- NULL
for (file in file_names) {
  creatinine_oor <- creatinine_oor %>% 
    bind_rows(read_excel(file, sheet = 2))
}
Hmisc::describe(creatinine_oor) #650 rows
creatinine_oor %>% 
  distinct(`Creat mg/dL`) %>% 
  pull(`Creat mg/dL`) %>% 
  str_view_all( "[[:digit:]]*\\.*[[:digit:]]+")
creatinine_raw <- creatinine_oor %>% 
  rename(oor_value = `Creat mg/dL`) %>% 
  mutate(`Creat mg/dL` = case_when(
    str_detect(oor_value, ",") ~ as.numeric(str_replace(oor_value, ",", ".")),
    str_detect(oor_value, "-") ~ str_extract_all(oor_value, "[[:digit:]]*\\.*[[:digit:]]+") %>% sapply(function(x) mean(as.numeric(x))),
    T ~ as.numeric(str_extract(oor_value, "[[:digit:]]*\\.*[[:digit:]]+"))
  )) 
creatinine_raw %>% 
  distinct(oor_value, `Creat mg/dL`)

#> normal value ---
for (file in file_names) {
  creatinine_raw <- creatinine_raw %>% 
    bind_rows(read_excel(file, sheet = 1))
}
Hmisc::describe(creatinine_raw)
names(creatinine_raw) <- tolower(names(creatinine_raw))

#> convert messed-up GRIDs and dates ---
length(unique(creatinine_raw$grid))
sum(unique(creatinine_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(creatinine_raw$grid) %in% unique(changed_grid$old_grid)) # no OLD GRID
sum(unique(creatinine_raw$grid) %in% unique(changed_grid$updated_grid))
creatinine_raw %>% 
  filter(!grid %in% static_raw$grid, !grid %in% changed_grid$updated_grid)


#> Only use those values falling in CAM-ICU encounters and take worst/maximum creatinine for each day ---
creatinine_raw %<>% 
  filter(!is.na(`creat mg/dl`)) %>% 
  mutate(lab_date = as_date(lab_date))
creatinine_daily <- sqldf::sqldf('SELECT * 
                                 FROM cam_visits as t1
                                 INNER JOIN creatinine_raw as t2 
                                 ON t1.grid = t2.grid AND lab_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, lab_date, `creat mg/dl`) %>% 
  mutate(day = as.numeric(lab_date - adm_date) + 1) %>% 
  group_by(grid, adm_id, day, lab_date) %>% 
  summarise(creatinine = max(`creat mg/dl`)) %>% 
  ungroup() %>% 
  mutate(
    sofa_renal = case_when(
      creatinine < 1.2 ~ 0,
      creatinine < 2.0 ~ 1,
      creatinine < 3.5 ~ 2,
      creatinine <= 5.0 ~ 3,
      creatinine > 5.0 ~ 4
    )
  )
creatinine_daily %>% 
  count(sofa_renal)
ggplot(creatinine_daily) +
  geom_histogram(aes(x = creatinine))
ggplot(creatinine_daily) +
  geom_boxplot(aes(y = creatinine)) +
  scale_y_log10()



#. Cardiovascular: MAP and pressor --------------------

#> MAP data ---------------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/Blood Pressure",
                         full.names = T)
map_raw <- NULL
for (file in file_names) {
  map_raw <- map_raw %>% 
    bind_rows(read_tsv(file, na = c("", "NA", "null")))
}
names(map_raw) <- str_to_lower(names(map_raw))

map_raw %>% 
  filter(!str_detect(blood_pressure, "/")) 
map_raw %>% 
  filter(is.na(blood_pressure)) 


#> convert messed-up GRIDs and dates ---
length(unique(map_raw$grid))
sum(unique(map_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(map_raw$grid) %in% unique(changed_grid$old_grid)) # no OLD GRID
sum(unique(map_raw$grid) %in% unique(changed_grid$updated_grid))
map_raw %>% 
  filter(!grid %in% static_raw$grid, !grid %in% changed_grid$updated_grid)


#> Only use those values falling in CAM-ICU encounters and take worst/minimum MAP for each day ---
map_raw %<>% 
  filter(!is.na(blood_pressure)) %>% 
  mutate(test_date = as_date(test_date))
#! keep sbp and dbp to check
map_daily <- sqldf::sqldf('SELECT * 
                                 FROM cam_visits as t1
                                 INNER JOIN map_raw as t2 
                                 ON t1.grid = t2.grid AND test_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, test_date, blood_pressure) %>% 
  mutate(day = as.numeric(test_date - adm_date) + 1,
         sbp = sapply(str_split(blood_pressure, "/"), '[', 1) %>% as.numeric(),
         dbp = sapply(str_split(blood_pressure, "/"), '[', 2) %>% as.numeric(),
         map = (sbp + 2*dbp)/3
         ) %>% 
  group_by(grid, adm_id, day, test_date) %>% 
  summarise(map = min(map)) %>% 
  ungroup()
ggplot(map_daily) +
  geom_histogram(aes(x = map))
ggplot(map_daily) +
  geom_boxplot(aes(y = map)) 

#> pressor data ---------------------------
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
  select(drug_name, starts_with('drug_route')) %>% 
  Hmisc::describe()

#> convert messed-up GRIDs and dates ---
length(unique(pressor_raw$grid))
sum(unique(pressor_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(pressor_raw$grid) %in% unique(changed_grid$old_grid))
sum(unique(pressor_raw$grid) %in% unique(changed_grid$updated_grid))


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
sum(pressor_raw1$grid %in% changed_grid$old_grid)

#> Only use those values falling in CAM-ICU encounters and take worst pressor for each day ---
levels(factor(pressor_raw1$drug_name)) 
pressor_daily <- sqldf::sqldf('SELECT * 
                                 FROM cam_visits as t1
                                 INNER JOIN pressor_raw1 as t2 
                                 ON t1.grid = t2.grid AND drug_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, drug_date, drug_name) %>% 
  mutate(day = as.numeric(drug_date - adm_date) + 1) %>% 
  group_by(grid, adm_id, day, drug_date) %>% 
  summarise(pressor = max(drug_name)) %>% 
  ungroup() 

#> combine MAP and pressor data -------
map_pressor_daily <- map_daily %>% 
  full_join(pressor_daily,
            by = c("grid", "adm_id", "day", "test_date" = "drug_date")) %>% 
  mutate(sofa_cardio = case_when(
    pressor %in% c("EPINEPHRINE", "NOREPINEPHRINE") ~ 3,
    pressor %in% c("DOBUTAMINE", "DOPAMINE") ~ 2,
    map < 70 ~ 1,
    map >= 70 ~ 0)
    )
map_pressor_daily %>% count(sofa_cardio)  



#. Respiration ---------------------------------------------
#> ventilationd data ----------------
vent_raw <- read_excel("../../Mito Delirium BioVU Data/Phenotype data/ventilation_days.xlsx")
names(vent_raw) <- str_to_lower((names(vent_raw)))
vent_raw %>% Hmisc::describe()

#> convert messed-up GRIDs and dates ---
length(unique(vent_raw$grid))
sum(unique(vent_raw$grid) %in% static_raw$grid)
sum(unique(vent_raw$grid) %in% changed_grid$old_grid)
sum(unique(vent_raw$grid) %in% changed_grid$updated_grid)
vent_raw1 <- vent_raw %>% 
  left_join(changed_grid, by = c("grid" = "old_grid")) %>% 
  mutate(
    code_date = if_else(!is.na(updated_grid),
                        as_date(code_date) - old_dob + updated_dob,
                        as_date(code_date)),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, code_date) %>% 
  mutate(vent = 1) %>% 
  arrange(grid, code_date)
sum(vent_raw1$grid %in% changed_grid$old_grid)


vent_daily <- sqldf::sqldf('SELECT * 
                                 FROM cam_visits as t1
                                 INNER JOIN vent_raw1 as t2 
                                 ON t1.grid = t2.grid AND code_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, code_date, vent) %>% 
  mutate(day = as.numeric(code_date - adm_date) + 1) %>% 
  select(-adm_date) ## vent_raw1 already at daily level

#> PaO2/FiO2 Ratio data ---------------
pf_ratio_raw <- read_csv("../output/pao2_fio2_ratio_calc_20190927.csv")
Hmisc::describe(pf_ratio_raw)
sum(unique(pf_ratio_raw$grid) %in% static_raw$grid)
sum(unique(pf_ratio_raw$grid) %in% changed_grid$old_grid)
sum(unique(pf_ratio_raw$grid) %in% changed_grid$updated_grid)
pf_ratio_raw %>% 
  filter(!grid %in% static_raw$grid, !grid %in% changed_grid$updated_grid)


#> Only use those values falling in CAM-ICU encounters and take worst/min ratio for each day ---
pf_ratio_raw %<>% 
  filter(!is.na(ratio_c)) %>% 
  mutate(lab_time = as.character(lab_time))
pf_ratio_daily <- sqldf::sqldf('SELECT * 
                                 FROM cam_visits as t1
                                 INNER JOIN pf_ratio_raw as t2 
                                 ON t1.grid = t2.grid AND lab_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, lab_date, ratio_c) %>% 
  mutate(day = as.numeric(lab_date - adm_date) + 1) %>% 
  group_by(grid, adm_id, day, lab_date) %>% 
  summarise(pf_ratio = min(ratio_c)) %>% 
  ungroup() 
ggplot(pf_ratio_daily) +
  geom_histogram(aes(x = pf_ratio))
ggplot(pf_ratio_daily) +
  geom_boxplot(aes(y = pf_ratio)) +
  scale_y_log10()

#> SpO2/FiO2 Ratio data -----------------
sf_ratio_raw <- read_csv("../output/spo2_fio2_ratio_calc_20191010.csv")
Hmisc::describe(sf_ratio_raw)
sum(unique(sf_ratio_raw$grid) %in% static_raw$grid)
sum(unique(sf_ratio_raw$grid) %in% changed_grid$old_grid)
sum(unique(sf_ratio_raw$grid) %in% changed_grid$updated_grid)
sf_ratio_raw %>% 
  filter(!grid %in% static_raw$grid, !grid %in% changed_grid$updated_grid)

#> Only use those values falling in CAM-ICU encounters and take worst/min ratio for each day ---
sf_ratio_raw %<>% 
  filter(!is.na(ratio_c)) %>% 
  mutate(lab_time = as.character(lab_time))
sf_ratio_daily <- sqldf::sqldf('SELECT * 
                               FROM cam_visits as t1
                               INNER JOIN sf_ratio_raw as t2 
                               ON t1.grid = t2.grid AND lab_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, lab_date, ratio_c) %>% 
  mutate(day = as.numeric(lab_date - adm_date) + 1) %>% 
  group_by(grid, adm_id, day, lab_date) %>% 
  summarise(sf_ratio = min(ratio_c)) %>% 
  ungroup() 
ggplot(sf_ratio_daily) +
  geom_histogram(aes(x = sf_ratio))
ggplot(sf_ratio_daily) +
  geom_boxplot(aes(y = sf_ratio)) +
  scale_y_log10()


#> combine ventilation and ratios data -----------

ratio_vent_daily <- pf_ratio_daily %>% 
  full_join(sf_ratio_daily, by = c("grid", "adm_id", "day", "lab_date")) %>% 
  full_join(vent_daily, by = c("grid", "adm_id", "day", "lab_date" = "code_date")) %>% 
  mutate(sofa_respiration = case_when(
    (pf_ratio < 100 | sf_ratio < 89) & vent == 1 ~ 4,
    (pf_ratio < 200 | sf_ratio < 214) & vent == 1 ~ 3,
    pf_ratio < 300 | sf_ratio < 357 ~ 2, # including ratio < 200 and < 100 but no vent
    pf_ratio < 400 | sf_ratio < 512 ~ 1,
    pf_ratio >= 400 | sf_ratio >= 512 ~ 0
  ))
ratio_vent_daily %>% count(sofa_respiration)
ratio_vent_daily %>% 
  filter(is.na(sofa_respiration))
# still a lot of missing


resp_infection <-  sqldf::sqldf('SELECT * 
                                FROM infections_w1d as t1
                                INNER JOIN ratio_vent_daily as t2 
                                ON t1.grid = t2.grid AND lab_date BETWEEN onset_date-2 AND onset_date+1') %>% 
  as_tibble() %>% 
  select(-grid..7) %>% 
  group_by(grid, adm_id) %>% 
  filter(is.na(sofa_respiration)) %>% 
  summarise(sofa_respiration = max(sofa_respiration, na.rm = T)) %>% 
  ungroup() # %>%  # 3818 encounters had ventaltion but no ratio data.
#  count(sofa_respiration) 
resp_infection %>% 
  Hmisc::describe()





#> merge daily SOFA scores together ----------------------------------------------------
sofa_daily <- bilirubin_daily %>% 
  full_join(creatinine_daily, by = c("grid", "adm_id", "day", "lab_date")) %>% 
  full_join(platelet_daily, by = c("grid", "adm_id", "day", "lab_date")) %>% 
  full_join(rass_daily, by = c("grid", "adm_id", "day", "lab_date" = "rass_date")) %>% 
  full_join(map_pressor_daily, by = c("grid", "adm_id", "day", "lab_date" = "test_date")) %>% 
  full_join(ratio_vent_daily, by = c("grid", "adm_id", "day", "lab_date")) %>% 
  mutate(sofa = case_when(
    is.na(sofa_respiration) + is.na(sofa_coagulation) + is.na(sofa_liver) + 
      is.na(sofa_cardio) + is.na(sofa_cns) + is.na(sofa_renal) != 6 ~
      coalesce(sofa_liver, 0) + coalesce(sofa_coagulation, 0) + 
      coalesce(sofa_cns, 0) + coalesce(sofa_cardio, 0) + 
      coalesce(sofa_respiration, 0)) + coalesce(sofa_renal, 0))


sofa_daily %>% 
  count(sofa) %>% 
  print(n = 30)
sofa_daily %>% 
  count(data_type)
sofa_daily %>% 
  filter(data_type == "All missing", vent == 1) 
sofa_daily %>% 
  select(sofa_coagulation, sofa_liver, sofa_cardio, sofa_cns, sofa_respiration, sofa_renal) %>% 
  sapply(function(x) percent(sum(is.na(x))/length(x))) 

write_csv(sofa_daily, "../output/daily_sofa_score_20191010.csv")
