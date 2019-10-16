library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)

rhee_infection <- read_csv("../output/rhee_infection_20191015.csv")
changed_grid <- read_csv("../output/changed_grid_dob_20190924.csv")
static_raw <- read_csv("../../Mito Delirium BioVU Data/Data/Samuels_Delirium_STATIC_20180718.csv") %>% 
  select(GRID)
names(static_raw) <- str_to_lower(names(static_raw))

#. Vasopressor initiation ------------------
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
#' not exactly the same names as in Rhee's paper 

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
pressor_raw1 %>% 
  count(drug_route1, drug_route2, drug_route3)

# keep IV vasopressors only
pressor_raw1 %<>% 
  filter(drug_route1 == "IV" | drug_route2 == "IV" | drug_route3 == "IV")


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
  distinct(grid, adm_id, adm_date, dc_date, blood_date) %>% 
  mutate(new_pressor = 1)

#. Mechnical ventilation initiation -----------------
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
  arrange(grid, code_date)
sum(vent_raw1$grid %in% changed_grid$old_grid)

#> find initiation of mechanical ventilation (no ventilation on the prior calendar day) ---
vent_new_date <- sqldf::sqldf('SELECT *
        FROM vent_raw1 as t1
        left join vent_raw1 as t2
        ON t1.grid = t2.grid AND t2.code_date == t1.code_date - 1') %>% 
  as_tibble() %>% 
  filter(is.na(grid..3)) %>% 
  distinct(grid, code_date) 
vent_new <- sqldf::sqldf('SELECT * 
              FROM rhee_infection as t1
             INNER JOIN vent_new_date as t2 
             ON t1.grid = t2.grid AND code_date <= dc_date AND code_date BETWEEN blood_date-2 AND blood_date+2') %>% 
  as_tibble() %>% 
  select(-grid..6) %>% 
  distinct(grid, adm_id, adm_date, dc_date, blood_date) %>%
  mutate(new_vent = 1)

#. lactate ---------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/Lactate",
                         full.names = T)

#> out of range value ---
lactate_oor <- NULL
for (file in file_names) {
  lactate_oor <- lactate_oor %>% 
    bind_rows(read_excel(file, sheet = 2))
}
Hmisc::describe(lactate_oor) #238 rows
lactate_oor %>% 
  distinct(`LAC (mmol/L)`) %>% 
  pull(`LAC (mmol/L)`) %>% 
  str_view_all( "[[:digit:]]*\\.*[[:digit:]]+")
lactate_raw <- lactate_oor %>% 
  rename(oor_value = `LAC (mmol/L)`) %>% 
  mutate(`LAC (mmol/L)` = as.numeric(str_extract(oor_value, "[[:digit:]]*\\.*[[:digit:]]+"))
  ) 
lactate_raw %>% 
  distinct(oor_value, `LAC (mmol/L)`)

#> normal value ---
for (file in file_names) {
  lactate_raw <- lactate_raw %>% 
    bind_rows(read_excel(file, sheet = 1))
}
Hmisc::describe(lactate_raw)
names(lactate_raw) <- tolower(names(lactate_raw))

#> convert messed-up GRIDs and dates ---
length(unique(lactate_raw$grid))
sum(unique(lactate_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(lactate_raw$grid) %in% unique(changed_grid$old_grid)) # no OLD GRID
sum(unique(lactate_raw$grid) %in% unique(changed_grid$updated_grid))
lactate_raw %>% 
  filter(!grid %in% static_raw$grid, !grid %in% changed_grid$updated_grid)

#> find >= 2 days
lactate_date <- lactate_raw %>% 
  filter(`lac (mmol/l)` >= 2.0) %>% 
  distinct(grid, lab_date) %>% 
  mutate(lab_date = as_date(lab_date))
lactate_ge2 <- sqldf::sqldf('SELECT * 
              FROM rhee_infection as t1
                         INNER JOIN lactate_date as t2 
                         ON t1.grid = t2.grid AND lab_date <= dc_date AND lab_date BETWEEN blood_date-2 AND blood_date+2') %>% 
  as_tibble() %>% 
  select(-grid..6) %>% 
  distinct(grid, adm_id, adm_date, dc_date, blood_date) %>%
  mutate(lactate_ge2 = 1)

#. creatinine doubling ------------------------
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
creatinine_raw %<>% 
  filter(!is.na(`creat mg/dl`)) %>% 
  mutate(lab_date = as_date(lab_date))

# find baseline for each hospitaliztion, then filter to find creatinine doubling within 2 day window 
creatinine_dbl <- sqldf::sqldf('SELECT * 
              FROM rhee_infection as t1
              INNER JOIN creatinine_raw as t2 
              ON t1.grid = t2.grid AND lab_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, blood_date, lab_date, `creat mg/dl`) %>% 
  group_by(grid, adm_id) %>% 
  mutate(creat_bl = min(`creat mg/dl`)) %>% 
  ungroup() %>% 
  group_by(grid, adm_id, adm_date, dc_date, blood_date) %>% 
  filter(lab_date >= blood_date - 2, 
         lab_date <= blood_date + 2,
         `creat mg/dl` >= 2*creat_bl) %>% 
  ungroup() %>% 
  distinct(grid, adm_id, adm_date, dc_date, blood_date) %>% 
  mutate(creat_dbl  = 1)


#. biliburin doubling --------------------------------------------------------
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
bilirubin_raw %<>% 
  filter(!is.na(`tbil (mg/dl)`)) %>% 
  mutate(lab_date = as_date(lab_date))

# find baseline for each hospitaliztion, then filter to find bilirubin doubling within 2 day window 
bilirubin_dbl <- sqldf::sqldf('SELECT * 
              FROM rhee_infection as t1
              INNER JOIN bilirubin_raw as t2 
              ON t1.grid = t2.grid AND lab_date BETWEEN adm_date-3 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, blood_date, lab_date, `tbil (mg/dl)`) %>% 
  group_by(grid, adm_id) %>% 
  mutate(bil_bl = min(`tbil (mg/dl)`)) %>% 
  ungroup() %>% 
  group_by(grid, adm_id, adm_date, dc_date, blood_date) %>% 
  filter(lab_date >= blood_date - 2, 
         lab_date <= blood_date + 2,
         `tbil (mg/dl)` >= 2.0,
         `tbil (mg/dl)` >= 2*bil_bl) %>% 
  ungroup() %>% 
  distinct(grid, adm_id, adm_date, dc_date, blood_date) %>% 
  mutate(bil_dbl  = 1)


#. platelet decline ---------------------
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
platelet_raw %<>%
  filter(!is.na(`plt-ct (thou/ul)`)) %>%
  mutate(lab_date = as_date(lab_date))

# find baseline for each hospitaliztion, then filter to find platelet doubling within 2 day window
platelet_dcl <- sqldf::sqldf('SELECT *
                              FROM rhee_infection as t1
                              INNER JOIN platelet_raw as t2
                              ON t1.grid = t2.grid AND lab_date BETWEEN adm_date-3 AND dc_date') %>%
  as_tibble() %>%
  select(grid, adm_id, adm_date, dc_date, blood_date, lab_date, `plt-ct (thou/ul)`) %>%
  group_by(grid, adm_id) %>%
  mutate(plt_bl = max(`plt-ct (thou/ul)`)) %>%
  ungroup() %>%
  group_by(grid, adm_id, adm_date, dc_date, blood_date) %>%
  filter(lab_date >= blood_date - 2,
         lab_date <= blood_date + 2,
         plt_bl > 0.1,
         `plt-ct (thou/ul)` < 0.1,
         `plt-ct (thou/ul)` <= 0.5*plt_bl) %>%
  ungroup() %>%
  distinct(grid, adm_id, adm_date, dc_date, blood_date) %>%
  mutate(plt_dcl  = 1)

#' The minimum plt count is 1000/ul, so no one met criteria.

#. Put everything together -----------------------------------------------------
## keep the first blood date with the worst organ dysfunction value for each encounter
rhee_sepsis <- rhee_infection %>% 
  left_join(pressor_new) %>% 
  left_join(vent_new) %>% 
  left_join(creatinine_dbl) %>% 
  left_join(bilirubin_dbl) %>% 
  left_join(platelet_dcl) %>% 
  left_join(lactate_ge2) %>% 
  mutate(rhee = if_else(is.na(new_pressor | new_vent | creat_dbl | bil_dbl | plt_dcl | lactate_ge2), 0, 1)) %>% 
  group_by(grid, adm_id, adm_date, dc_date) %>% 
  slice(which.max(rhee)) %>% 
  ungroup()
rhee_sepsis %>% 
  count(rhee)
write_csv(rhee_sepsis, "../output/sepsis_rhee_20191015.csv")


#. compare with sepsis3 infection -------------------------------
cam_visits <- read_csv("../output/cam_stay_20190925.csv") 
sepsis3 <- read_csv("../output/sepsis3_20191014.csv")

sepsis3 %>% count(sepsis3)


sepsis_comp <- cam_visits %>% 
  select(grid, adm_id) %>% 
  left_join(sepsis3 %>% 
              select(grid, adm_id, sofa, sepsis3, data_type)) %>% 
  left_join(rhee_sepsis %>% 
              select(grid, adm_id, rhee)) %>%  
  mutate(sepsis3_infection = if_else(is.na(sepsis3), 0, 1),
         sepsis3 = if_else(is.na(sepsis3), 0, sepsis3),
         rhee_infection = if_else(is.na(rhee), 0, 1),
         rhee = if_else(is.na(rhee), 0, rhee))
xtabs(~ sepsis3_infection + rhee_infection, data = sepsis_comp, addNA = T)
xtabs(~ sepsis3 + rhee, data = sepsis_comp, addNA = T)
sepsis_comp %>% 
  filter(rhee_infection == 1) %>% 
  with(xtabs(~sepsis3 + rhee))

sepsis_comp %>% filter(rhee == 1, sepsis3 == 0) %>% count(data_type)

## All rhee infections are sepsis3 infections

