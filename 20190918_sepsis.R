

library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
library(scales)

#. Identify suspected infection -----------------------------------------------
#> Data import and clean ---------------
load("../output/data_raw.RData")
changed_grid <- read_csv("../output/changed_grid_dob.csv")
cam_visits <- read_csv("../output/cam_stay_20190917.csv") 
blood_raw <- read_excel("../../Mito Delirium BioVU Data/Phenotype data/culture_merge.xlsx",
                        sheet = "Blood Culture Days")
names(blood_raw) <- str_to_lower(names(blood_raw))
names(changed_grid) <- str_to_lower(names(changed_grid))


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
abx_raw <- med_raw %>% 
  filter(drug_class == "antibiotic") %>% 
  select(-drug_class) 


#> convert messed-up GRIDs and dates -------------------
length(unique(abx_raw$grid))
sum(unique(abx_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(abx_raw$grid) %in% unique(changed_grid$old_grid))
sum(unique(abx_raw$grid) %in% unique(changed_grid$updated_grid))

length(unique(blood_raw$grid))
sum(unique(blood_raw$grid) %in% unique(static_raw$grid)) 
sum(unique(blood_raw$grid) %in% unique(changed_grid$old_grid))
sum(unique(blood_raw$grid) %in% unique(changed_grid$updated_grid))

abx_raw1 <- abx_raw %>% 
  left_join(changed_grid, by = c("grid" = "old_grid")) %>% 
  mutate(
    drug_date = case_when(
      dob != dummy_dob ~ mdy(drug_date) - dob + dummy_dob,
      T ~ mdy(drug_date)),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, drug_date, drug_name, drug_route1, drug_route2, drug_route3) %>% 
  arrange(grid, drug_date) # some duplicates
sum(abx_raw1$grid %in% changed_grid$old_grid)
changed_grid %>% 
  filter(dob != dummy_dob, old_grid %in% abx_raw$grid) 
abx_raw1 %>% filter(grid == "R200032573") %>% 
  select(grid, drug_date, drug_name) %>% 
  print(n = 40)
abx_raw %>% 
  filter(grid %in% c("R272295989", "R200032573")) %>% 
  select(grid, drug_date, drug_name) %>% 
  print(n = 40)


blood_raw1 <- blood_raw %>% 
  left_join(changed_grid, by = c("grid" = "old_grid")) %>% 
  mutate(
    blood_date = case_when(
      dob != dummy_dob ~ as_date(`blood culture code_date`) - dob + dummy_dob,
      T~ as_date(`blood culture code_date`)),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, blood_date) %>% 
  arrange(grid, blood_date) # some duplicates
sum(blood_raw1$grid %in% changed_grid$old_grid)
changed_grid %>% 
  filter(dob != dummy_dob, old_grid %in% blood_raw$grid) 
blood_raw1 %>% filter(grid == "R200032573") %>% 
  select(grid, blood_date) %>% 
  print(n = 40)
blood_raw %>% 
  filter(grid %in% c("R272295989", "R200032573")) %>% 
  select(grid, `blood culture code_date`) %>% 
  print(n = 40)


#> assign adm_id to each abx/blood draw -----------------------------
abx_raw2 <- sqldf::sqldf('SELECT * 
                         FROM cam_visits as t1
                         INNER JOIN abx_raw1 as t2 
                         ON t1.grid = t2.grid AND drug_date BETWEEN adm_date-1 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, drug_date:drug_route3) %>% 
  mutate(day = as.numeric(drug_date - adm_date) + 1) %>% 
  select(grid, adm_id, adm_date, dc_date, drug_date, day, drug_name:drug_route3) 
abx_distinct <- abx_raw2 %>% 
  distinct(grid, adm_id, adm_date, dc_date, drug_date, day)
## check abx dates counted in two stays
abx_distinct %>% 
  count(day)
abx_distinct %>% 
  group_by(grid, drug_date) %>% 
  count() %>% 
  filter(n > 1)

blood_raw2 <- sqldf::sqldf('SELECT * 
                           FROM cam_visits as t1
                           INNER JOIN blood_raw1 as t2
                           ON t1.grid = t2.grid AND blood_date BETWEEN adm_date-1 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, blood_date) %>% 
  mutate(day = as.numeric(blood_date - adm_date) + 1) %>% 
  select(grid, adm_id, adm_date, dc_date, blood_date, day) 

#> go through abx and blood culture data to find qualifying admission ----------
infections <- sqldf::sqldf('SELECT t1.grid, t1.adm_id, t1.adm_date, t1.dc_date, blood_date, drug_date
                           FROM abx_distinct as t1
                           INNER JOIN blood_raw2 as t2
                           ON t1.grid = t2.grid AND t1.adm_id = t2.adm_id AND blood_date BETWEEN drug_date-3 AND drug_date + 1') %>% 
  as_tibble() %>% 
  mutate(onset_date = pmin(blood_date, drug_date),
         onset_day = as.numeric(onset_date - adm_date + 1)) %>% 
  group_by(grid, adm_id, adm_date, dc_date) %>% 
  summarise(onset_date = min(onset_date), onset_day = min(onset_day)) %>% 
  ungroup()
infections %>% 
  count(onset_day)
infections %>% 
  distinct(grid, adm_id) 
## we decided to limit onset date 1 day within hospital admission date
infections_w1d <- infections %>% 
  filter(onset_day %in% 0:2)
infections_w1d %>% 
  count(onset_day)
#' 36,285 infections; 24,827 within 1 day of admission date
nrow(infections)/nrow(cam_visits)
nrow(infections_w1d)/nrow(cam_visits)


#. calculate SOFA score -------------------------------------------------------
#> liver-blirubin ---------------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/Bilirubin",
                         full.names = T)

#>> out of range value ------------------------
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
##>> normal value ---------------------
for (file in file_names) {
  bilirubin_raw <- bilirubin_raw %>% 
    bind_rows(read_excel(file, sheet = 1))
}
Hmisc::describe(bilirubin_raw)
names(bilirubin_raw) <- tolower(names(bilirubin_raw))

##>> convert messed-up GRIDs and dates -------------
length(unique(bilirubin_raw$grid))
sum(unique(bilirubin_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(bilirubin_raw$grid) %in% unique(changed_grid$old_grid))
sum(unique(bilirubin_raw$grid) %in% unique(changed_grid$updated_grid))
#!!! Now we have NEW updated GRID, will need their DOB to make sure dates are correct
# no need to convert, but will correct later.
bilirubin_raw %>% 
  distinct(grid, lab_date)
ggplot(bilirubin_raw) +
  geom_histogram(aes(x = `tbil (mg/dl)`))

#>> take worst/maximum bilirubin for each day and infection --------------
#' For indentify sepsis purpose, not necessary to get daily status
bilirubin_daily <- bilirubin_raw %>% 
  filter(!is.na(`tbil (mg/dl)`)) %>% 
  mutate(lab_date = as_date(lab_date)) %>% 
  group_by(grid, lab_date) %>% 
  summarise(bilirubin = max(`tbil (mg/dl)`)) %>% 
  ungroup()
bilirubin_infection <-  sqldf::sqldf('SELECT * 
                                     FROM infections_w1d as t1
                                     INNER JOIN bilirubin_daily as t2 
                                     ON t1.grid = t2.grid AND lab_date BETWEEN onset_date-2 AND onset_date+1') %>% 
  as_tibble() %>% 
  select(-grid..7) %>% 
  group_by(grid, adm_id) %>% 
  summarise(bilirubin = max(bilirubin)) %>% 
  ungroup() %>% 
  mutate(
    sofa_liver = case_when(
      bilirubin < 1.2 ~ 0,
      bilirubin < 2.0 ~ 1,
      bilirubin < 6.0 ~ 2,
      bilirubin <= 12.0 ~ 3,
      bilirubin > 12.0 ~ 4
    )
  )
bilirubin_infection %>% 
  Hmisc::describe()


#> coagulation: platelets --------------------------------------------------
#>> out of range value ------------------------
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
#>> normal value -------------------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/Platelet",
                         pattern = "labs.xlsx$",
                         full.names = T)
for (file in file_names) {
  platelet_raw <- platelet_raw %>% 
    bind_rows(read_excel(file))
}
Hmisc::describe(platelet_raw)
names(platelet_raw) <- tolower(names(platelet_raw))

##>> convert messed-up GRIDs and dates -------------
length(unique(platelet_raw$grid))
sum(unique(platelet_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(platelet_raw$grid) %in% unique(changed_grid$old_grid))
sum(unique(platelet_raw$grid) %in% unique(changed_grid$updated_grid))
#!!! Now we have NEW updated GRID, will need their DOB to make sure dates are correct
# no need to convert, but will correct later.
platelet_raw %>% 
  distinct(grid, lab_date)
ggplot(platelet_raw) +
  geom_histogram(aes(x = `plt-ct (thou/ul)`))
## take worst/maximum platelet for each day and infection
platelet_daily <- platelet_raw %>% 
  mutate(lab_date = as_date(lab_date)) %>% 
  group_by(grid, lab_date) %>% 
  summarise(platelet = min(`plt-ct (thou/ul)`)) %>% 
  ungroup()
platelet_infection <-  sqldf::sqldf('SELECT * 
                                    FROM infections_w1d as t1
                                    INNER JOIN platelet_daily as t2 
                                    ON t1.grid = t2.grid AND lab_date BETWEEN onset_date-2 AND onset_date+1') %>% 
  as_tibble() %>% 
  select(-grid..7) %>% 
  group_by(grid, adm_id) %>% 
  summarise(platelet = min(platelet)) %>% 
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
platelet_infection %>% 
  Hmisc::describe()

#> central nervous system: RASS -----------------------------------
#>> convert messed-up GRIDs and dates -------------
sum(unique(rass_raw$grid) %in% unique(changed_grid$old_grid))
sum(unique(rass_raw1$grid) %in% unique(changed_grid$updated_grid))
rass_raw1 <- rass_raw %>% 
  left_join(changed_grid, by = c("grid" = "old_grid")) %>% 
  rename(rass_time = rass_date) %>% 
  mutate(
    dttm = case_when(
      dob != dummy_dob ~ mdy_hms(rass_time) - as_datetime(dob) + as_datetime(dummy_dob),
      T ~ mdy_hms(rass_time)),
    rass_date = as_date(dttm),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, dttm, rass_date, rass_score, rass_score_test_name) %>% 
  arrange(grid, dttm)
sum(rass_raw1$grid %in% changed_grid$old_grid)
## can ignore duplicated RASS assessed at the same time, becasue we are taking worst daily value
rass_raw1 %>% distinct(grid, rass_date)
rass_daily <- rass_raw1 %>% 
  filter(rass_score %in% -5:4) %>% 
  select(-rass_score_test_name) %>% 
  distinct() %>% 
  group_by(grid, rass_date) %>% 
  summarise(rass = min(rass_score)) %>% 
  ungroup()
rass_daily %>% count(rass)
rass_infection <-  sqldf::sqldf('SELECT * 
                                FROM infections_w1d as t1
                                INNER JOIN rass_daily as t2 
                                ON t1.grid = t2.grid AND rass_date BETWEEN onset_date-2 AND onset_date+1') %>% 
  as_tibble() %>% 
  select(-grid..7) %>% 
  group_by(grid, adm_id) %>% 
  summarise(rass = min(rass)) %>% 
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
rass_infection %>% 
  Hmisc::describe()

#> Cardiovascular: pressor --------------------
pressor_raw <- med_raw %>% 
  filter(drug_class == "pressor") %>% 
  select(-drug_class) 
pressor_raw %>% 
  count(drug_name)

#>> convert messed-up GRIDs and dates -------------------
length(unique(pressor_raw$grid))
sum(unique(pressor_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(pressor_raw$grid) %in% unique(changed_grid$old_grid))
sum(unique(pressor_raw$grid) %in% unique(changed_grid$updated_grid))


pressor_raw1 <- pressor_raw %>% 
  left_join(changed_grid, by = c("grid" = "old_grid")) %>% 
  mutate(
    drug_date = case_when(
      dob != dummy_dob ~ mdy(drug_date) - dob + dummy_dob,
      T ~ mdy(drug_date)),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, drug_date, drug_name, drug_route1, drug_route2, drug_route3) %>% 
  arrange(grid, drug_date) # one duplicate
sum(pressor_raw1$grid %in% changed_grid$old_grid)
changed_grid %>% 
  filter(dob != dummy_dob, old_grid %in% pressor_raw$grid) 
pressor_raw1 %>% filter(grid == "R200032573") %>% 
  select(grid, drug_date, drug_name) %>% 
  print(n = 40)
pressor_raw %>% 
  filter(grid %in% c("R272295989", "R200032573")) %>% 
  select(grid, drug_date, drug_name) %>% 
  print(n = 40)
pressor_raw %>% 
  distinct(grid, drug_date)

## take worst SOFA for each infection
pressor_infection <-  sqldf::sqldf('SELECT * 
                                    FROM infections_w1d as t1
                                    INNER JOIN pressor_raw1 as t2 
                                    ON t1.grid = t2.grid AND drug_date BETWEEN onset_date-2 AND onset_date+1') %>% 
  as_tibble() %>% 
  select(grid, adm_id, drug_name) %>%
  mutate(sofa_cardio = case_when(
    drug_name %in% c("DOBUTAMINE", "DOPAMINE") ~ 2,
    drug_name %in% c("EPINEPHRINE", "NOREPINEPHRINE") ~ 3)
    ) %>% 
  group_by(grid, adm_id) %>% 
  summarise(sofa_cardio = max(sofa_cardio))  %>% 
  ungroup()  


#> merge SOFA scores together ---------------
sepsis <- infections_w1d %>% 
  left_join(
    bilirubin_infection,
    by = c("grid", "adm_id")
  ) %>% 
  left_join(
    platelet_infection,
    by = c("grid", "adm_id")
  ) %>% 
  left_join(
    rass_infection,
    by = c("grid", "adm_id")
  ) %>% 
  left_join(
    pressor_infection,
    by = c("grid", "adm_id")
  ) %>% 
  mutate(sofa = case_when(
    is.na(sofa_liver) + is.na(sofa_coagulation) + is.na(sofa_cns) + is.na(sofa_cardio) != 4 ~
    coalesce(sofa_liver, 0) + coalesce(sofa_coagulation, 0) + 
           coalesce(sofa_cns, 0) + coalesce(sofa_cardio, 0)),
    data_type = case_when(
      is.na(sofa_liver) + is.na(sofa_coagulation) + is.na(sofa_cns) + is.na(sofa_cardio) == 4 ~ "All missing",
      is.na(sofa_liver) + is.na(sofa_coagulation) + is.na(sofa_cns) + is.na(sofa_cardio) > 1 ~ "Missing > 1 system",
      is.na(sofa_liver) ~ "No Liver SOFA",
      is.na(sofa_coagulation) ~ "No Coagulation SOFA",
      is.na(sofa_cns) ~ "No CNS SOFA",
      is.na(sofa_cardio) ~ "No Cardio SOFA",
      is.na(sofa_liver) + is.na(sofa_coagulation) + is.na(sofa_cns) + is.na(sofa_cardio) == 0 ~ "Complete data"
    )
  )
sepsis %>% 
  Hmisc::describe()
sepsis %>% 
  filter(is.na(sofa_liver) + is.na(sofa_coagulation) + is.na(sofa_cns) + is.na(sofa_cardio) != 4) %>% 
  select(grid, adm_id, starts_with("sofa"))
summary(sepsis$sofa)
ggplot(sepsis) +
  geom_histogram(aes(x = sofa), binwidth = 1) +
  labs(title = "Distribution of SOFA score") 

sepsis %>% 
  select(sofa_coagulation, sofa_liver, sofa_cardio, sofa_cns) %>% 
  sapply(function(x) percent(sum(is.na(x))/length(x))) 
ggplot(sepsis) +
  geom_bar(aes(x = factor(1), fill = data_type)) +
  coord_polar("y") +
 # scale_fill_brewer(palette="Blues") +
  theme_minimal()
sepsis %>% 
  count(data_type) %>% 
  mutate(pct = percent(n/nrow(sepsis), accuracy = 0.01))
sepsis %>% 
  filter(sofa < 2) %>% 
  count(data_type) %>% 
  mutate(pct = percent(n/nrow(sepsis), accuracy = 0.01))
