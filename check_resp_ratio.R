## 1. To calculate PaO2/FiO2 and compare with the available ratio data
## decided to use calculated PaO2/FiO2 ratio
## 2. To calculate SpO2/FiO2

library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
#library(scales)
changed_grid <- read_csv("../output/changed_grid_dob_20190924.csv")


#> Ratio data ---------------------------------------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/PO2_FI02_ratio",
                         pattern = ".xlsx$",
                         full.names = T)
ratio_raw <- NULL
for (file in file_names) {
  ratio_raw <- ratio_raw %>% 
    bind_rows(read_excel(file))
}
names(ratio_raw) <- str_to_lower(names(ratio_raw))
ratio_raw_min <- ratio_raw %>% 
  distinct() %>% 
  mutate(lab_date = as_date(lab_date)) %>% 
  group_by(grid, lab_date, lab_time) %>% 
  summarise(`po2/fi (mmhg)` = min(`po2/fi (mmhg)`)) %>% 
  ungroup() 
  
Hmisc::describe(ratio_raw)



#> FIO2 data ------------------------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/FIO2",
                         pattern = ".xlsx$",
                         full.names = T)
fio2_raw <- NULL
for (file in file_names) {
  fio2_raw <- fio2_raw %>% 
    bind_rows(read_excel(file))
}
names(fio2_raw) <- str_to_lower(names(fio2_raw))
fio2_raw %<>% distinct() 
Hmisc::describe(fio2_raw)

#> PO2 data ------------------------------------
#>> out of range value ------------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/Arterial pO2",
                         pattern = ".xlsx$",
                         full.names = T)
po2_oor <- NULL
for (file in file_names) {
  po2_oor <- po2_oor %>% 
    bind_rows(read_excel(file, sheet = 2))
}
Hmisc::describe(po2_oor) #650 rows
po2_raw <- po2_oor %>% 
  rename(oor_value = `Arterial pO2 mmHg`) %>% 
  mutate(`Arterial pO2 mmHg` = as.numeric(str_extract(oor_value, "[[:digit:]]+")))
po2_raw %>% 
  distinct(oor_value, `Arterial pO2 mmHg`)
# only two oor values <30, > 800
#>> normal value --------------------
for (file in file_names) {
  po2_raw <- po2_raw %>% 
    bind_rows(read_excel(file))
}
names(po2_raw) <- str_to_lower(names(po2_raw))
po2_raw %<>% 
  distinct() %>% 
  select(-oor_value) %>% 
  mutate(lab_date = as_date(lab_date))
Hmisc::describe(po2_raw)

#> check data --------------------------------
ratio_raw %>% filter(`po2/fi (mmhg)` == 0)
ratio_raw %>% 
  ggplot(aes(x = `po2/fi (mmhg)`)) +
  geom_histogram() +
  scale_x_log10() # two peaks


fio2_raw %>% 
  filter(fio2 <= 100) %>% 
  ggplot(aes(x = fio2)) +
  geom_histogram() +
  scale_x_log10() # two/three peaks? i
fio2_raw %>% 
  filter(fio2 > 100)
fio2_raw %>% 
  filter(fio2 <= 0)
fio2_raw %>% 
  filter(fio2 <= 1) %>% 
  ggplot(aes(x = fio2)) +
  geom_histogram() 
fio2_raw %>% 
  filter(between(fio2, 1.1, 100)) %>% 
  ggplot(aes(x = fio2)) +
  geom_histogram()
fio2_raw %>% 
  filter(fio2 <= 1) %>% 
  summary()
fio2_raw %>% 
  filter(between(fio2, 1.1, 100)) %>% 
  summary()

# convert everything to 0-1 scale.
fio2_raw1 <- fio2_raw %>% 
  filter(fio2 <= 100) %>% 
  mutate(fio2_c = if_else(fio2 > 1, fio2/100, fio2),
         lab_date = as_date(lab_date)) 



po2_raw %>% 
  summary()
po2_raw %>% 
  filter(`arterial po2 mmhg` < 30)
po2_raw %>% 
  ggplot(aes(x = `arterial po2 mmhg`)) +
  geom_histogram()
po2_raw %>% 
  filter(`arterial po2 mmhg` > 0) %>% 
  summary()


sum(unique(fio2_raw$grid) %in% changed_grid$old_grid)
sum(unique(po2_raw$grid) %in% changed_grid$old_grid)

fio2_raw %>% 
  distinct()
po2_raw %>% 
  distinct()
ratio_calc <- po2_raw %>% 
  inner_join(fio2_raw1) %>% 
  rename(po2 = `arterial po2 mmhg`) %>% 
  mutate(ratio_c = round(po2/fio2_c, 0)) %>% 
  group_by(grid, lab_date, lab_time) %>% 
  filter(ratio_c == min(ratio_c)) %>% 
  ungroup() %>% 
  distinct()
ratio_calc %>% 
  distinct(grid, lab_date, lab_time)
ratio_calc %>% 
  group_by(grid, lab_date, lab_time) %>% 
  filter(n() > 1) %>% 
  print(n = 60)
ratio_calc %<>% 
  group_by(grid, lab_date, lab_time) %>% 
  filter(fio2 == min(fio2)) %>% 
  ungroup()
ratio_calc %>% Hmisc::describe()

ratio_calc %>% 
  filter(fio2_c < 0.21) %>% 
  Hmisc::describe() # for FiO2 < 0.21, 95% are < 0.1
ratio_calc %>% 
  filter(fio2_c >= 0.21)

ratio_comp <- ratio_raw_min %>% 
  full_join(ratio_calc)
ratio_comp %>% 
  filter(abs(`po2/fi (mmhg)` - ratio_c) <= 1) 
ratio_comp %>% 
  filter(abs(`po2/fi (mmhg)` - ratio_c) > 1) 
ratio_comp %>% 
  filter(abs(`po2/fi (mmhg)` - ratio_c) > 1, fio2 <= 1) %>% 
  print(n = 50)
ratio_comp %>% 
  filter(abs(`po2/fi (mmhg)` - ratio_c) > 1, fio2 > 1) %>% 
  print(n = 50) 
# The raw ratio value from EHR is not very accurate, did not convert FiO2 > 1 to FiO2 < 1
# will us calculated ratio

ratio_comp %>% 
  filter(is.na(`po2/fi (mmhg)`)) %>% 
  print(n = 100)
ratio_comp %>% 
  filter(is.na(ratio_c)) %>% 
  print(n = 100) # the extra value don't seem to be of much value.
ratio_comp %>% 
  filter(ratio_c == Inf) %>% 
  print(n = 100)
ratio_calc$ratio_c[ratio_calc$ratio_c == Inf] <- NA 
ratio_calc %>% 
     summary()
ratio_calc %>% 
  filter(ratio_c < 10)
ratio_calc %>% 
  filter(ratio_c > 1000)
ratio_calc %>% 
  filter(fio2_c < 0.1)
ratio_calc %>% 
  ggplot(aes(x = ratio_c)) +
  geom_histogram() +
  scale_x_log10()
  


write_csv(ratio_calc, "../output/pao2_fio2_ratio_calc_20190927.csv")

#> Nasal O2 data -------------------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/Nasal_O2_rate/",
                         pattern = ".xlsx$",
                         full.names = T)
nasal_o2_raw <- NULL
for (file in file_names) {
  if (file != file_names[8]) {
    nasal_o2_raw %<>% 
      bind_rows(read_excel(file))
  } else {
    # set 6 file has different lab time from other (AM/PM instead of 24 hours)
    nasal_o2_raw %<>% 
      bind_rows(read_excel(file) %>% 
                  mutate(LAB_TIME = str_c(str_sub(LAB_TIME, -8, -1), ".0"))
                )
  }
}
names(nasal_o2_raw) <- str_to_lower(names(nasal_o2_raw))
nasal_o2_raw %<>% distinct() %>% 
  mutate(lab_date = as_date(lab_date))
Hmisc::describe(nasal_o2_raw)
ggplot(nasal_o2_raw) +
  geom_histogram(aes(x = `naslo2 (l)`)) +
  scale_x_log10()
nasal_o2_raw %>% 
  filter(`naslo2 (l)` > 100)

# merge with FiO2 values to check
nasal_o2_check <- fio2_raw1 %>% 
  inner_join(nasal_o2_raw)
nasal_o2_check %>% 
  filter(fio2 == `naslo2 (l)`) %>% 
  print(n = 100)
# only 93 entries
fio2_raw1 %>% 
  filter(fio2_c < 0.21)
93/2199
# Only 4% of the < 21 FiO2 values had a matching Nasal values 


#> O2 saturation data ------------------------------------
#>> out of range value ------------------------
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/O2Sat",
                         pattern = ".xlsx$",
                         full.names = T)
so2_oor <- NULL
for (file in file_names) {
  so2_oor <- so2_oor %>% 
    bind_rows(read_excel(file, sheet = 2))
}
Hmisc::describe(so2_oor) #27 rows
so2_raw <- so2_oor %>% 
  rename(oor_value = `O2Sat %`) %>% 
  mutate(`O2Sat %` = as.numeric(str_extract(oor_value, "[[:digit:]]+")))
so2_raw %>% 
  distinct(oor_value, `O2Sat %`)
# only three oor values >99, >100, >110
#>> normal value --------------------
for (file in file_names) {
  so2_raw <- so2_raw %>% 
    bind_rows(read_excel(file))
}
names(so2_raw) <- str_to_lower(names(so2_raw))

# check values

so2_raw %<>% 
  distinct() %>% 
  select(-oor_value) %>% 
  mutate(lab_date = as_date(lab_date),)
Hmisc::describe(so2_raw)

so2_raw %>% 
  filter(`o2sat %` > 100.) %>% 
  count(`o2sat %`) %>% 
  print(n = 30)



#> calculate SF ratio --------------------------------
sf_ratio <- so2_raw %>% 
  rename(so2 = `o2sat %`) %>% 
  mutate(so2_c = if_else(so2 > 100, 100, so2)) %>% 
  inner_join(fio2_raw1) %>% 
  mutate(ratio_c = round(so2_c/fio2_c, 0)) %>% 
  group_by(grid, lab_date, lab_time) %>% 
  filter(ratio_c == min(ratio_c)) %>% 
  ungroup() %>% 
  distinct() %>% 
  filter(ratio_c != Inf)
sf_ratio %>% 
  filter(ratio_c > 10000)
Hmisc::describe(sf_ratio)
write_csv(sf_ratio, "../output/spo2_fio2_ratio_calc_20191010.csv")


#. check why missing -----------------------

infections <- read_csv("../output/sepsis3_all_infections_20190927.csv")
infections_w1d <- infections %>% 
  filter(onset_day %in% 0:2)
ratio_raw <- po2_raw %>% 
  full_join(fio2_raw1) %>% 
  full_join(so2_raw) %>% 
  full_join(nasal_o2_raw)
ratio_raw1 <- sqldf::sqldf('SELECT * 
                               FROM infections_w1d as t1
                               LEFT JOIN ratio_raw as t2 
                               ON t1.grid = t2.grid AND lab_date <= dc_date AND lab_date BETWEEN onset_date-2 AND onset_date+1') %>% 
  as_tibble() %>% 
  select(-grid..7) 
ratio_raw1 %>%
  select(`arterial po2 mmhg`:`naslo2 (l)`) %>% 
  Hmisc::describe() # fio2 had the most missing, then o2sat, then po2
l1 <- ratio_raw1 %>% 
  filter(!is.na(`arterial po2 mmhg`)) %>% 
  distinct(grid, adm_id) #9736 encounters
l2 <- ratio_raw1 %>% 
  filter(!is.na(fio2)) %>% 
  distinct(grid, adm_id) #7041 encounters
l3 <- ratio_raw1 %>% 
  filter(!is.na(`o2sat %`)) %>% 
  distinct(grid, adm_id) #7369 encounters
l4 <- ratio_raw1 %>% 
  filter(!is.na(`naslo2 (l)`)) %>% 
  distinct(grid, adm_id) #1229 encounters

l3 %>% dplyr::setdiff(l1)
ratio_raw1 %>% filter(grid == "R243497431", adm_id == 1)
c(9736, 7041, 7369)/24827

ratio_raw1 %>% 
  filter(!is.na(`naslo2 (l)`), is.na(fio2), !is.na(`arterial po2 mmhg`) | !is.na(`o2sat %`), 
         `naslo2 (l)` < 10) %>% 
  select(grid, adm_id, onset_date, lab_date, `arterial po2 mmhg`:`naslo2 (l)`) %>% 
  distinct(grid, adm_id)# will eliminate missing value for ~900 encounters 

# check FiO2_c < 0.21 that get into SOFA calculation
# only need to check the max FiO2 b/c higher FiO2,  higher SOFA score.
ratio_raw1 %>% 
  group_by(grid, adm_id) %>% 
  summarise(fio2_c = max(fio2_c, na.rm = T)) %>% 
  filter(fio2_c < 0.21, fio2_c >= 0)
# Only up to 40 encounters, and if this has any effect, will miss a few sepsis.