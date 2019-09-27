## To calculate PaO2/FiO2 and compare with the available ratio data
## decided to use calculated PaO2/FiO2 ratio

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

