library(tidyverse)
library(magrittr)
library(readxl)

cam_visits <- read_csv("../output/cam_stay_20190925.csv") 
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/patient_Location",
                         pattern = ".xlsx$",
                         full.names = T)
loc_raw <- NULL
for (file in file_names) {
  loc_raw <- loc_raw %>% 
    bind_rows(read_excel(file))
}
names(loc_raw) <- str_to_lower(names(loc_raw))
loc_raw %<>% 
  mutate(lab_date = as_date(lab_date))


(loc_ct <- loc_raw %>% 
  distinct(grid, lab_date, patloc) %>% 
  count(patloc) %>% 
  arrange(desc(n)) %>% 
  print(n = 20))
# write_csv(loc_ct, "../output/patient_all_location_count.csv")

loc_cam_visit <- sqldf::sqldf('SELECT t1.adm_id, t2.*
              FROM cam_visits as t1
             INNER JOIN loc_raw as t2 
             ON t1.grid = t2.grid AND lab_date BETWEEN adm_date AND dc_date') %>% 
  as_tibble() 
loc_cam_visit %>% distinct(grid, adm_id)
82291/114897
(loc_ct <- loc_cam_visit %>% 
  distinct(grid, lab_date, patloc) %>% 
  count(patloc) %>% 
  arrange(desc(n)) %>% 
  print(n = 20))
write_csv(loc_ct, "../output/patient_cam_visit_location_count.csv")
