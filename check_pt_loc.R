library(tidyverse)
library(magrittr)
library(readxl)
file_names <- list.files("../../Mito Delirium BioVU Data/Lab values/patient_Location",
                         pattern = ".xlsx$",
                         full.names = T)
loc_raw <- NULL
for (file in file_names) {
  loc_raw <- loc_raw %>% 
    bind_rows(read_excel(file))
}
names(loc_raw) <- str_to_lower(names(loc_raw))
(loc_ct <- loc_raw %>% 
  distinct(grid, lab_date, patloc) %>% 
  count(patloc) %>% 
  arrange(desc(n)) %>% 
  print(n = 20))
write_csv(loc_ct, "../output/patient_location_count.csv")
