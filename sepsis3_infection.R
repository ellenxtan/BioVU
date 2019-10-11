# Identify suspected infection 

changed_grid <- read_csv("../output/changed_grid_dob_20190924.csv")
cam_visits <- read_csv("../output/cam_stay_20190925.csv") 
#> Data import and clean ---------------

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
    drug_date = if_else(!is.na(updated_grid),
                        mdy(drug_date) - old_dob + updated_dob,
                        mdy(drug_date)),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, drug_date, drug_name, drug_route1, drug_route2, drug_route3) %>% 
  arrange(grid, drug_date) # some duplicates
sum(abx_raw1$grid %in% changed_grid$old_grid)
changed_grid %>% 
  filter(updated_grid == "R200032573") 
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
    blood_date = if_else(!is.na(updated_grid),
                         as_date(`blood culture code_date`) - old_dob + updated_dob,
                         as_date(`blood culture code_date`)),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, blood_date) %>% 
  arrange(grid, blood_date) # some duplicates
sum(blood_raw1$grid %in% changed_grid$old_grid)
changed_grid %>% 
  filter(updated_grid == "R200032573") 
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
  # in sepsis-3 paper supp. it said "we identified only the first episode of suspected infection of each encounter"
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
## checked whether these 24827 
write_csv(infections, "../output/sepsis3_all_infections_20190927.csv")