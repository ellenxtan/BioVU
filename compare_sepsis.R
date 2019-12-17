# 1. compare sepsis
# 2. find distinct GRIDs with sepsis and see which ones have genotype data
# 3. check the visits with a sepsis code but negative for both sepsis definitions

cam_visits <- read_csv("../output/cam_stay_20190925.csv") 
sepsis3 <- read_csv("../output/sepsis3_20191014.csv")
rhee_sepsis <- read_csv("../output/sepsis_rhee_20191217.csv")

#. Find CAM-ICU admissions with sepsis code ------------------------------------
sepsis_code_raw <- read_excel("../../Mito Delirium BioVU Data/Phenotype data/Sepsis Code Days.xlsx") %>% 
  mutate(`sepsis CODE_DATE` = as_date(`sepsis CODE_DATE`))

names(sepsis_code_raw) <- str_to_lower(names(sepsis_code_raw))

#> convert messed-up GRIDs and dates ---
length(unique(sepsis_code_raw$grid))
sum(unique(sepsis_code_raw$grid) %in% unique(static_raw$grid))  # check whether any new updated GRID
sum(unique(sepsis_code_raw$grid) %in% unique(changed_grid$old_grid)) # 170
sum(unique(sepsis_code_raw$grid) %in% unique(changed_grid$updated_grid)) # 181

sepsis_code_raw1 <- sepsis_code_raw %>% 
  left_join(changed_grid, by = c("grid" = "old_grid")) %>% 
  mutate(
    code_date = if_else(!is.na(updated_grid),
                        `sepsis code_date` - old_dob + updated_dob,
                        `sepsis code_date`),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, code_date) %>% 
  arrange(grid, code_date)
sum(sepsis_code_raw1$grid %in% changed_grid$old_grid)

sepsis_code <- sqldf::sqldf('SELECT *
                            FROM cam_visits as t1
                            INNER JOIN sepsis_code_raw1 as t2
                            ON t1.grid = t2.grid AND code_date BETWEEN adm_date-1 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, code_date) %>% 
  mutate(sepsis_day = as.numeric(code_date - adm_date + 1)) %>% 
  group_by(grid, adm_id) %>% 
  summarise(sepsis_code_adm = 1,
            sepsis_code_w1d = if_else(sum(sepsis_day %in% 0:2) > 0, 1, 0)) %>% 
  ungroup()
sepsis_code %>% 
  Hmisc::describe()

#. compare rhee, sepsis3 and ICD9 code -------------------------------
rhee_sepsis %>% count(rhee)
sepsis3 %>% count(sepsis3)
sepsis_comp <- cam_visits %>% 
  select(grid, adm_id) %>% 
  left_join(sepsis3 %>% 
              select(grid, adm_id, sofa, sepsis3, data_type)) %>% 
  left_join(rhee_sepsis %>% 
              select(grid, adm_id, rhee)) %>%  
  left_join(sepsis_code) %>%
  mutate(sepsis3_infection = if_else(is.na(sepsis3), 0, 1),
         sepsis3 = if_else(is.na(sepsis3), 0, sepsis3),
         rhee_infection = if_else(is.na(rhee), 0, 1),
         rhee = if_else(is.na(rhee), 0, rhee),
         sepsis_code_adm = if_else(is.na(sepsis_code_adm), 0, 1),
         sepsis_code_w1d = if_else(sepsis_code_w1d %in% 1, 1, 0)
  )
sepsis_comp %>% Hmisc::describe()

xtabs(~ sepsis3_infection + rhee_infection, data = sepsis_comp, addNA = T) # All rhee infections are sepsis3 infections
xtabs(~ sepsis3 + rhee, data = sepsis_comp, addNA = T)
xtabs(~ sepsis_code_adm + rhee, data = sepsis_comp, addNA = T)
xtabs(~ sepsis_code_w1d + rhee, data = sepsis_comp, addNA = T)
xtabs(~ sepsis_code_adm + sepsis3, data = sepsis_comp, addNA = T)
xtabs(~ sepsis_code_w1d + sepsis3, data = sepsis_comp, addNA = T)
sepsis_comp %>% 
  filter(rhee_infection == 1) %>% 
  with(xtabs(~sepsis3 + rhee))
sepsis_comp %>% filter(rhee == 1, sepsis3 == 0) %>% count(data_type)

write_csv(sepsis_comp, "../output/sepsis_compare_20191217.csv")


#. Get distinct GRID ---------------------
dob_dis <- read_csv("../output/dob_discrepancy.csv") 
sepsis_comp %<>% anti_join(dob_dis, by = "grid")
rhee_sepsis %<>% anti_join(dob_dis, by = "grid")
rhee_grid <- sepsis_comp %>% 
  filter(rhee == 1) %>% 
  distinct(grid)
rhee_sepsis3_grid <- sepsis_comp %>% 
  filter(rhee == 1 | sepsis3 == 1) %>% 
  distinct(grid)
rhee_sepsis3_code_grid <- sepsis_comp %>% 
  filter(rhee == 1 | sepsis3 == 1 | sepsis_code_w1d == 1) %>% 
  distinct(grid)
openxlsx::write.xlsx(list(rhee_grid, rhee_sepsis3_grid, rhee_sepsis3_code_grid),
                     file = "../output/sepsis_grids_20191204.xlsx",
                     sheetName = c("Rhee only", "Rhee or Sepsis-3", "Rhee or Sepsis-3 or Sepsis code"))

## check sepsis-code positive other negative -------------
sepsis_comp %>% 
  filter(sepsis_code_w1d == 1, sepsis3 != 1, rhee != 1) %>% 
  count(sepsis3_infection, rhee_infection)
#' 240 had both rhee and sepsis-3 infecgtion, 99 had sepsis-3 infection only
sepsis_comp %>% 
  filter(sepsis_code_w1d == 1, sepsis3 != 1, rhee != 1, sepsis3_infection == 1) %>% 
  count(data_type)
sepsis_comp %>% 
  filter(sepsis_code_w1d == 1, sepsis3 != 1, rhee != 1, rhee_infection == 1) 
rhee_sepsis %>% 
  semi_join(
    sepsis_comp %>% 
      filter(sepsis_code_w1d == 1, sepsis3 != 1, rhee != 1, rhee_infection == 1),
    by = c("grid", "adm_id")
  ) %>% 
  print(n = 300)
