# -----------------------------------------------------------------------------#
# 1. compare sepsis
# 2. find distinct GRIDs with sepsis and see which ones have genotype data
# 3. check the visits with a sepsis code but negative for both sepsis definitions
# -----------------------------------------------------------------------------#

library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)

#. compare rhee, sepsis3 and ICD9 code -------------------------------------------------------------
cam_visits <- read_csv("../output/cam_stay_20190925.csv") 
sepsis3 <- read_csv("../output/sepsis3_20191014.csv")
rhee_sepsis <- read_csv("../output/sepsis_rhee_20191217.csv")
changed_grid <- read_csv("../output/changed_grid_dob_20190924.csv")
static_raw <- read_csv("../../Mito Delirium BioVU Data/Data/Samuels_Delirium_STATIC_20180718.csv") 
names(static_raw) <- str_to_lower(names(static_raw))

#> Find CAM-ICU admissions with sepsis code ------------------------------------
sepsis_code_raw <- read_excel("../../Mito Delirium BioVU Data/Phenotype data/Sepsis Code Days.xlsx") %>% 
  mutate(`sepsis CODE_DATE` = as_date(`sepsis CODE_DATE`))

names(sepsis_code_raw) <- str_to_lower(names(sepsis_code_raw))

#>> convert messed-up GRIDs and dates ---
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

## The new genotype_status had the same DOB as static raw, so will just ignore DOB discrepancy in demo.
# #> remove GRIDs who had discrepancy in DOB > 10 days ----------------
# dob_dis <- read_csv("../output/dob_discrepancy.csv") 
# cam_visits %<>% anti_join(dob_dis, by = "grid")
# sepsis3 %<>% anti_join(dob_dis, by = "grid")
# rhee_sepsis %<>% anti_join(dob_dis, by = "grid")
# sepsis_code %<>% anti_join(dob_dis, by = "grid")

#> combine and compare -------------------------------
rhee_sepsis %>% count(rhee)
sepsis3 %>% count(sepsis3)
sepsis_comp <- cam_visits %>% 
  select(grid, adm_id, adm_date, dc_date) %>% 
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


#. Get distinct GRIDs and their genotype status ---------------------------------
#> GRIDs with either Rhee or sepsis code.
rhee_code_grid <- sepsis_comp %>% 
  filter(rhee == 1 | sepsis_code_w1d == 1) %>% 
  group_by(grid) %>% 
  summarise(
    rhee = if_else(sum(rhee) > 0, 1, 0),
    sepsis_code = if_else(sum(sepsis_code_w1d) > 0, 1, 0),
    sepsis3 = if_else(sum(sepsis3) > 0, 1, 0)
    )

#> genotype status --------------------------
genotype_status <- read_excel("../../Mito Delirium BioVU Data/Demographics/Sample_Genotyping_Status.xlsx")
names(genotype_status) <- str_to_lower(names(genotype_status))
genotype_status %>% Hmisc::describe()
## check the data with problematic date
genotype_status %>% 
  slice(24823:24826) %>% 
  select(grid, dob)
sepsis_comp %>% 
  filter(grid == "R225349237")
genotype_status %<>% 
  mutate(dob = if_else(is.na(dob), ymd(18990412), as_date(dob)))

# check messed up grid
changed_grid <- read_csv("../output/changed_grid_dob_20190924.csv")
sum(genotype_status$grid %in% changed_grid$old_grid) # NO old grid
# compare with GRIDs in CAM-ICU visits
sum(genotype_status$grid %in% sepsis_comp$grid)
sepsis_comp %>% distinct(grid)
#' all GRID in genotype_status are in demo_raw and static_raw, see changed_grid_dob.R


# grid not in genotype_status
grid_not_in_genotype <- sepsis_comp %>% 
  distinct(grid) %>% 
  anti_join(genotype_status, by = "grid") %>% 
  left_join(rhee_code_grid, by = "grid") %>% 
  mutate(sepsis = if_else(!is.na(rhee) , 1, 0)) %>% 
  select(grid, sepsis)
grid_not_in_genotype %>% 
  count(sepsis)

write_csv(grid_not_in_genotype, "../output/grid_not_in_genotype_status.csv")
 
# check other variables
genotype_status %>% 
  count(`in biovu`, category)
genotype_status %>% 
  count(`have mega`, category)
genotype_status %>% 
  count(`compromised blood sample`, category)
with(genotype_status, xtabs(~ category + `in biovu`))
with(genotype_status, xtabs(~ category + `have mega`))
with(genotype_status, xtabs(~ category + `have haplogroup`))
with(genotype_status, xtabs(~ category + `compromised blood sample`))
with(genotype_status, xtabs(~ `have mega` + `in biovu`))  # only samples in BioVU have mega 
with(genotype_status, xtabs(~ `have haplogroup` + `in biovu`)) # samples can have haplogroup regardless whether they are in BioVU
with(genotype_status, xtabs(~ `compromised blood sample` + `in biovu`)) # samples not in BioVU don't have the status of compromised blood sample

#> get genotype status for each grid -----------------
rhee_code_grid %<>% 
  left_join(genotype_status, by = "grid")
all_grid <- sepsis_comp %>% 
  group_by(grid) %>% 
  summarise(
    n_cam_icu_encounter = n(), 
    n_rhee = sum(rhee),
    n_sepsis_code = sum(sepsis_code_w1d),
    n_sepsis3 = sum(sepsis3)) %>% 
  mutate(
    sepsis = case_when(
      n_rhee > 0 | n_sepsis_code > 0 ~ "Rhee or Sepsis code",
      n_sepsis3 > 0 ~ "Sepsis-3 only",
      n_rhee == 0 & n_sepsis_code == 0 & n_sepsis3 == 0 ~ "No Sepsis")
    ) %>% 
  left_join(genotype_status, by = "grid")

rhee_code_grid %>% Hmisc::describe()
all_grid %>% Hmisc::describe()

rhee_code_grid %>% count(category)
all_grid %>% count(category)
with(all_grid, addmargins(xtabs( ~ category + sepsis, addNA = T)))

openxlsx::write.xlsx(list(rhee_code_grid, all_grid),
                     file = "../output/sepsis_grids_20191218.xlsx",
                     sheetName = c("Sepsis GRIDs (Rhee or code)", "All GRIDs"))




#. check sepsis-code only encounters ------------------------------ -------------
sepsis_comp %>% 
  filter(sepsis_code_w1d == 1, sepsis3 != 1, rhee != 1) %>% 
  count(sepsis3_infection, rhee_infection)
#' 242 had both rhee and sepsis-3 infecgtion, 99 had sepsis-3 infection only
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

#> check those encounters don't even have infection ----------------------------
sepsis_code_only <- sepsis_comp %>% 
  filter(sepsis_code_w1d == 1, rhee != 1, sepsis3 != 1, sepsis3_infection == 0, rhee_infection == 0) %>% 
  select(grid:dc_date)
# get sepsis code dates
sepsis_code_raw2 <- sqldf::sqldf('SELECT *
                            FROM cam_visits as t1
                            INNER JOIN sepsis_code_raw1 as t2
                            ON t1.grid = t2.grid AND code_date BETWEEN adm_date-1 AND dc_date') %>% 
  as_tibble() %>% 
  select(grid, adm_id, adm_date, dc_date, code_date) %>% 
  mutate(sepsis_day = as.numeric(code_date - adm_date + 1)) %>% 
  filter(sepsis_day %in% 0:2) 

sepsis_code_only <- sepsis_code_raw2 %>% 
  semi_join(sepsis_code_only)
  

# check whether these encounters had antibiotic administration
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


## convert messed-up GRIDs 
changed_grid <- read_csv("../output/changed_grid_dob_20190924.csv")
length(unique(abx_raw$grid))
sum(unique(abx_raw$grid) %in% unique(changed_grid$old_grid))
sum(unique(abx_raw$grid) %in% unique(changed_grid$updated_grid))
abx_raw %>% 
  count(drug_name) %>% 
  print(n = 100)
abx_raw %>% 
  count(drug_route1, drug_route2, drug_route3)
abx_raw %>% 
  filter(drug_name == 'VANCOMYCIN') %>% 
  count(drug_route1, drug_route2, drug_route3)


abx_raw1 <- abx_raw %>% 
  left_join(changed_grid, by = c("grid" = "old_grid")) %>% 
  mutate(
    drug_date = if_else(!is.na(updated_grid),
                        mdy(drug_date) - old_dob + updated_dob,
                        mdy(drug_date)),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, drug_date, drug_name, drug_route1, drug_route2, drug_route3) %>% 
  arrange(grid, drug_date)

# encounters with abx administration during hospital stay - 2633
sepsis_code_abx <- sqldf::sqldf('SELECT *
        FROM sepsis_code_only as t1
        left join abx_raw1 as t2
          ON t1.grid = t2.grid AND t2.drug_date BETWEEN t1.adm_date - 3 AND t1.dc_date') %>% 
  as_tibble() %>% 
  filter(!is.na(drug_name)) %>% 
  distinct(grid, adm_id, adm_date, dc_date)

# encounters with abx administration within 2 day window of sepsis code date - 1079
sepsis_code_abx_w2d <- sqldf::sqldf('SELECT *
        FROM sepsis_code_only as t1
        left join abx_raw1 as t2
          ON t1.grid = t2.grid AND t2.drug_date BETWEEN t1.code_date - 2 AND t1.code_date + 2') %>% 
  as_tibble() %>% 
  filter(!is.na(drug_name)) %>% 
  distinct(grid, adm_id, adm_date, dc_date)

abx_raw1 %>% filter(grid == "R200017241")
sepsis_code_only %>% filter(grid == "R200017241")

abx_raw1 %>% filter(grid == "R200021057")
sepsis_code_only %>% filter(grid == "R200021057")

abx_raw1 %>% filter(grid == "R200155544")
sepsis_code_only %>% filter(grid == "R200155544")

sepsis_code_only %>% 
  distinct(grid) %>% 
  semi_join(changed_grid, by = c("grid" = "updated_grid"))
