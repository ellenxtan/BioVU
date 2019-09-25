## create new daily/visit level data
## correct mssed-up ID, convert date by date - old_dob + updated_dob
library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
library(scales)


#. daily status for messed-up ID ----------------------------------------------
load("../output/data_raw.RData")
changed_grid_dob <- read_csv("../output/changed_grid_dob_20190924.csv")
changed_grid_dob %>% 
  filter(updated_grid == "R200022925")

#> CAM ---------------------
sum(unique(cam_raw$grid) %in% changed_grid$old_grid)

cam_raw1 <- cam_raw %>% 
  left_join(changed_grid_dob, by = c("grid" = "old_grid")) %>% 
  rename(cam_time = cam_date) %>% 
  mutate(
    dttm = if_else(!is.na(updated_grid),
                   mdy_hms(cam_time) - as_datetime(old_dob) + as_datetime(updated_dob),
                   mdy_hms(cam_time)),
    cam_date = as_date(dttm),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, dttm, cam_date, cam_value, cam_test_name) %>% 
  arrange(grid, dttm) # No exact duplicates
cam_icu <- cam_raw1 %>% 
  filter(cam_test_name == "CAM-ICU") %>% 
  select(grid, cam_date, dttm, cam_value)

sum(unique(cam_raw1$grid) %in% changed_grid$old_grid)
cam_raw %>% distinct(grid)
cam_raw1 %>% distinct(grid)
cam_raw %>% filter(grid %in% c("R275532955", "R200022925")) %>% print(n = 40)
cam_raw1 %>% filter(grid == "R200022925") %>% print(n = 40)

#> RASS -----------------------------
sum(unique(rass_raw$grid) %in% changed_grid$old_grid)

rass_raw1 <- rass_raw %>% 
  left_join(changed_grid_dob, by = c("grid" = "old_grid")) %>% 
  rename(rass_time = rass_date) %>% 
  mutate(
    dttm = if_else(!is.na(updated_grid),
                   mdy_hms(rass_time) - as_datetime(old_dob) + as_datetime(updated_dob),
                   mdy_hms(rass_time)),
    rass_date = as_date(dttm),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
    ) %>% 
  distinct(grid, dttm, rass_date, rass_score, rass_score_test_name) %>% 
  arrange(grid, dttm) # No exact duplicates

sum(rass_raw1$grid %in% changed_grid$old_grid)
rass_raw %>% distinct(grid)
rass_raw1 %>% distinct(grid)
rass_raw %>% filter(grid %in% c("R200022925", "R275532955")) %>% print(n = 70)
rass_raw1 %>% filter(grid == "R200022925") %>% print(n = 70)



#> reduce RASS duplication ----------------------
# rass_ct <- rass_raw1 %>%
#   group_by(grid, dttm) %>%
#   count() %>%
#   ungroup() %>%
#   filter(n > 1)
# rass_ct %>% count(n) # 285 duplicates, all in pairs

# take the smaller RASS score for each dttm
rass_clean <- rass_raw1 %>% 
  filter(!is.na(rass_score)) %>% # also remove 898 NAs
  group_by(grid, dttm) %>% 
  filter(rass_score == min(rass_score)) %>% 
  ungroup() %>% 
  distinct(grid, dttm, rass_date, rass_score)

#> reduce CAM duplication --------------------
# cam_ct <- cam_icu %>%
#   group_by(grid, dttm) %>%
#   count() %>%
#   ungroup() %>% 
#   filter(n > 1)
# cam_ct %>% count(n) # 92 duplicates, all in pairs
cam_icu_dup_w <- cam_icu %>% 
  semi_join(cam_ct, by = c("grid", "dttm")) %>% 
  arrange(grid, dttm, cam_value) %>% 
  group_by(grid, dttm) %>% 
  mutate(cam_value.x = lag(cam_value)) %>% 
  ungroup() %>% 
   filter(!is.na(cam_value.x)) %>% 
  rename(cam_value.y = cam_value)
cam_icu_dup_w %>% 
  group_by(cam_value.x, cam_value.y) %>% 
  count() %>% 
  print(n = 10)
# check in CAM_raw1 - No other CAM values
# cam_raw1 %>% 
#   semi_join(cam_ct, by = c("grid", "dttm")) %>% 
#   arrange(grid, dttm) %>% 
#   filter(cam_test_name != "CAM-ICU")

# clean CAM-ICU based on RASS
cam_icu_dup_rass <- cam_icu_dup_w %>%
  left_join(rass_clean, 
            by = c("grid", "dttm")) %>% 
  select(grid, dttm, cam_date, cam_value.x, cam_value.y, rass_date, rass_score) %>% 
  print(n = 100)
cam_icu_dup_rass %>% 
  count(cam_value.x, cam_value.y, rass_score) %>% 
  print(n = 50)
cam_icu_dup_rass <- cam_icu_dup_rass %>% 
  mutate(cam_value = case_when(
    cam_value.x == "Delirium present" & cam_value.y == "No delirium" ~ "Unk",
    cam_value.x == "Negtiv" & cam_value.y == "Postiv" ~ "Unk",
    cam_value.x %in% c("Negtiv", "Postiv") & cam_value.y == "Unases" & rass_score %in% -5:-4 ~ "UA",
    cam_value.x %in% c("Negtiv", "Postiv") & cam_value.y == "Unases" & !rass_score %in% -5:-4 ~ cam_value.x
  )) 
cam_icu_dup_rass %>% 
  count(cam_value.x, cam_value.y, rass_score, cam_value) 
cam_icu_clean <- cam_icu %>% 
  select(grid, cam_value, dttm, cam_date) %>% 
  anti_join(cam_icu_dup_rass, by = c("grid", "dttm")) %>% 
  bind_rows(select(cam_icu_dup_rass, grid, cam_value, dttm, cam_date))
cam_icu_clean %>% 
  count(cam_value)

#> Join CAM RASS data --------------------------------------------------------
cam_rass <- cam_icu_clean %>% 
  full_join(rass_clean, 
            by = c("grid", "dttm")) %>% 
  select(grid, dttm, cam_date, cam_value, rass_date, rass_score) %>% 
  arrange(grid, dttm) %>% 
  mutate(
    dt = as_date(dttm),
    rass = case_when(
      rass_score %in% -5:4 ~ rass_score
    ),
    cam = case_when(
      rass_score %in% -5:-4 ~ "UA",
      cam_value %in% c("Delirium present", "Postiv") ~ "Positive",
      cam_value %in% c("No delirium", "Negtiv") ~ "Negative",
      cam_value == "Unk" ~ "Unk" # conflicting CAM-ICU at the same time point
    )
  )
## check to make sure no duplicated dttm
cam_rass %>% distinct(grid, dttm) 

addmargins(xtabs( ~ rass_score + cam_value, data = cam_rass, addNA = T))
addmargins(xtabs( ~ rass + cam, data = cam_rass, addNA = T))

#> CAM RASs daily status
cam_rass_daily <- cam_rass %>%
  filter(!is.na(rass) | !is.na(cam)) %>% 
  group_by(grid, dt) %>% 
  count(cam) %>% 
  ungroup() %>% 
  spread(key = cam, value = n, fill = 0L) %>% 
  bind_cols(
    cam_rass %>% 
      filter(!is.na(rass) | !is.na(cam)) %>% 
      mutate(
        rass_c = case_when(
          rass %in% -5:-4 ~ "n_rass_coma",
          rass %in% -3:4 ~ "n_rass_nocoma",
          is.na(rass) ~ "n_rass_na"
        )
      ) %>% 
      group_by(grid, dt) %>% 
      count(rass_c) %>% 
      ungroup() %>% 
      spread(key = rass_c, value = n, fill = 0L) 
  ) %>% 
  rename(n_cam_pos = Positive,
         n_cam_neg = Negative,
         n_cam_unk = Unk,
         n_cam_ua = UA,
         n_cam_na = `<NA>`) %>% 
  mutate(n = n_rass_coma + n_rass_na + n_rass_nocoma) %>% 
  select(-grid1, -dt1)
cam_rass_daily %>% 
  filter(n_cam_unk > 0) %>% 
  print(n = 50)
cam_rass_daily <- cam_rass_daily %>% 
  mutate(
    status.today = case_when(
      n_cam_pos > 0 ~ "Delirious",
      n_cam_unk > 0 ~ "Unknown: conflicting CAM",
      n_rass_coma > 0 ~ "Comatose",
      n_cam_neg > 0 ~ "Normal",
      n == n_cam_na & n_rass_nocoma > 0 ~ "Unknown: RASS only",
      n == n_cam_na & n == n_rass_na ~ "Unknown: No CAM nor RASS"
    )) 
cam_rass_daily %>% 
  filter(n_cam_unk > 0) %>% 
  arrange(status.today) %>% 
  print(n = 50) 
cam_rass_daily %>% 
  count(status.today)
sum(unique(cam_rass_daily$grid) %in% changed_grid$old_grid)
sum(unique(cam_rass_daily$grid) %in% changed_grid$updated_grid)
write_csv(cam_rass_daily, "../output/daily_status_20190925.csv")
cam_rass_daily %>% count(year(dt))
min(cam_rass_daily$dt)

# check really old dates
cam_rass_daily %>% 
  filter(year(dt) < 2004) %>% 
  select(grid, dt, status.today)
cam_raw1 %>% 
  filter(grid %in% c("R204442438", "R232701930", "R235826909", "R245834805")) %>% 
  print(n = 100)
rass_raw1 %>% 
  filter(grid %in% c("R204442438", "R232701930", "R235826909", "R245834805")) %>% 
  print(n = 100)
changed_grid_dob %>% 
  filter(old_grid %in% c("R204442438", "R232701930", "R235826909", "R245834805") | 
           updated_grid %in% c("R204442438", "R232701930", "R235826909", "R245834805"))
demo_raw %>% 
  filter(grid %in% c("R204442438", "R232701930", "R235826909", "R245834805") | 
           primary_grid %in% c("R204442438", "R232701930", "R235826909", "R245834805")) %>% 
  select(grid, primary_grid, dob, deceased)

#. Find CAM-ICU encounters --------------------------------------------------
#> find hospital admission dates ----------------
sum(unique(discharge_raw$grid) %in% changed_grid$old_grid)
discharge_raw1 <- discharge_raw %>% 
  left_join(changed_grid_dob, by = c("grid" = "old_grid")) %>% 
  rename(adm_time = admiss_date,
         dc_time = discharge_date) %>% 
  mutate(
    adm_dttm = if_else(!is.na(updated_grid),
                       mdy_hms(adm_time) - as_datetime(old_dob) + as_datetime(updated_dob),
                       mdy_hms(adm_time)),
    dc_dttm = if_else(!is.na(updated_grid),
                      mdy_hms(dc_time) - as_datetime(old_dob) + as_datetime(updated_dob),
                      mdy_hms(dc_time)),
    adm_date = as_date(adm_dttm),
    dc_date = as_date(dc_dttm),
    grid = if_else(!is.na(updated_grid), updated_grid, grid)
  ) %>% 
  distinct(grid, adm_dttm, dc_dttm, adm_date, dc_date) %>% 
  arrange(grid, adm_dttm, dc_dttm) # no exact duplicates

sum(discharge_raw1$grid %in% changed_grid$old_grid)
discharge_raw %>% distinct(grid)
discharge_raw1 %>% distinct(grid)
discharge_raw1 %>% filter(grid == "R200022925") %>% print(n = 40)
discharge_raw %>% filter(grid %in% c("R200022925", "R275532955")) %>% print(n = 40)

discharge_raw2 <- discharge_raw1 %>%
  distinct(grid, adm_date, dc_date) %>% 
  filter(year(adm_date) %in%  c(1924, 1958, 2000, 2001, 2004:2019))
discharge_raw2 %>% filter(adm_date > dc_date)


discharge_raw1 %>% 
  group_by(grid, adm_date, dc_date) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n > 1, !is.na(dc_date))  
discharge_raw %>% 
  filter(grid == "R268402258")
discharge_raw %>% 
  filter(grid == "R270484727")
## same adm/dc date, not due to messed up ID
discharge_raw2 %>% 
  group_by(grid) %>% 
  filter(lag(dc_date) > adm_date)

hosp_dates <- NULL          
for (id in unique(discharge_raw2$grid)) {
  ## Subset hospital admission data to only work with one patient
  use.data <- discharge_raw2 %>% 
    filter(grid == id)  
  ## Create vector of all unique dates the patient was in the hospital
  all.hosp.dates <- lapply(1:nrow(use.data), function(x) 
    if (is.na(use.data$dc_date[x])) {
      use.data$adm_date[x]  # if discharge data missing, include only admission date as hospital date
    } else {seq(use.data$adm_date[x], use.data$dc_date[x], by = "days")}
  ) %>% 
    unlist() %>% 
    unique() %>% 
    as_date() %>% 
    sort()
  ## Create daily level data
  pt_dates <- tibble(grid = rep(id, length(all.hosp.dates)),
                     hosp_date = all.hosp.dates)
  hosp_dates <- bind_rows(hosp_dates, pt_dates)
  cat('ID:', id, '\n')
}
hosp_dates$in_adm <- 1

#. merge with CAM-ICU dates and defind admissions  
cam_hosp <- cam_rass_daily %>% 
  select(grid, dt, status.today) %>% 
  # consider conflicting CAM as missing
  filter(status.today %in% c("Comatose", "Delirious", "Normal")) %>% 
  full_join(
    hosp_dates,
    by = c("grid", "dt" = "hosp_date")
  ) %>% 
  arrange(grid, dt) %>% 
  group_by(grid) %>% 
  mutate(
    dt_diff  = dt - lag(dt),
    adm_id = cumsum(coalesce(dt_diff, 0) > 1) + 1) %>% 
  ungroup() 
## find adm/dc date and whether it's a no-CAM visit
cam_visits1 <- cam_hosp %>%
  group_by(grid, adm_id) %>% 
  summarise(adm_date = min(dt),
            dc_date = max(dt),
            hosp_days = dc_date - adm_date + 1,
            no_cam_days = sum(is.na(status.today)),
            note = case_when(
              sum(is.na(in_adm)) == n() ~ "Only CAM-ICU, no admission",
              sum(is.na(in_adm)) > 0 ~ "Admission extended by CAM-ICU days"
            )
  ) %>% 
  ungroup() %>% 
  filter(hosp_days != no_cam_days)

cam_hosp <- cam_hosp %>% 
  semi_join(
    cam_visits1, by = c("grid", "adm_id")
  )

## find first/last cam and days in between
cam_visits2 <- cam_hosp %>% 
  filter(!is.na(status.today)) %>% 
  group_by(grid, adm_id) %>% 
  summarise(first_cam = min(dt),
            last_cam = max(dt),
            bt_days = last_cam - first_cam + 1,
  ) %>% 
  ungroup()


## find # of coma/delirium/norm for each visit
cam_visits3 <- cam_hosp %>%
  group_by(grid, adm_id) %>% 
  count(status.today) %>% 
  ungroup() %>% 
  spread(key = status.today, value = n, fill = 0L) %>% 
  select(-`<NA>`) 

## put everything together
cam_visits <- cam_visits1 %>%
  left_join(cam_visits2, by = c("grid", "adm_id")) %>% 
  left_join(cam_visits3, by = c("grid", "adm_id")) %>% 
  mutate(any_coma = ifelse(Comatose > 0, 1, 0),
         any_del = ifelse(Delirious > 0, 1, 0),
         all_norm = ifelse(Normal == bt_days, 1, 0),
         norm_un = ifelse(any_coma + any_del + all_norm == 0, 1, 0),
         all_hole_size = bt_days - Comatose - Delirious - Normal
  ) %>% 
  rename(
    coma_days = Comatose,
    del_days = Delirious,
    norm_days = Normal
  ) %>% 
  filter(bt_days != coma_days) # remove all coma visits

cam_visits %>% 
  count(note)
cam_visits %>% 
  select(note, hosp_days, no_cam_days, bt_days, all_hole_size, coma_days:norm_un) %>% 
  Hmisc::describe()
cam_visits %>% 
  count(all_norm)
write_csv(cam_visits, "../output/cam_stay_2019025.csv")

#> visit summary: status ----------------
coma_q <- cam_visits %>% 
  filter(any_coma > 0) %>% 
  pull(coma_days) %>% 
  quantile()
del_q <- cam_visits %>% 
  filter(any_del > 0) %>% 
  pull(del_days) %>% 
  quantile()

tibble(
  `Visit Status` = c("Total # of visits", 
                     "# of visits with coma", "median # of days with coma (IQR)",
                     "# of visits with delirium", "median # of days with delirium (IQR)",
                     "# of visits with all normal", "# of visits with normal + unknown"),
  n = c(comma(nrow(cam_visits)), 
        comma(sum(cam_visits$any_coma)), paste0(coma_q[3], " (", coma_q[2], ", ", coma_q[4], ")"),
        comma(sum(cam_visits$any_del)), paste0(del_q[3], " (", del_q[2], ", ", del_q[4], ")"),
        comma(sum(cam_visits$all_norm)), comma(sum(cam_visits$norm_un)))
) 
#> Missing rate ---------------------------
sum(cam_visits$all_hole_size == 0)
sum(cam_visits$all_hole_size == 0)/nrow(cam_visits)
sum(cam_visits$bt_days)
sum(cam_visits$all_hole_size)
as.numeric(sum(cam_visits$all_hole_size))/as.numeric(sum(cam_visits$bt_days)) # 8%

sum(unique(cam_visits$grid) %in% changed_grid$old_grid) 
sum(unique(cam_visits$grid) %in% changed_grid$updated_grid) 


