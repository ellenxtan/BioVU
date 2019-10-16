## compare DOB and changed GRIDs in new demo data to our old data
## produce DOB for all changed GRIDs.
library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)


load("../output/data_raw.RData")

#. Get DOB for all messed-up ID --------------------------
changed_grid <- read_excel("../../Mito Delirium BioVU Data/Data/Changed_GRIDS.xlsx") %>% 
  distinct()
names(changed_grid) <- str_to_lower(names(changed_grid))
Hmisc::describe(changed_grid)
sum(static_raw$grid %in% changed_grid$old_grid) #2583
sum(static_raw$grid %in% changed_grid$updated_grid) #962


demo_file <- list.files(path = "../../Mito Delirium BioVU Data/Demographics/", 
                        pattern = "_demo.txt$", full.names = T)
demo_raw <- NULL
for (file in demo_file){
  demo_raw %<>% 
    bind_rows(read_tsv(file, na = c("", "NA", "null"),
                       col_types = "cccnnnnnnnccc"))
}
names(demo_raw) <- str_to_lower(names(demo_raw))

demo_raw %<>% 
  distinct() %>% 
  mutate(dob = ymd(dob))

demo_raw %>% 
  select(grid, primary_grid, dob) %>% 
  Hmisc::describe() # 101,846 unique GRIDs

demo_raw %>% 
  filter(!is.na(primary_grid)) 
demo_raw %>% 
  filter(!is.na(primary_grid), !is.na(dob))
demo_raw %>% 
  filter(is.na(primary_grid), is.na(dob))
sum(demo_raw$primary_grid %in% demo_raw$grid)



demo_changed_grid <- demo_raw %>% 
  filter(!is.na(primary_grid)) %>% 
  distinct(grid, primary_grid) 


sum(demo_changed_grid$primary_grid %in% changed_grid$updated_grid)

sum(static_raw$grid %in% demo_raw$grid)
sum(changed_grid$updated_grid %in% demo_raw$grid) # all the old and updated GRIDs are in demo file
sum(demo_raw$grid %in% static_raw$grid | demo_raw$grid %in% changed_grid$updated_grid)
sum(unique(changed_grid$updated_grid) %in% demo_raw$primary_grid)
sum(unique(changed_grid$updated_grid) %in% demo_changed_grid$grid)
sum(unique(changed_grid$old_grid) %in% demo_changed_grid$grid)
demo_raw %>% 
  filter(!grid %in% static_raw$grid, !grid %in% changed_grid$updated_grid, grid %in% primary_grid)
demo_raw %>% 
  filter(is.na(primary_grid)) %>% 
  distinct(grid)
#' static_raw had 100,286 GRIDs.
#' changed_grid had 2583 old_grid, 2495 updated_grid
#' all the old_grid are in static_raw
#' 962 of the updated_grid are in static_raw, 1533 of the updated_grid NOT in static_raw
#' demo_raw had 101,846 GRIDs, including 100,286 GRIDs in static_raw, all 2495 updated_GRIDs in changed_grid
#' 100,286 + 2495 - 962 = 101,819, there are still 27 new GRIDs in demo_raw.  They also appear as primary_grid.
#' demo_raw 
#' Final # of unique GRID: 101,846- 2,612 = 99,234


demo_changed_grid %>% 
  filter(!grid %in% changed_grid$old_grid)


changed_grid_full <- demo_changed_grid %>% 
  full_join(changed_grid, by = c("grid" = "old_grid")) %>% 
  arrange(grid) 
changed_grid_full %>% 
  filter(is.na(updated_grid) | is.na(primary_grid))
changed_grid_full %>% 
  filter(is.na(updated_grid) | is.na(primary_grid), grid %in% static_raw$grid) 
#' These were the 27 new GRIDs in demo_raw
changed_grid_full %>% 
  filter(is.na(updated_grid) | is.na(primary_grid), !grid %in% static_raw$grid) 
changed_grid_full %>% 
  filter(primary_grid != updated_grid) 
#' These two updated_grids were futher updated to primary_grid




#. compare dob in demo vs. static ---------------------------------------
static_raw %<>%
  mutate(dob = mdy(dob))
grid_dob_prob <- demo_raw %>% 
  select(grid, dob) %>% 
  inner_join(
    static_raw %>% 
      select(grid, dob),
    by = "grid") %>% 
  filter(dob.x != dob.y) %>% 
  mutate(diff = dob.x - dob.y) %>% 
  arrange(grid) %>% 
  print(n = 30)
changed_grid %>% 
  filter(updated_grid %in% c("R205380161", "R202346769"))
static_raw %>% 
  filter(grid == "R272249649") %>% 
  select(grid, dob)
demo_raw %>% 
  filter(grid == "R272249649") %>% 
  select(grid, dob)
demo_raw %>% 
  select(grid, dob, primary_grid) %>% 
  inner_join(
    static_raw %>% 
      select(grid, dob),
    by = "grid") %>% 
  filter(dob.x != dob.y, grid %in% primary_grid)

demo_raw %>% 
  select(grid, dob) %>% 
  inner_join(
    static_raw %>% 
      select(grid, dob),
    by = "grid") %>% 
  filter(dob.x != dob.y) %>% 
  mutate(diff = dob.x - dob.y) %>% 
  filter(abs(diff) > 10) %>% 
  print(n = 30)

demo_raw %>% 
  select(grid, dob, primary_grid) %>% 
  inner_join(
    static_raw %>% 
      select(grid, dob),
    by = "grid") %>% 
  filter(dob.x != dob.y, (grid %in% changed_grid$old_grid | grid %in% changed_grid$updated_grid))


  
write_csv(grid_dob_prob, "../output/dob_discrepancy.csv")




#' The new changed GRIDs had 27 + 2 more old GRIDS compared to before
#' 27 in our data, 2 are further updated GRIDs not in our data
#' 5 old GRIDs updated to different GRIDs due to the further update.
changed_grid_dob <- demo_raw %>% 
  filter(grid %in% primary_grid) %>% 
  select(grid, dob) %>% 
  rename(primary_grid = grid) %>% 
  right_join(demo_changed_grid,
             by = "primary_grid") %>% 
  left_join(select(static_raw, grid, dob),
            by = "grid") %>% 
  select(grid, dob.y, primary_grid, dob.x) %>% 
  rename(old_grid = grid,
         old_dob = dob.y,
         updated_grid = primary_grid,
         updated_dob = dob.x) 

changed_grid_dob %>% Hmisc::describe()
changed_grid_dob %>% 
  filter(is.na(old_dob))
# check dob with static_raw
changed_grid_dob %>% 
  inner_join(static_raw, by = c("updated_grid" = "grid")) %>% 
  select(updated_grid, updated_dob, dob) %>% 
  filter(updated_dob != dob)

write_csv(changed_grid_dob, "../output/changed_grid_dob_20190924.csv")


