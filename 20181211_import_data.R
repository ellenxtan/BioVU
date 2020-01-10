library(tidyverse)
static_raw <- read_csv("../data/Samuels_Delirium_STATIC_20180718.csv")
discharge_raw <- read_csv("../data/Samuels_Delirium_DISCHARGE_20180718.csv")
cam_raw <- read_csv("../data/Samuels_Delirium_CAM_20180718.csv")
rass_raw <- read_csv(("../data/Samuels_Delirium_RASS_SCORE_20180718.csv"))

names(static_raw) <- tolower(names(static_raw))
names(discharge_raw) <- tolower(names(discharge_raw))
names(cam_raw) <- tolower(names(cam_raw))
names(rass_raw) <- tolower(names(rass_raw))

save.image(file = "../output/data_raw.RData")

describe(static_raw)
describe(discharge_raw)
describe(cam_raw)
describe(rass_raw)



length(unique(static_raw$grid)) 
length(unique(discharge_raw$grid))
length(unique(cam_raw$grid))
length(unique(rass_raw$grid))

sum(unique(discharge_raw$grid) %in% static_raw$grid)
sum(unique(cam_raw$grid) %in% static_raw$grid)
sum(unique(rass_raw$grid) %in% static_raw$grid)


cam_raw %>% 
  count(cam_value)
cam_raw %>% 
  count(cam_test_name)
xtabs( ~ cam_value + cam_test_name, data = cam_raw, addNA = T)

rass_raw %>% 
  count(rass_score)
rass_raw %>% 
  count(rass_score_test_name)
xtabs( ~ rass_score + rass_score_test_name, data = rass_raw, addNA = T)

static_raw %>% 
  count(biovu_flag)
static_raw %>% 
  count(compromised_flag)
static_raw %>% 
  count(mega_queue_flag)
addmargins(xtabs( ~ compromised_flag + biovu_flag, data = static_raw, addNA = T))
addmargins(xtabs( ~ mega_queue_flag + biovu_flag, data = static_raw, addNA = T))