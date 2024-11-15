library(tidyverse)
library(sswidb)
library(sswids)
library(sf)
library(mapview)
library(raster)
library(landscapemetrics)
library(exactextractr)
library(nngeo)
select <- dplyr::select

#######################################################
##       Connecting to Snapshot Wisconsin database   ##
#######################################################

# connect to data base
sswids::connect_to_sswidb(db_version = 'PROD')

# create project folders
sswids::setup_project()

#####################################################################################
####                               Set data parameters                          #####
#####################################################################################

# set min/max years for obtaining data
min_year <- 2018
max_year <- 2023

# season start/end dates (in -MM-DD format)
# start with this and consider early spring and fall for getting newborns and yearlings 
min_date <- '-01-01'
max_date <- '-12-31'

#early spring
min_date <- '-04-01'
max_date <- '-06-01'

#fall
min_date <- '-09-20'
max_date <- '-11-30'

# what years of data to query?
years <- seq(min_year, max_year, 1)

# create data frame of seasons for data filtering
seasons_df <-
  create_season_dates(
    min_date = min_date,
    max_date = max_date,
    years = years
  )

# check out these dates
sswids::check_season_dates(seasons_df)

sswidb_species(conn)
# set species and grid types
species <- c('Turkey')
grid <- c('SSWI',"ELKBR")


# set classification precision level
prec <- 0

# set distance (meters) between camera_location_seq_nos to merge/average locations
cam_distance <- 100

# set precision of lat/long coordinates (how many decimal places are needed?)
coord_prec <- 4

# set desired proportion of photos classified (per sampling occasion) threshold
ppn_class_threshold <- 0


##########################################################
##                   Query raw data                     ##
##########################################################


# query databases
raw_data <-
  sswids::query_raw_data(
    species = species,
    grid = grid,
    season = seasons_df, # season dates/years to query
    prec = prec # ,
    # by default all 3 data sets are queried, but can just query detections/effort only 
    # outputs = c('detections', 'effort', 'locations')
  )

# save the raw queries
write_raw_data(raw_data, filename="2024")

# continue using detections, effort, and locations separately
# read in from raw_data folder
detections_df_raw <- read_csv('./data_raw/detections_df_raw2024.csv')
effort_df_raw <- read_csv('./data_raw/effort_df_raw2024.csv')
locs_df_raw <- read_rds('./data_raw/locations_df_raw2024.rds')

# see available spatial layers
list_spatial_layers()

TurkeyZones <- get_spatial_data("turkey_mgt_zones")
Counties <- get_spatial_data("counties")


#Remove detections without Wisconsin location data
list_nolocs <- rm_noloc_data(locs_df = locs_df_raw, effort_df = effort_df_raw, detections_df = detections_df_raw, "bear_zones", bear_mgmt_zone_id)

#Remove impossible dates
list_outsideactivedates <- rm_bad_batches(datalist=list_nolocs)

#Remove low precision locations
list_removelowpreclocs <- remove_low_precision_lat_long(list_outsideactivedates, coordinate_precision=4)

#Merge close locations
list_camsiteid <- merge_nearby_cameras(list_removelowpreclocs, cam_distance=100)

#Spatially subset camera locations
#list_spatialfilter <- spatial_subset(datalist = list_camsiteid, sf_layer = "ruffed_grouse_priority_areas",filter_statement = "rugr_pr != 'NA'")

#Remove overlapping effort
list_removeoverlap <- detect_remove_overlap(list_camsiteid)


#Calculate daily effort
effort_by_day_df <-
  sswids::effort_by_day(effort = list_removeoverlap[["effort DF"]])

#Calculate coordinates by cam_site_id
mean_locs_df <-
  sswids::average_camera_coordinates(
    locations = list_removeoverlap[["locs DF"]],
    effort = effort_by_day_df
  )

#Add in spatial covariates
mean_locs_sf_layers = mean_locs_df %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 3071)

mean_locs_sf_layers <-
  st_join(
    mean_locs_sf_layers,
    get_spatial_data("turkey_mgt_zones") %>%
      st_transform(., st_crs(mean_locs_sf_layers)) %>%
      st_make_valid(.),
    
    join = st_within
  )

#Create sampling occasions
# set desired number of sampling occasions
num_occasions <- 52
# 
effort_by_occ_df <-
  sswids::create_sampling_occasions(
    seasons = seasons_df,
    effort_by_day = effort_by_day_df,
    num_occasions = num_occasions
  )

BearEffortbyOccEx <- readRDS("C:/Users/wildeefb/Documents/GIT/Bears/data_clean/effort_by_occ_dfALL.rds")

#Calculate proportion classified by occasion
# use all data by setting proportion classified to 0
ppn_class_threshold = 0
effort_by_occ_df <-
  calculate_prop_classified(
    seasons = seasons_df,
    effort_day = effort_by_day_df,
    effort_occ = effort_by_occ_df
  )

day_occasion_df <-
  seasons_df %>%
  group_by(year) %>%
  nest() %>%
  # create date sequence for each year
  mutate(date = map(data, date_sequence)) %>%
  unnest(date) %>%
  select(-data) %>%
  # using row_number give day of season starting with day 1
  mutate(day_of_season = row_number()) %>%
  ungroup() %>%
  # split season into equal intervals (1-day, 3-day, ...1 week)
  # ntile() assigns each day into a sampling occasion
  mutate(
    occ = ntile(day_of_season, num_occasions)
  )

Q = DBI::dbGetQuery(conn,"SELECT
    g83100.ds_location_effort.class_final_trigger_count,
    g83100.ds_location_effort.class_effort_trigger_count,
    g83100.ds_location_effort.time_lapse_trigger_count,
    g83100.ds_location_effort.motion_trigger_count,
    g83100.ds_location_effort.final_date,
    g83100.ds_location_effort.camera_location_seq_no
FROM
    g83100.ds_location_effort")# %>% 
Q <-  filter(Q, CAMERA_LOCATION_SEQ_NO %in% unique(effort_by_day_df$camera_location_seq_no))
#Q2 = left_join(day_occasion_df, Q, by=join_by(date == FINAL_DATE))
effort_by_day2 <- left_join(effort_by_day_df, day_occasion_df, by=join_by(date_active == date, year == year))
Q2 = left_join(effort_by_day2, Q, by=join_by(date_active == FINAL_DATE, 
                                             camera_location_seq_no == CAMERA_LOCATION_SEQ_NO
                                             ))
view(head(Q))
view(head(Q2))

Q3 <- Q2%>%group_by(cam_site_id, year.x, occ)%>% 
  summarise(classified= sum(CLASS_EFFORT_TRIGGER_COUNT), total=sum(MOTION_TRIGGER_COUNT),
            ppn_classified=classified/total)
#colnames(Q3)[2] <- "year"
view(head(Q3))
cam1055 <- Q2[Q2$cam_site_id == "cam_1055" & Q2$year == 2018,]
effort_by_occ_df2 <- left_join(Q3, effort_by_occ_df, by =c("cam_site_id", "year", "occ"))
cam1055_2 <- effort_by_occ_df2[effort_by_occ_df2$cam_site_id == "cam_1055" & effort_by_occ_df2$year == 2018,]

# Q2 = Q2 %>%
#   left_join(dat3) %>%
#   replace_na(list(wolf_count=0))
# 
# Q3 = Q2 %>%
#   mutate(year = year(FINAL_DATE)) %>%
#   group_by(year,CAMERA_LOCATION_SEQ_NO) %>%
#   # need to filter out effort
#   summarise(wolf_sum = sum(wolf_count),
#             wolf_occ = ifelse(wolf_sum>0,1,0)) %>%
#   group_by(year) %>%
#   summarise(sites_occ = sum(wolf_occ),
#             sites_n = n()) %>%
#   mutate(prop_occ = sites_occ/sites_n)
# 
# Q3




write_rds(effort_by_occ_df2, "./data_clean/effort_by_occ_dfALL.rds")
effort_by_occ_df <- read_rds("./data_clean/effort_by_occ_dfALL.rds")

#Create detection histories
detections_wide_df_maxcount <-
  sswids::summarize_detections(
    detections = list_removeoverlap[["detections DF"]],
    seasons = seasons_df,
    summary_value = "max count"
  )

detections_wide_df_counttriggers <-
  summarize_detections(
    detections = list_removeoverlap[["detections DF"]],
    seasons = seasons_df,
    summary_value = "count triggers"
  )

#Join detections, effort, and spatial information
effort_by_occ_df_maxcount <-
  sswids::join_detections_effort(
    effort_by_occ = effort_by_occ_df2,
    detections = detections_wide_df_maxcount,
    discard_na_rows = FALSE
  )

effort_by_occ_df_counttriggers <-
  sswids::join_detections_effort(
    effort_by_occ = effort_by_occ_df2,
    detections = detections_wide_df_counttriggers,
    discard_na_rows = FALSE
  )

# merge spatial information with effort wide form 
effort_by_occ_df_maxcount_join = effort_by_occ_df_maxcount %>%
  full_join(mean_locs_sf_layers %>%
              select(-c(camera_location_seq_no)) %>%
              distinct, .)

effort_by_occ_df_counttriggers_join = effort_by_occ_df_counttriggers %>%
  full_join(mean_locs_sf_layers %>%
              select(-c(camera_location_seq_no)) %>%
              distinct,.)

# save effort join files
write_rds(effort_by_occ_df_maxcount_join,"data_clean/effort_by_occ_df_maxcount_joinALL.Rds")
write_rds(effort_by_occ_df_counttriggers_join,"data_clean/effort_by_occ_df_counttriggers_joinALL.Rds")



##################################scrap#####################################
#                  %in% c("count triggers", "max count")
# convert detections to wide format
# so there's a column of counts for different age/sex classes, etc.
detections <-
  list_camsiteid[["detections DF"]] %>%
  tidyr::pivot_wider(
    names_from = c(class_key),
    values_from = count,
    # fill in 0 adult/young; male/female, etc. counts if absent
    values_fill = 0
  ) %>%
  # tidy up
  dplyr::arrange(year, cam_site_id, detection_datetime)

day_occasion_df <-
  seasons_df %>%
  dplyr::group_by(year) %>%
  tidyr::nest() %>%
  dplyr::mutate(date = purrr::map(data, date_sequence)) %>%
  tidyr::unnest(date) %>%
  dplyr::select(-data) %>%
  dplyr::mutate(day_of_season =  dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(occ = dplyr::ntile(day_of_season, num_occasions))

# now create date in the detection data frame
# use that to join in the occasion #
# and summarize detection counts by occasion
detections <-
  detections %>%
  # date not in detections yet so create it
  dplyr::mutate(date = as.Date(detection_datetime)) %>%
  # assign day to equal interval bin
  dplyr::left_join(., day_occasion_df, by = c("year", "date")) #%>%
# matches() here picks out the sswi keys (which are always capitalized)
dplyr::select(cam_site_id:species, date, day_of_season, occ, matches("[A-Z]", ignore.case = FALSE)) %>%
  dplyr::group_by(year, cam_site_id, occ) %>%
  if (summary_value == "max count") {
    # calculate max count over each occasion
    # this works as key column headings are capitalized
    dplyr::summarise(dplyr::across(matches("[A-Z]", ignore.case = FALSE), max, na.rm = TRUE))
  }else{
    # COUNT instead of MAX
    dplyr::summarise(dplyr::across(matches("[A-Z]", ignore.case = FALSE), ~sum(. > 0, na.rm=TRUE)))} %>%
  dplyr::arrange(cam_site_id, year, occ) %>%
  dplyr::ungroup()

detections_maxcount_demo <- read_rds('C:/Users/wildeefb/Documents/GIT/sswids/vignettes/data_demo/detections_wide_df_maxcount.Rds')



######################################scrap############################
df <- tibble(x = c(1, 1, 1, 2, 2, 3), y = 1:6, z = 6:1)

# Specify variables to nest using name-variable pairs.
# Note that we get one row of output for each unique combination of
# non-nested variables.
df2 <- df %>% nest(data = c(y, z))


seasonest <- seasons_df %>%
  dplyr::group_by(year) %>%
  tidyr::nest() %>% 
  dplyr::mutate(date = purrr::map(data, date_sequence)) %>%
  tidyr::unnest(date)
