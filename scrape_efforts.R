# Libraries
library(yaml)
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)

# Get authentication token
credentials <- read_yaml('credentials.yaml')

token <- oauth2.0_token(
  endpoint = oauth_endpoint(
    request = NULL,
    authorize = 'https://www.strava.com/oauth/authorize',
    access = 'https://www.strava.com/oauth/token'
  ),
  app = oauth_app('strava', credentials$client_id, credentials$secret),
  scope = 'activity:read_all',
  as_header = F
)

# Get list of all activities
# TODO: Stop once old activities have been reached
i <- 1
done <- FALSE
if (file.exists("activities.csv")) {
  activities_stripped <- read.csv("activities.csv")
} else {
  activities_stripped <- data.frame(id = as.integer())
}
activities_new <- data.frame()
while(!done) {
  req <- GET(
    url = 'https://www.strava.com/api/v3/athlete/activities',
    config = token,
    query = list(per_page = 200, 
                 page = i)
  )
  activities_page <- fromJSON(content(req, as = 'text'), flatten = T)
  if (nrow(activities_page) < 200) {
    done <- TRUE
  }
  i <- i + 1
  activities_new <- activities_new %>%
    bind_rows(activities_page)
}
activities_new <- activities_new %>%
  filter(! id %in% activities_stripped$id)
message(paste("Found", nrow(activities_new), "new activities"))

# Get all of segment efforts
if (file.exists("efforts.csv")) {
  efforts_stripped <- read.csv("efforts.csv")
} else {
  efforts_stripped <- data.frame(activity.id = as.integer())
}
efforts_new <- data.frame(segment.id = as.integer(), 
                          activity.id = as.integer(), 
                          start_date_local = as.character(), 
                          elapsed_time = as.integer(), 
                          segment.name = as.character(), 
                          segment.start_latitude = as.numeric(), 
                          segment.start_longitude = as.numeric(), 
                          segment.distance = as.numeric())
for (activity in activities_new[['id']]) {
  message(activity)
  req <- GET(
    url = paste0('https://www.strava.com/api/v3/activities/', activity),
    config = token,
    query = list(include_all_efforts = TRUE)
  )
  efforts_page <- fromJSON(content(req, as = 'text'), flatten = T)[['segment_efforts']]
  efforts_new <- efforts_new %>%
    bind_rows(efforts_page)
}

# Strip back data for writing
efforts_stripped <- efforts_stripped %>%
  bind_rows(efforts_new %>%
              select(segment.id, activity.id, start_date_local, time = elapsed_time))

activities_stripped <- activities_stripped %>%
  bind_rows(activities_new %>%
              select(id, start_date_local, name, type, distance, average_speed, 
                     moving_time, total_elevation_gain, gear_id))

if (file.exists("segments.csv")) {
  segments_stripped <- read.csv("segments.csv")
} else {
  segments_stripped <- data.frame()
}

segments_stripped <- segments_stripped %>% 
  bind_rows(
    efforts_new %>%
      filter(!segment.id %in% segments_stripped$segment.id) %>%
      # Get required info
      select(segment.id, segment.name, segment.start_latitude, segment.start_longitude, segment.distance) %>%
      distinct()) %>%
  # Increment names to remove duplicates
  group_by(segment.name) %>%
  add_tally() %>%
  mutate(i = row_number()) %>%
  ungroup() %>%
  mutate(segment.name = case_when(
    n == 1 ~ segment.name,
    TRUE ~ paste0(segment.name, " (", i, ")")
  )) %>%
  select(-c(n, i))

# Read in existing KOM data
if (file.exists("koms.csv")) {
  koms <- read.csv("koms.csv")
} else {
  koms <- data.frame(segment.id = as.integer(), 
                     kom_time = as.integer(),
                     qom_time = as.integer())
}

# Scrape KOMs for segments found in scrape_efforts.R
# TODO: Accept hazardous segment waiver?
koms_raw <- data.frame(segment.id = as.integer(), 
                       kom_text = as.character(),
                       qom_text = as.character())

for (segment in segments_stripped$segment.id) {
  # Skip if we already have times for this segment unless doing full refresh
  if (!segment %in% koms$segment.id) {
    webpage <- read_html(paste0('https://www.strava.com/segments/', segment))
    rank_data_html <- html_nodes(webpage,'.track-click')
    
    kom <- str_split(html_text(rank_data_html)[1], "\\\\|[^[:print:]]")[[1]][4]
    qom <- str_split(html_text(rank_data_html)[2], "\\\\|[^[:print:]]")[[1]][4]
    
    message(paste(segment, kom, qom))
    
    row <- data.frame(segment.id = segment, 
                      kom_text = kom, 
                      qom_text = qom)
    koms_raw <- bind_rows(koms_raw, row)
  }
}

# Convert strings to seconds
koms_processed <- koms_raw %>%
  mutate(kom_text = as.character(kom_text),
         qom_text = as.character(qom_text),
         kom_text = case_when(
           str_detect(kom_text, "s") ~ paste0("0:", str_sub(kom_text, 1, -2)),
           TRUE ~ kom_text
         ), qom_text = case_when(
           str_detect(qom_text, "s") ~ paste0("0:", str_sub(qom_text, 1, -2)),
           TRUE ~ qom_text
         )) %>%
  separate(kom_text, c("kom_minutes", "kom_seconds"), ":", remove = FALSE) %>%
  separate(qom_text, c("qom_minutes", "qom_seconds"), ":", remove = FALSE) %>%
  mutate(kom_seconds = as.integer(kom_seconds),
         kom_minutes = as.integer(kom_minutes),
         kom_time = (kom_minutes * 60) + kom_seconds,
         qom_seconds = as.integer(qom_seconds),
         qom_minutes = as.integer(qom_minutes),
         qom_time = (qom_minutes * 60) + qom_seconds)

koms_updated <- koms %>%
  bind_rows(koms_processed %>%
              select(segment.id, kom_time, qom_time))

# Write data  
write.csv(activities_stripped, "activities.csv", row.names=FALSE)
write.csv(segments_stripped, "segments.csv", row.names=FALSE)
write.csv(efforts_stripped, "efforts.csv", row.names=FALSE)
write.csv(koms_updated, "koms.csv", row.names=FALSE)

if (!file.exists("leaderboard.csv")) {
  write.csv(data.frame(segment.id = as.integer(), 
                       position = as.integer(),
                       time = as.integer()), 
            "leaderboards.csv", 
            row.names=FALSE)
}