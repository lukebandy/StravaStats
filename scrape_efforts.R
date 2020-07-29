# Libraries
library(yaml)
library(tidyverse)
library(httr)
library(jsonlite)

# Variables
full_refresh <- FALSE

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
activities <- NA
while(!done) {
  req <- GET(
    url = 'https://www.strava.com/api/v3/athlete/activities',
    config = token,
    query = list(per_page = 200, 
                 page = i)
  )
  activities_new <- fromJSON(content(req, as = 'text'), flatten = T)
  if (nrow(activities_new) < 200) {
    done <- TRUE
  }
  i <- i + 1
  if (is.na(activities)) {
    activities <- activities_new
  }
  else {
    activities <- activities %>%
      bind_rows(activities_new)
  }
}
message(paste("Found", nrow(activities), " activities"))

# Get all of segment efforts
# TODO: Skip activity if efforts have previously been downloaded
efforts <- NA
for (activity in activities[['id']]) {
  message(activity)
  i <- 1
  done <- FALSE
  while(!done) {
    req <- GET(
      url = paste0('https://www.strava.com/api/v3/activities/', activity),
      config = token,
      query = list(per_page = 200, 
                   page = i)
    )
    efforts_new <- fromJSON(content(req, as = 'text'), flatten = T)[['segment_efforts']]
    if (length(efforts_new) > 0) {
      if (nrow(efforts_new) < 200) {
        done <- TRUE
      }
      i <- i + 1
      if (is.na(efforts)) {
        efforts <- efforts_new
      }
      else {
        efforts <- efforts %>%
          bind_rows(efforts_new)
      }
    }
    else {
      done <- TRUE
    }
  }
}

# Strip back data for writing
activities_stripped <- activities %>%
  select(id, start_date_local, name, type, distance, average_speed)

segments_stripped <- efforts %>%
  # Get required info
  select(segment.id, segment.name, segment.start_latitude, segment.start_longitude, segment.distance) %>%
  distinct() %>%
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
  
efforts_stripped <- efforts %>%
  select(segment.id, activity.id, start_date_local, moving_time) %>%
  # Is this the first effort for this segment?
  arrange(segment.id, start_date_local) %>%
  group_by(segment.id) %>%
  mutate(is_first = row_number() == 1) %>%
  ungroup()

# Read in existing KOM data
if (file.exists("koms.csv")) {
  koms <- read.csv("koms.csv")
} else {
  koms <- data.frame(segment.id = as.integer(), 
                     kom_time = as.integer(),
                     qom_time = as.integer())
}

# Scrape KOMs for segments found in scrape_efforts.R
koms_raw <- data.frame(segment.id = as.integer(), 
                       kom_text = as.character(),
                       qom_text = as.character())

for (segment in segments_stripped$segment.id) {
  # Skip if we already have times for this segment unless doing full refresh
  if (full_refresh | !segment %in% koms$segment.id) {
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
write.csv(koms_processed, "koms.csv", row.names=FALSE)
