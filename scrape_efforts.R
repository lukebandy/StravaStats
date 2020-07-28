# Libraries
library(yaml)
library(tidyverse)
library(httr)
library(jsonlite)

# Get token
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

# Get list of my activities
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

# Get all of my segment efforts
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

# Write data
write.csv(activities_stripped, "activities.csv", row.names=FALSE)
write.csv(segments_stripped, "segments.csv", row.names=FALSE)
write.csv(efforts_stripped, "efforts.csv", row.names=FALSE)
