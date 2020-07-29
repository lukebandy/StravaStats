# Libraries
library(tidyverse)
library(rvest)

# Scrape KOMs for segments found in scrape_efforts.R
koms_raw <- data.frame(segment.id = as.integer(), 
                       kom_text = as.character(),
                       qom_text = as.character())

for (segment in segments_stripped$segment.id) {
  webpage <- read_html(paste0('https://www.strava.com/segments/', segment))
  rank_data_html <- html_nodes(webpage,'.track-click')
  
  kom <- str_split(html_text(rank_data_html)[1], "\\\\|[^[:print:]]")[[1]][4]
  qom <- str_split(html_text(rank_data_html)[2], "\\\\|[^[:print:]]")[[1]][4]
  
  message(paste(segment, kom, qom))
  
  row <- data.frame(segment.id = segment, 
                    extraction_date = Sys.time(),
                    kom_text = kom, 
                    qom_text = qom)
  koms_raw <- bind_rows(koms_raw, row)
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

# Write data
write.csv(koms_processed, "koms.csv", row.names=FALSE)
