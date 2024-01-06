# Convert string timestamps to datetime objects with lubridate

checkps = checkps %>%
  mutate(timestamp = as_datetime(timestamp, format = "%FT%H:%M:%OSZ")) %>%
  arrange(timestamp)

gpu = gpu %>%
  mutate(timestamp = as_datetime(timestamp, format = "%FT%H:%M:%OSZ")) %>%
  arrange(timestamp)
