# Convert checkps from 2 row START/STOP form to a one row start time/stop time form

checkps_start_stop = checkps %>%
  group_by(taskId, eventName, eventType) %>%
  filter(row_number()==1) %>% # filter out duplicate rows
  ungroup() %>%
  group_by(taskId, eventName) %>%
  arrange(eventType) %>% # Ensure start times come before stop times
  summarise(start = nth(timestamp, 1),
            stop = nth(timestamp, 2),
            hostname = first(hostname),
            level = first(level), .groups = "drop") %>% # reduce two rows in to one
  arrange(start, hostname) %>%
  relocate(start, stop, hostname, eventName, .before = taskId) %>% # reorder for neatness
  mutate(dur = stop-start) %>% # find durations
  relocate(dur, .after = stop)

