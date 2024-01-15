# Combine tables so we have the performance metrics matched up to tasks

# use SQLite to match between time stamps 
# event start <= gpu timestamp <= event end
gpu_tasks = sqldf("SELECT gpu.hostname, gpu.powerDrawWatt, gpu.gpuTempC, 
gpu.gpuUtilPerc, gpu.gpuMemUtilPerc, gpu.timestamp, 
checkps_start_stop.eventName, checkps_start_stop.taskId, checkps_start_stop.level, 
checkps_start_stop.start, checkps_start_stop.stop  FROM gpu
       LEFT JOIN checkps_start_stop
       ON  gpu.hostname = checkps_start_stop.hostname
       AND gpu.timestamp BETWEEN checkps_start_stop.start AND checkps_start_stop.stop")

# Replace empty event names with "Idle" since the GPU isn't doing anything
# For taskId, carry forward taskIds to fill NAs, set first NA chuck to be "vmXXXX-pre-start"
gpu_tasks = gpu_tasks %>%
  relocate(start, stop, timestamp, 
           hostname, eventName, 
           powerDrawWatt, gpuTempC, 
           gpuUtilPerc, gpuMemUtilPerc, 
           taskId, level) %>%
  arrange(hostname, timestamp) %>%
  mutate(eventName = replace_na(eventName, "Idle"))  %>%
  mutate(eventName = factor(eventName)) %>%
  group_by(hostname) %>%
  fill(taskId, .direction = "down") %>%
  mutate(taskId = coalesce(taskId, "pre-start"))

# Do some funky data manip to get times for the start and stop of idle events
gpu_tasks = gpu_tasks %>%
  filter(eventName == "Idle" | eventName == "Total") %>%
  mutate(idle_start = stop) %>%
  fill(idle_start, .direction = "down") %>%
  mutate(idle_stop = start) %>%
  fill(idle_stop, .direction = "up") %>%
  filter(eventName == "Idle") %>%
  ungroup() %>%
  select(idle_start, idle_stop, timestamp, eventName, hostname) %>% 
  full_join(. , gpu_tasks, 
            by = c("timestamp", "eventName", "hostname"),
            relationship = "many-to-many") %>%
  mutate(start = coalesce(start, idle_start)) %>%
  mutate(stop = coalesce(stop, idle_stop)) %>%
  select(-idle_start, -idle_stop) %>%
  arrange(hostname, timestamp)
