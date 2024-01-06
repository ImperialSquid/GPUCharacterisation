# boil down the gpu_tasks dataset to get averaged metrics
benchmarked_tasks = gpu_tasks %>%
  group_by(hostname, taskId, eventName) %>%
  summarise(start = first(na.omit(start)), 
            stop = last(na.omit(stop)),
            avgPower = mean(powerDrawWatt),
            avgMem = mean(gpuMemUtilPerc),
            avgUtil = mean(gpuUtilPerc),
            avgTemp = mean(gpuTempC), .groups = "keep") %>%
  ungroup() %>%
  mutate(dur = stop-start) %>%
  relocate(dur, .after = stop) %>%
  arrange(hostname, start)
