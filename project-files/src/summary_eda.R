# Just a lot of misc eda
checkps %>%
  group_by(taskId, eventName) %>%
  group_by(n()) %>%
  summarise(rows = n())

checkps %>%
  group_by(taskId, eventName, eventType) %>%
  filter(n() == 2) %>%
  summarise(diff = as.numeric(nth(timestamp, 1) - 
                                nth(timestamp, 2))) %>%
  filter(diff != 0)

gpu %>%
  group_by(hostname) %>%
  count(gpuSerial) %>%
  filter(n() > 1) %>%
  summarise()

gpu %>%
  group_by(gpuSerial) %>%
  count(gpuUUID) %>%
  filter(n() > 1) %>%
  summarise()

length(unique(gpu2$gpuSerial))
length(unique(gpu$hostname))