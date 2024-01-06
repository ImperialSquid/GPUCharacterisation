# Use regex to strip out all but the level from the jobId

checkps = checkps %>%
  mutate(level = gsub("(^1024-lvl)|(.{37}$)", "", jobId)) %>%
  select(-jobId)
