# profiling as.posixct from base R
tic("as.posix")
tic("posix - checkps")
x = checkps %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%FT%H:%M:%OSZ")) %>%
  arrange(timestamp)
toc("posix - checkps")

tic("posix - gpu")
x = gpu %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%FT%H:%M:%OSZ")) %>%
  arrange(timestamp)
toc("posix - gpu")
toc("as.posix")

rm(x)


# profiling as_datetime from lubridate
tic("as_datetime")
tic("as_datetime - checkps")
x = checkps %>%
  mutate(timestamp = as_datetime(timestamp, format = "%FT%H:%M:%OSZ")) %>%
  arrange(timestamp)
toc("as_datetime - checkps")

tic("as_datetime - gpu")
x = gpu %>%
  mutate(timestamp = as_datetime(timestamp, format = "%FT%H:%M:%OSZ")) %>%
  arrange(timestamp)
toc("as_datetime - gpu")
toc("as_datetime")

rm(x)