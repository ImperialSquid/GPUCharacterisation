# Previously we established that particular tasks within one area of the map
# are much quicker to render
# Aside from particular tasks being much quicker, there may be other factors 
# that affect render time
# This section will focus on those alternative factors

# Investigate the interaction between power draw/memory util/
# temperature/rendering duration
benchmarked_tasks %>%
  filter(eventName == "Rendering") %>%
  filter(dur > 27) %>%    # remove outliers
  mutate(dur = as.numeric(dur)) %>% 
  select(avgPower, avgTemp, avgUtil, avgMem, dur) %>% 
  cor(.)

#           avgPower   avgTemp   avgUtil    avgMem       dur
# avgPower 1.0000000 0.4005992 0.5788924 0.6230360 0.6048566
# avgTemp            1.0000000 0.1193750 0.1017243 0.1174742
# avgUtil                      1.0000000 0.7748840 0.7806070
# avgMem                                 1.0000000 0.7801920
# dur                                              1.0000000

# From this we can see that a high task duration is correlated with
# higher power draw, and especially with higher processor utilisation 
# and memory utilisation.

# Conversely it's not highly correlated with temperature, in fact,
# temperature is nor highly correlated with any other the variables
# under investigation, this is likely due to sufficient cooling on 
# the GPUs for whatever task being attempted so we can safely 
# discard it for future analysis

# Exploring total resource utilisation, avgUtil and avgMem are 
# both percentage based so a lot of tasks reaching 100% would imply
# bottlenecking, alternatively a lot of low percentages would imply
# resources not being used efficiently and GPUs spending 
# a lot of up time idle

benchmarked_tasks %>% 
  filter(eventName == "Rendering") %>%
  filter(dur > 27) %>%    # remove outliers
  filter(! is.na(avgMem)) %>% 
  mutate(dur = as.numeric(dur)) %>% 
  summarise(max = max(avgMem), 
            p95 = quantile(avgMem, 0.95),
            p90 = quantile(avgMem, 0.90),
            p75 = quantile(avgMem, 0.75), 
            med = median(avgMem),
            mean = mean(avgMem), 
            p25 = quantile(avgMem, 0.25),
            p10 = quantile(avgMem, 0.10), 
            p05 = quantile(avgMem, 0.05),
            min = min(avgMem))

# avgUtil
#  max   p95   p90   p75   med  mean   p25   p10   p05   min
# 89.3  78.4  77.2  75.0  72.5  72.2  69.6  66.7  64.7  45.6
# 
# avgMem
#  max   p95   p90   p75   med  mean   p25   p10   p05   min
# 53.5  47.5  45.8  42.0    38  38.0    34  30.5  28.8  17.2

# We can see processor utilisation peaks around 90%, in fact 95% of 
# rendering tasks have an average utilisation of at least 64.7%,
# we can infer that there is little chance of bottlenecking and no
# obvious evidence of lots of idling either.

# For memory util, here we see no evidence of idling or bottlenecking 
# either. While memory is not being fully used, peaking at avg 53.5%, 
# having unused memory is not the end of the world either and increasing
# the size of the job given to each card is likely to increase processor
# util too

# Unfortunately it's hard to gain many more insights about the interplay
# between these factors due to this data not being obtained in a controlled
# experiment. We also can't use linear regression due to what is likely a
# complex causal relationship between duration and proc util/mem util/power