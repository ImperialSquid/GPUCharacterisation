# Having previously shown that some tasks have consistently low render times,
# but that this is due tp them being in one corner of the area to render,
# and that temperature is largely uncorrelateed to everything but power draw

# This section I want to see if we can identify perpetually slow/fast GPUs
# ie are some cards consistently slow-/faster than others, regardless of task

# Firstly, are render times normally distributed? (This affects what kinds
# of tests we can use)

benchmarked_tasks %>% 
  filter(dur > 27) %>% 
  filter(eventName == "Rendering") %>% 
  mutate(dur = as.numeric(dur)) %>%
  ggplot() +
  stat_qq(aes(sample = dur)) +
  stat_qq_line(aes(sample = dur))

# By visual inspection, render duration (excluding our corner coords tasks),
# likely isn't normally distributed, this makes intuitive sense since
# there will be some lower bound on render time, but it has a potentially
# infinite upper limit, leading to a right skew (heavy on the left)

# Assessing relationship between card and duration
# Kruskal-Wallis nonparametric one way ANOVA
benchmarked_tasks %>%
  filter(eventName == "Rendering") %>%
  filter(dur > 27) %>% 
  mutate(dur = as.numeric(dur)) %>%
  kruskal.test(dur ~ hostname, data = .)

# Kruskal-Wallis rank sum test
# 
# data:  dur by hostname
# Kruskal-Wallis chi-squared = 12426, df = 1023, p-value < 2.2e-16

# We see strong evidence of render duration being dependant on gpu card

# Next we want to identify individual cards.
# 
# Due to the non-Normal distribution of the render times, comparing 
# individual medians rather than means makes more sense as it's less
# sensitive to outliers caused by particularly long tasks
# 
# We can also exploit a statistics trick to see if they are meaningfuly
# different from the group median, if we have to confidence intervals
# (both using the same confidence level) and find that they don't overlap
# Then we can say they are statistically significantly 
# different at that confidence level

# Find nonparametric 95% CI for render time median per GPU
render_cis = checkps_start_stop %>%
  filter(eventName == "Rendering") %>%
  filter(dur > 27) %>% 
  mutate(dur = as.numeric(dur)) %>%
  group_by(hostname) %>%
  summarise(ci_lower = wilcox.test(dur, conf.int = TRUE, 
                                   conf.level = 0.95)$conf.int[1],
            ci_upper = wilcox.test(dur, conf.int = TRUE, 
                                   conf.level = 0.95)$conf.int[2]) %>%
  ungroup() %>% 
  as.data.frame()

# Find nonparametric 95% CI for render time median overall
overall_render_ci = checkps_start_stop %>%
  filter(eventName == "Rendering") %>%
  filter(dur > 27) %>% 
  mutate(dur = as.numeric(dur)) %>%
  summarise(overall_lower = wilcox.test(dur, conf.int = TRUE, 
                                        conf.level = 0.95)$conf.int[1],
            overall_upper = wilcox.test(dur, conf.int = TRUE, 
                                        conf.level = 0.95)$conf.int[2]) %>%
  as.data.frame()

# Use non overlapping CIs to find significantly faster or slower GPUs
fastest = render_cis %>%
  filter(ci_upper < overall_render_ci$overall_lower) %>%
  select(hostname)

slowest = render_cis %>%
  filter(ci_lower > overall_render_ci$overall_upper) %>%
  select(hostname)

# Hartigan's test for unimodality
checkps2 %>%
  filter(eventName == "Rendering") %>%
  mutate(dur = as.numeric(dur)) %>%
  group_by(hostname) %>%
  summarise(dtp = dip.test(dur)$p.value) %>%
  filter(dtp < 0.05)
