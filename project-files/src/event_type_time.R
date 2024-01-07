# Which event types dominate task runtime?
# 
# Initial guess: Rendering takes the most time across all cards

# summary stats of rendering percentage
benchmarked_tasks %>%
  group_by(taskId) %>%
  filter(eventName == "Total" | eventName == "Rendering") %>%
  arrange(eventName) %>%
  summarise(ren_perc = nth(as.numeric(dur), 1) / nth(as.numeric(dur), 2)) %>%
  ungroup() %>%
  summarise(max = max(ren_perc), p95 = quantile(ren_perc, 0.95),
            p75 = quantile(ren_perc, 0.75), med = median(ren_perc),
            mean = mean(ren_perc), p25 = quantile(ren_perc, 0.25),
            p10 = quantile(ren_perc, 0.10), p05 = quantile(ren_perc, 0.05),
            min = min(ren_perc))

# A tibble: 1 × 9
# max   p95   p75   med   mean  p25   p10   p05   min
# 0.988 0.980 0.977 0.975 0.969 0.972 0.969 0.965 0.350
# 
# 95% of times rendering takes at least 96.5% of the total time per task,
# However this can fall as low as 35%

# library(ggbeeswarm)

benchmarked_tasks %>% 
  filter(eventName == "Rendering") %>% 
  ggplot() + 
  # geom_quasirandom(aes(x = eventName, y = dur), alpha = 0.1) +  # would be nice to beeswarm but just too many points, ends up looking coloured in
  geom_violin(aes(x = eventName, y = dur), colour="1", fill = NA)+
  scale_y_continuous(breaks = c(4:16)*5) + 
  geom_hline(yintercept = 24:30, alpha = 0.5) +
  coord_flip()

# By visual inspection we can see two clear clumps of times spent rendering
# Adding lines means we can visually see that the lower clump seems to end around 25 secs and the upper begins just under 29

# Investigating possible reasons - assessing event dur times between fast and slow cards
benchmarked_tasks %>% 
  select(hostname, taskId, dur, eventName) %>% 
  pivot_wider(names_from = eventName, values_from = dur) %>% 
  mutate(categ = if_else(Rendering < 25, "low_ren", "norm_ren")) %>% 
  pivot_longer(cols = c("Total", "Rendering", "Uploading", "Tiling", "Idle", "Configuring"),
               names_to = "eventName",
               values_to = "dur") %>% 
  filter(! is.na(categ)) %>%   # filter when gpus are idle before start and after end of batch (dur is non finite due to missing a start or an end)
  ggplot() +
  geom_boxplot(aes(x = eventName, y = dur)) +
  facet_wrap(vars(categ))

# By visual inspection, some tasks seem to render practically identically
# Are they just quick tasks or are the cards performing them abnormally good and consistent?

# Hypo: if quick renders are card dependant, splitting the set into two groups and seeing which cards appear in both will be a small proportion,
# alternatively, if they are not card dependant, they will be randomly distributed into each group, and the overlap will be basically everything

benchmarked_tasks %>% 
  select(hostname, taskId, dur, eventName) %>% 
  mutate(categ = if_else(dur < 27, "low_ren", "norm_ren")) %>%  # 27 here is an arbirary split since dientangling bimodal dists is an ongoing area
  filter(eventName == "Rendering") %>% 
  filter(! is.na(categ)) %>% 
  group_by(hostname, categ) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = categ, values_from = n) %>%  # pivot categories ...
  mutate(just_low = cumsum((! is.na(low_ren)) & (is.na(norm_ren))),  # ... and use cumsum to count occurances
         just_norm = cumsum((is.na(low_ren)) & (! is.na(norm_ren))),
         both_low_norm = cumsum((! is.na(low_ren)) & (! is.na(norm_ren)))) %>% 
  summarise(just_low = last(just_low),
            just_norm = last(just_norm),
            both_low_norm = last(both_low_norm))

# A tibble: 1 × 3
# just_low just_norm both_low_norm
#        0        93           931

# As theorised, cards would either divide between categories or appear in both
# In this case they appeared in both so we can reasonably reject the theory that 
# some cards are abnormally fast/quitting after some time period

# New theory: renders that are abnormally quick are taking place 
# on the edge of the rendered area

temp = task_x_y %>% 
  right_join(., benchmarked_tasks, 
             by=c("taskId"), relationship = "one-to-many") %>%
  filter(eventName == "Rendering") %>%
  filter(level == 12) %>% 
  select(x, y, dur) %>% 
  mutate(dur = as.numeric(dur))

temp %>% 
  ggplot() + 
  geom_point(aes(x = x, y = y, colour = dur), alpha = 0.1)

# Visual inspecition would implly that the lower left corner (low x, low y)
# were our quick renders

summary(lm(dur ~ x + y, data = temp))

rm(temp)

# Call:
#   lm(formula = dur ~ x + y, data = temp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -17.769  -3.373   0.253   3.786  41.117 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.746e+01  6.013e-02  623.08   <2e-16 ***
#   x           1.105e-02  3.086e-04   35.80   <2e-16 ***
#   y           1.829e-02  3.086e-04   59.29   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.837 on 65533 degrees of freedom
# Multiple R-squared:  0.06821,	Adjusted R-squared:  0.06818 
# F-statistic:  2399 on 2 and 65533 DF,  p-value: < 2.2e-16

# And linear regression of dur on x and y confirms that our coords
# are highly significant in predicting the time taken