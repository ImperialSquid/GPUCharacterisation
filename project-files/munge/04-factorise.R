# Manually define factor levels and relabel for neatness

checkps = checkps %>%
  mutate(eventName = 
           factor(eventName, 
                  levels = c("TotalRender", "Saving Config", "Render", "Tiling", "Uploading"), 
                  labels = c("Total", "Configuring", "Rendering", "Tiling", "Uploading")))

