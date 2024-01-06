# Extract hostnames and other columns that don't distinguish rows, 
# assign a simple vm name (can be reversed from vm_names table)

vm_names = gpu %>%
  select(hostname, gpuSerial, gpuUUID) %>%
  mutate(hostname_simple = 
           factor(hostname, levels = unique(hostname), 
                  labels = 
                    paste0("vm", sprintf("%04d", 1:n_distinct(hostname)))
                  )) %>%
  group_by(hostname_simple) %>%
  summarise(hostname = first(hostname),
            gpuSerial = first(gpuSerial),
            gpuUUID = first(gpuUUID), .groups = "drop") %>% 
  as.data.frame()

# vm_names2 %>%
#   group_by(hostname_simple) %>%
#   count(hostname) %>%
#   filter(n() > 1) %>%
#   summarise()
# vm_names2 %>%
#   group_by(hostname_simple) %>%
#   count(gpuSerial) %>%
#   filter(n() > 1) %>%
#   summarise()
# vm_names2 %>%
#   group_by(hostname_simple) %>%
#   count(gpuUUID) %>%
#   filter(n() > 1) %>%
#   summarise()

# replace hostname with simplified hostname
gpu = gpu %>%
  select(-gpuSerial, -gpuUUID) %>%
  left_join(. , vm_names, by = "hostname") %>%
  relocate(hostname_simple, .after = hostname) %>%
  select(-hostname, -gpuSerial, -gpuUUID) %>%
  rename(hostname = hostname_simple) %>%
  as.data.frame()

# replace hostname with simplified hostname
checkps = checkps %>%
  left_join(. , vm_names, by = "hostname") %>%
  relocate(hostname_simple, .after = hostname) %>%
  select(-hostname, -gpuSerial, -gpuUUID) %>%
  rename(hostname = hostname_simple) %>%
  as.data.frame()
