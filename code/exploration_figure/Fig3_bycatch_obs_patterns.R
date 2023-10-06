

#################################
### clean working environment ###
#################################
rm(list = ls())

########################
### Load in packages ###
#######################
library(tidyverse)

####################
### Read in data ###
####################

#### total merge: Keep original depth from 1980s data ####

total_merge <- read.csv("data/confidential/processed/fig3_merge_obs_and_trip_data.csv")

################################
### set up figure background ###
################################

base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    legend.position = "none",
                    legend.background = element_rect(fill=alpha('blue', 0)),
                    plot.tag =element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))


#### Sensitive Species ####
#### Only find Harbor seal and California Sea lion from 1980's data ####
#### What about Common murre and Harbor porpoise?####

#####################
#### Format Data ####
#####################

#### BPUE & proportion sets - distance ####

# add distance category to total_merge
total_merge_dist <- total_merge %>%
  filter(!is.na(dist_km)) %>%
  mutate(dist_catg = ifelse(data_source == "CDFW(1983-1989)",
                            cut(dist_km, breaks = seq(0,41,0.5), label = seq(0,40.5, 0.5), right = F) %>% as.character() %>% as.numeric(),
                            cut(dist_km, breaks = seq(0,80,1), label = seq(0, 79, 1), right = F) %>% as.character() %>% as.numeric()))


# Find out how many sets in each unique combination of data source and dist_catg
number_sets_bpue_dist <- total_merge_dist %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = FALSE)

# Find out how many summed efforts in each unique combination of data source and dist_catg
number_effort_bpue_dist <- total_merge_dist %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, dist_catg) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = TRUE)

# Find out how many sealion bycatch in each unique combination of data source and dist_catg
number_sealion_bpue_dist <- total_merge_dist %>%
  filter(comm_name == "California sea lion") %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_sealion = sum(n_discarded_total), set_sealion = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = TRUE)

# Find out how many harbor seal bycatch in each unique combination of data source and dist_catg
number_seal_bpue_dist <- total_merge_dist %>%
  filter(comm_name == "Harbor seal") %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_seal = sum(n_discarded_total), set_seal = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = TRUE)

merge_bpue_dist <- merge(number_sets_bpue_dist, number_effort_bpue_dist, by = "unique_id", all.x = TRUE) %>%
  merge(number_sealion_bpue_dist, by = "unique_id", all.x = TRUE) %>%
  merge(number_seal_bpue_dist, by = "unique_id", all.x = TRUE) %>%
  mutate(bpue_sealion = number_sealion/number_efforts) %>%
  mutate(bpue_seal = number_seal/number_efforts) %>%
  mutate(proportion_sealion = set_sealion/number_sets) %>%
  mutate(proportion_seal = set_seal/number_sets) %>%
  mutate_all(~replace(., is.na(.), 0))

#### BPUE & proportion sets - depth ####

number_sets_bpue_depth <- total_merge %>%
  filter(!is.na(haul_depth_fa)) %>%
  group_by(data_source, haul_depth_fa) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, haul_depth_fa), sep = "-", remove = FALSE)

number_effort_bpue_depth <- total_merge %>%
  filter(!is.na(haul_depth_fa)) %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, haul_depth_fa) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, haul_depth_fa) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, haul_depth_fa), sep = "-", remove = TRUE)

number_sealion_bpue_depth <- total_merge %>%
  filter(comm_name == "California sea lion") %>%
  group_by(data_source, haul_depth_fa) %>%
  summarize(number_sealion = sum(n_discarded_total), set_sealion = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, haul_depth_fa), sep = "-", remove = TRUE)

number_seal_bpue_depth <- total_merge %>%
  filter(comm_name == "Harbor seal") %>%
  group_by(data_source, haul_depth_fa) %>%
  summarize(number_seal = sum(n_discarded_total), set_seal = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, haul_depth_fa), sep = "-", remove = TRUE)

merge_bpue_depth <- merge(number_sets_bpue_depth, number_effort_bpue_depth, by = "unique_id", all.x = TRUE) %>%
  merge(number_sealion_bpue_depth, by = "unique_id", all.x = TRUE) %>%
  merge(number_seal_bpue_depth, by = "unique_id", all.x = TRUE) %>%
  mutate(bpue_sealion = number_sealion/number_efforts) %>%
  mutate(bpue_seal = number_seal/number_efforts) %>%
  mutate(proportion_sealion = set_sealion/number_sets) %>%
  mutate(proportion_seal = set_seal/number_sets) %>%
  mutate_all(~replace(., is.na(.), 0))


#### BPUE & proportion sets - lat ####

total_merge_latitude <- total_merge %>%
  filter(!is.na(haul_lat_dd)) %>%
  mutate(lat_catg = cut(haul_lat_dd, breaks = seq(32.4, 35.7,0.1), 
                        label = seq(32.4, 35.6, 0.1), right = F) %>% as.character() %>% as.numeric())

number_sets_bpue_lat <- total_merge_latitude %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = FALSE)

number_effort_bpue_lat <- total_merge_latitude %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, lat_catg) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = TRUE)

number_sealion_bpue_lat <- total_merge_latitude %>%
  filter(comm_name == "California sea lion") %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_sealion = sum(n_discarded_total), set_sealion = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = TRUE)

number_seal_bpue_lat <- total_merge_latitude %>%
  filter(comm_name == "Harbor seal") %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_seal = sum(n_discarded_total), set_seal = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = TRUE)

merge_bpue_lat <- merge(number_sets_bpue_lat, number_effort_bpue_lat, by = "unique_id", all.x = TRUE) %>%
  merge(number_sealion_bpue_lat, by = "unique_id", all.x = TRUE) %>%
  merge(number_seal_bpue_lat, by = "unique_id", all.x = TRUE) %>%
  mutate(bpue_sealion = number_sealion/number_efforts) %>%
  mutate(bpue_seal = number_seal/number_efforts) %>%
  mutate(proportion_sealion = set_sealion/number_sets) %>%
  mutate(proportion_seal = set_seal/number_sets) %>%
  mutate_all(~replace(., is.na(.), 0))

#### BPUE & proportion sets - Julian days ####

total_merge_jd <- total_merge %>%
  mutate(jd_catg = cut(julian_day, breaks = seq(3, 372, 7), 
                       label = seq(3, 365, 7), right = F) %>% as.character() %>% as.numeric())

number_sets_bpue_jd <- total_merge_jd %>%
  group_by(data_source, jd_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, jd_catg), sep = "-", remove = FALSE) %>%
  filter(!is.na(jd_catg))

number_effort_bpue_jd <- total_merge_jd %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, jd_catg) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, jd_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, jd_catg), sep = "-", remove = TRUE)

number_sealion_bpue_jd <- total_merge_jd %>%
  filter(comm_name == "California sea lion") %>%
  group_by(data_source, jd_catg) %>%
  summarize(number_sealion = sum(n_discarded_total), set_sealion = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, jd_catg), sep = "-", remove = TRUE)

number_seal_bpue_jd <- total_merge_jd %>%
  filter(comm_name == "Harbor seal") %>%
  group_by(data_source, jd_catg) %>%
  summarize(number_seal = sum(n_discarded_total), set_seal = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, jd_catg), sep = "-", remove = TRUE)

merge_bpue_jd <- merge(number_sets_bpue_jd, number_effort_bpue_jd, by = "unique_id", all.x = TRUE) %>%
  merge(number_sealion_bpue_jd, by = "unique_id", all.x = TRUE) %>%
  merge(number_seal_bpue_jd, by = "unique_id", all.x = TRUE) %>%
  mutate(bpue_sealion = number_sealion/number_efforts) %>%
  mutate(bpue_seal = number_seal/number_efforts) %>%
  mutate(proportion_sealion = set_sealion/number_sets) %>%
  mutate(proportion_seal = set_seal/number_sets) %>%
  mutate_all(~replace(., is.na(.), 0))

###BPUE & proportion sets - mesh size inches###
total_merge_ms_ref <- total_merge %>%
  pull(net_mesh_size_in) %>%
  unique() %>%
  sort() %>%
  as.character()

total_merge_ms <- total_merge %>%
  filter(!is.na(net_mesh_size_in)) %>%
  mutate(net_mesh_size_in=factor(net_mesh_size_in, levels=total_merge_ms_ref))
  #mutate(ms_catg = cut(net_mesh_size_in, breaks = seq(0, 22, 0.5), 
                       #label = seq(0, 21.5, 0.5), right = F) %>% as.ch


number_sealion_bpue_ms <- total_merge_ms %>%
  filter(comm_name == "California sea lion") %>%
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  mutate(bpue_sealion = n_caught/effort)
  
number_seal_bpue_ms <- total_merge_ms %>%
  filter(comm_name == "Harbor seal") %>%
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  mutate(bpue_seal = n_caught/effort)

#### BPUE & proportion sets - soak hours ####

total_merge_sh <- total_merge %>%
  filter(!is.na(soak_hr)) %>%
  mutate(sh_catg = cut(soak_hr, breaks = seq(0, 240, 6), 
                       label = seq(0, 234, 6), right = F) %>% as.character() %>% as.numeric())


number_sets_bpue_sh <- total_merge_sh %>%
  group_by(data_source, sh_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, sh_catg), sep = "-", remove = FALSE)

number_effort_bpue_sh <- total_merge_sh %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, sh_catg) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, sh_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, sh_catg), sep = "-", remove = TRUE)

number_sealion_bpue_sh <- total_merge_sh %>%
  filter(comm_name == "California sea lion") %>%
  group_by(data_source, sh_catg) %>%
  summarize(number_sealion = sum(n_discarded_total), set_sealion = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, sh_catg), sep = "-", remove = TRUE)

number_seal_bpue_sh <- total_merge_sh %>%
  filter(comm_name == "Harbor seal") %>%
  group_by(data_source, sh_catg) %>%
  summarize(number_seal = sum(n_discarded_total), set_seal = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, sh_catg), sep = "-", remove = TRUE)

merge_bpue_sh <- merge(number_sets_bpue_sh, number_effort_bpue_sh, by = "unique_id", all.x = TRUE) %>%
  merge(number_sealion_bpue_sh, by = "unique_id", all.x = TRUE) %>%
  merge(number_seal_bpue_sh, by = "unique_id", all.x = TRUE) %>%
  mutate(bpue_sealion = number_sealion/number_efforts) %>%
  mutate(bpue_seal = number_seal/number_efforts) %>%
  mutate(proportion_sealion = set_sealion/number_sets) %>%
  mutate(proportion_seal = set_seal/number_sets) %>%
  filter(!is.na(sh_catg)) %>%
  filter(!is.na(number_efforts)) %>%
  mutate_all(~replace(., is.na(.), 0))


#### CPUE - dist ####

total_merge_dist_cpue <- total_merge %>%
  mutate(dist_catg = ifelse(dist_km <= 10,
                            cut(dist_km, breaks = seq(0,10.5,0.5), label = seq(0, 10, 0.5), right = F) %>% as.character() %>% as.numeric(),
                            cut(dist_km, breaks = seq(10,80,5), label = seq(10, 75, 5), right = F) %>% as.character() %>% as.numeric()))

number_sets_dist_cpue <- total_merge_dist_cpue %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = FALSE)

number_effort_dist_cpue <- total_merge_dist_cpue %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, dist_catg) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = TRUE)

number_retained_dist_cpue <- total_merge_dist_cpue %>%
  # filter out NA n_retained species
  filter(!is.na(n_retained)) %>%
  group_by(data_source, dist_catg) %>%
  summarize(number_retained = sum(n_retained)) %>%
  unite (unique_id, c(data_source, dist_catg), sep = "-", remove = TRUE)

dist_cpue <-  merge(number_sets_dist_cpue, number_effort_dist_cpue, by = "unique_id", all.x = TRUE) %>%
  merge(number_retained_dist_cpue, by = "unique_id", all.x = TRUE) %>%
  mutate(cpue = number_retained/number_efforts) %>%
  # filter out NA cpue value
  filter(!is.na(cpue))


#### CPUE - Haul depth ####

total_merge_depth_cpue <- total_merge %>%
  mutate(depth_catg = ifelse(haul_depth_fa <= 20,
                             cut(haul_depth_fa, breaks = seq(0,22,2), label = seq(0, 20, 2), right = F) %>% as.character() %>% as.numeric(),
                             cut(haul_depth_fa, breaks = seq(20,460,40), label = seq(20, 420, 40), right = F) %>% as.character() %>% as.numeric()))

number_sets_depth_cpue <- total_merge_depth_cpue %>%
  filter(!is.na(haul_depth_fa)) %>%
  group_by(data_source, depth_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, depth_catg), sep = "-", remove = FALSE)

number_effort_depth_cpue <- total_merge_depth_cpue %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, depth_catg) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, depth_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, depth_catg), sep = "-", remove = TRUE)

number_retained_depth_cpue <- total_merge_depth_cpue %>%
  # filter out NA n_retained species
  filter(!is.na(n_retained)) %>%
  group_by(data_source, depth_catg) %>%
  summarize(number_retained = sum(n_retained)) %>%
  unite (unique_id, c(data_source, depth_catg), sep = "-", remove = TRUE)

depth_cpue <-  merge(number_sets_depth_cpue, number_effort_depth_cpue, by = "unique_id", all.x = TRUE) %>%
  merge(number_retained_depth_cpue, by = "unique_id", all.x = TRUE) %>%
  mutate(cpue = number_retained/number_efforts) %>%
  filter(!is.na(cpue))

#### CPUE - lat ####

total_merge_latitude_cpue <- total_merge %>%
  mutate(lat_catg = cut(haul_lat_dd, breaks = seq(32.4, 35.7,0.1), 
                        label = seq(32.4, 35.6, 0.1), right = F) %>% as.character() %>% as.numeric())

number_sets_lat_cpue <- total_merge_latitude_cpue %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = FALSE)

number_effort_lat_cpue <- total_merge_latitude_cpue %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, lat_catg) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = TRUE)

number_retained_lat_cpue <- total_merge_latitude_cpue %>%
  # filter out NA n_retained species
  filter(!is.na(n_retained)) %>%
  group_by(data_source, lat_catg) %>%
  summarize(number_retained = sum(n_retained)) %>%
  unite (unique_id, c(data_source, lat_catg), sep = "-", remove = TRUE)

lat_cpue <-  merge(number_sets_lat_cpue, number_effort_lat_cpue, by = "unique_id", all.x = TRUE) %>%
  merge(number_retained_lat_cpue, by = "unique_id", all.x = TRUE) %>%
  mutate(cpue = number_retained/number_efforts) %>%
  filter(!is.na(cpue))

#### CPUE - Julian days ####

total_merge_jd_cpue <- total_merge %>%
  mutate(jd_catg = cut(julian_day, breaks = seq(3, 372, 7), 
                       label = seq(3, 365, 7), right = F) %>% as.character() %>% as.numeric())

number_sets_jd_cpue <- total_merge_jd_cpue %>%
  group_by(data_source, jd_catg) %>%
  summarize(number_sets = n_distinct(set_id)) %>%
  unite (unique_id, c(data_source, jd_catg), sep = "-", remove = FALSE)


number_effort_jd_cpue <- total_merge_jd_cpue %>%
  filter(!is.na(net_length_fa)) %>%
  filter(!is.na(soak_hr)) %>%
  select(set_id, net_length_fa, soak_hr, data_source, jd_catg) %>%
  # turn units from hrs*fa into day*miles
  mutate(effort = soak_hr*net_length_fa * (1/(880*24))) %>%
  filter(!duplicated(set_id)) %>%
  group_by(data_source, jd_catg) %>%
  summarize(number_efforts = sum(effort)) %>%
  unite (unique_id, c(data_source, jd_catg), sep = "-", remove = TRUE)

number_retained_jd_cpue <- total_merge_jd_cpue %>%
  # filter out NA n_retained species
  filter(!is.na(n_retained)) %>%
  group_by(data_source, jd_catg) %>%
  summarize(number_retained = sum(n_retained)) %>%
  unite (unique_id, c(data_source, jd_catg), sep = "-", remove = TRUE)

jd_cpue <-  merge(number_sets_jd_cpue, number_effort_jd_cpue, by = "unique_id", all.x = TRUE) %>%
  merge(number_retained_jd_cpue, by = "unique_id", all.x = TRUE) %>%
  mutate(cpue = number_retained/number_efforts) %>%
  filter(!is.na(cpue))


###########################
#### Make plots - BPUE ####
###########################

# distance to shore

g_sl_bpue_dist <- ggplot(data = merge_bpue_dist, aes(x=dist_catg, y = bpue_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 3), x = c(0, 20)) +
  scale_size_continuous(lim = c(0, 500)) +
  labs(y = "Sea lion bycatch rate\n(Sea lion per mile-day)") +
  #labels
  theme_bw() + base_theme + theme(axis.title.x = element_blank())

g_sl_bpue_dist
  

g_hs_bpue_dist <- ggplot(data = merge_bpue_dist, aes(x=dist_catg, y = bpue_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 1), x = c(0, 20)) +
  scale_size_continuous(lim = c(0, 500)) +
  labs(x = "Distance to shore (km)", y = "Seal bycatch rate\n(Seal per mile-day)") +
  theme_bw() + base_theme

g_hs_bpue_dist

# haul depth

g_sl_bpue_depth <- ggplot(data = merge_bpue_depth, aes(x=haul_depth_fa, y = bpue_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 5), x = c(0, 50)) +
  scale_size_continuous(lim = c(0, 1000)) +
  theme_bw() + base_theme + theme(axis.title.x = element_blank(), axis.title.y = element_blank())


g_sl_bpue_depth

g_hs_bpue_depth <- ggplot(data = merge_bpue_depth, aes(x=haul_depth_fa, y = bpue_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 5), x = c(0, 50)) +
  scale_size_continuous(lim = c(0, 1000)) +
  labs(x = "Haul depth (fathoms)") +
  theme_bw() + base_theme + theme(axis.title.y = element_blank())

g_hs_bpue_depth

# latitude
g_sl_bpue_lat <- ggplot(data = merge_bpue_lat, aes(x=lat_catg, y = bpue_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 2), x = c(32, 35)) +
  scale_size_continuous(lim = c(0, 500)) +
  theme_bw() + base_theme + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
g_sl_bpue_lat

g_hs_bpue_lat <- ggplot(data = merge_bpue_lat, aes(x= lat_catg, y = bpue_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.5), x = c(32, 35)) +
  scale_size_continuous(lim = c(0, 500)) +
  labs(x = "Latitude (°N)") +
  theme_bw() + base_theme + theme(axis.title.y = element_blank())
g_hs_bpue_lat

# julian days

g_sl_bpue_jd <- ggplot(data = merge_bpue_jd, aes(x=jd_catg, y = bpue_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  #lims(y = c(0, 2.5)) +
  scale_size_continuous(lim = c(0, 500), breaks = seq(100, 500, 100)) +
  scale_fill_discrete(guide = "none") +
  labs(size = "Number of sets") +
  theme_bw() + base_theme + theme(axis.title.x = element_blank(), 
                                  axis.title.y = element_blank(),
                                  legend.position = c(0.7, 0.7),
                                  legend.key.size = unit(0.1, "cm"))


g_sl_bpue_jd

g_hs_bpue_jd <- ggplot(data = merge_bpue_jd, aes(x= jd_catg, y = bpue_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 1)) +
  scale_size_continuous(lim = c(0, 500), guide = "none") +
  labs(x = "Julian days", fill = "Data source") +
  theme_bw() + base_theme + theme(axis.title.y = element_blank(),
                                  legend.position = c(0.7, 0.88),
                                  legend.key.size = unit(0.1, "cm"))

g_hs_bpue_jd


# mesh size

g_sl_bpue_ms <- ggplot(data = number_sealion_bpue_ms, aes(x = net_mesh_size_in, y = bpue_sealion, fill = data_source))+
  geom_boxplot() +
  scale_y_continuous(trans = "log10") +
  theme_bw() + base_theme


g_sl_bpue_ms

g_hs_bpue_ms <- ggplot(data = number_seal_bpue_ms, aes(x = net_mesh_size_in, y = bpue_seal, fill = data_source)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log10") +
  theme_bw() + base_theme

g_hs_bpue_ms


# soak hour

g_sl_bpue_sh <- ggplot(data = merge_bpue_sh, aes(x = sh_catg, y = bpue_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 5)) +
  scale_size_continuous(lim = c(0, 2000), guide = "none") +
  theme_bw() + base_theme

g_sl_bpue_sh

g_hs_bpue_sh <- ggplot(data = merge_bpue_sh, aes(x = sh_catg, y = bpue_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 2)) +
  scale_size_continuous(lim = c(0, 2000), guide = "none") +
  theme_bw() + base_theme

g_hs_bpue_sh

#### Merge Plots - BPUE ####

layout_matrix <- matrix(data=c(1, 2, 3, 4,
                               5, 6, 7, 8), ncol=4, byrow=T)

g_bpue <- gridExtra::grid.arrange(g_sl_bpue_dist, g_sl_bpue_depth, g_sl_bpue_lat, g_sl_bpue_jd, g_hs_bpue_dist, g_hs_bpue_depth, g_hs_bpue_lat, g_hs_bpue_jd,
                                  layout_matrix=layout_matrix)
g_bpue

#### Save BPUE plots ####
ggsave(g_bpue, filename= file.path("Figures", "Fig3(A)_merged_obs_BPUE.png"),
       width = 7, height = 4.5, units = "in", dpi = 600)

######################################
#### Make plots - proportion sets ####
######################################

g_sl_pro_dist <- ggplot(data = merge_bpue_dist, aes(x=dist_catg, y = proportion_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.2), x = c(0, 20)) +
  scale_size_continuous(lim = c(0, 500)) +
  labs(y = "Sealion bycatch proportion (sets)") +
  theme_bw() + base_theme + theme(axis.title.x = element_blank())

g_sl_pro_dist

g_hs_pro_dist <- ggplot(data = merge_bpue_dist, aes(x=dist_catg, y = proportion_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.1), x = c(0, 20)) +
  scale_size_continuous(lim = c(0, 500)) +
  labs(x = "Distance to shore (km)", y = "Seal bycatch proportion (sets)") +
  theme_bw() + base_theme

g_hs_pro_dist


g_sl_pro_depth <- ggplot(data = merge_bpue_depth, aes(x=haul_depth_fa, y = proportion_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.2), x = c(0, 50)) +
  scale_size_continuous(lim = c(0, 500)) +
  theme_bw() + base_theme + theme(axis.title.x = element_blank(), axis.title.y = element_blank())

g_sl_pro_depth

g_hs_pro_depth <- ggplot(data = merge_bpue_depth, aes(x=haul_depth_fa, y = proportion_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.1), x = c(0, 50))+
  scale_size_continuous(lim = c(0, 500)) +
  labs(x = "Haul depth (fa)") +
  theme_bw() +base_theme + theme(axis.title.y = element_blank())

g_hs_pro_depth

g_sl_pro_lat <- ggplot(data = merge_bpue_lat, aes(x=lat_catg, y = proportion_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.25), x = c(32, 35)) +
  scale_size_continuous(lim = c(0, 500)) +
  theme_bw() +base_theme + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
g_sl_pro_lat

g_hs_pro_lat <- ggplot(data = merge_bpue_lat, aes(x=lat_catg, y = proportion_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.1), x = c(32,35))+
  scale_size_continuous(lim = c(0, 500)) +
  labs(x = "Latitude (N)") +
  theme_bw() +base_theme + theme(axis.title.y = element_blank())
g_hs_pro_lat

g_sl_pro_jd <- ggplot(data = merge_bpue_jd, aes(x=jd_catg, y = proportion_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(y = c(0, 0.3)) +
  scale_size_continuous(lim = c(0, 500)) +
  labs(size = "Number of sets", fill = "Data source") +
  theme_bw() +base_theme + theme(axis.title.x = element_blank(), 
                                    axis.title.y = element_blank(),
                                    legend.position = c(0.8, 0.7),
                                    legend.key.size = unit(0.1, "cm"))

g_sl_pro_jd

g_hs_pro_jd <- ggplot(data = merge_bpue_jd, aes(x=jd_catg, y = proportion_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  scale_size_continuous(lim = c(0, 500)) +
  labs(x = "Julian days") +
  theme_bw() +base_theme + theme(axis.title.y = element_blank())


g_hs_pro_jd

g_sl_pro_sh <- ggplot(data = merge_bpue_sh, aes(x = sh_catg, y = proportion_sealion, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  theme_bw() + base_theme

g_sl_pro_sh

g_hs_pro_sh <- ggplot(data = merge_bpue_sh, aes(x = sh_catg, y = proportion_seal, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  theme_bw() + base_theme

g_hs_pro_sh



#### Merge Plots - proportion sets ####

g_pro <- gridExtra::grid.arrange(g_sl_pro_dist, g_sl_pro_depth, g_sl_pro_lat, g_sl_pro_jd, g_hs_pro_dist, g_hs_pro_depth, g_hs_pro_lat, g_hs_pro_jd,
                                 layout_matrix=layout_matrix)
g_pro

#### Save proportion sets plot ####
ggsave(g_pro, filename= file.path("Figures", "Fig3(B)_merged_obs_pro.png"),
       width = 7, height = 7, units = "in", dpi = 600)



##########################
#### Make plots- CPUE ####
##########################

g_cpue_dist <-  ggplot(data = dist_cpue, aes(x=dist_catg, y = cpue, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(x = c(0, 20), y = c(0, 500)) +
  scale_size_continuous(lim=c(0, 500)) +
  labs(x = "Distance to shore (km)", y = "CPUE") +
  theme_bw() + base_theme 

g_cpue_dist

g_cpue_depth <-  ggplot(data = depth_cpue, aes(x=depth_catg, y = cpue, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  lims(x = c(0, 25), y = c(0, 250)) +
  scale_size_continuous(lim = c(0, 500)) +
  labs(x = "Haul depth (fa)") +
  theme_bw() + base_theme + theme(axis.title.y = element_blank())

g_cpue_depth


g_cpue_lat <-  ggplot(data = lat_cpue, aes(x=lat_catg, y = cpue, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  scale_size_continuous(lim = c(0, 500)) +
  labs(x = "Latitude(N)", y = "CPUE") +
  theme_bw() + base_theme 

g_cpue_lat


g_cpue_jd <-  ggplot(data = jd_cpue, aes(x=jd_catg, y = cpue, fill = data_source, size = number_sets)) +
  geom_point(pch = 21) +
  scale_size_continuous(lim = c(0, 500)) +
  labs(x = "Julian days", size = "Number of sets", fill = "Data source") +
  theme_bw() + base_theme + theme(axis.title.y = element_blank(),
                                  legend.position = c(0.5, 0.6),
                                  legend.key.size = unit(0.1, "cm"))

g_cpue_jd

#### Merge plots of CPUE ####

layout_matrix_cpue <- matrix(data=c(1, 2, 
                                    3, 4), ncol=2, byrow=T)

g_cpue <- gridExtra::grid.arrange(g_cpue_dist, g_cpue_depth, g_cpue_lat, g_cpue_jd,
                                  layout_matrix=layout_matrix_cpue)

#### Save plots of CPUE ####

ggsave(g_cpue, filename= file.path("Figures", "Fig3(C)_merged_obs_cpue.png"),
       width = 7, height = 7, units = "in", dpi = 600)





