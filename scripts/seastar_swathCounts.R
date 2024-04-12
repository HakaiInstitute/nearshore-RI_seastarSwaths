#'================== Introduction ==============================================
#' This script is for transforming and reformatting data from seastar
#' surveys done as part of the Hakai Institute's ongoing Rocky Intertidal 
#' project. The transformed data will be used for various summary figures.
#' 
#' Author: Tyrel Froese
#' Project: Nearshore
#' Survey: Rocky Intertidal
#' Date Created: 2024-04-12
#' Last Updated: 
#' ============================================================================
#================== Preamble ==================================================
library(tidyverse)
library(googlesheets4)

ss <- read_csv('./data/seastar_macroinverts-surveys.csv')

#================== Seastar & Chiton  Swaths 2018- ============================
# Find area--------------------------------------------------------------------
ss.areas <- read_sheet('1QnVrhQ-6-BbmZGkBPaAu0nR3FPIhNAlCmaCgdKMcV24',
                       range = 'Areas') %>% 
  select(site_id, tide, sfcArea_2D, sfcArea_3D)

colnames(ss.areas) <- c('site', 'tide_height_at_sampling_time', 
                        'unit_area_2D', 'unit_area_3D')

# Fill in missing tides
for (i in 1:length(ss$date)){
  if (ss$date[i] == '2018-06-15'){
    ss$tide_height_at_sampling_time[i] <- 0.0
  }
}

for (i in 1:length(ss$date)){
  if (ss$date[i] == '2018-08-12'){
    ss$tide_height_at_sampling_time[i] <- 0.0
  }
}

for (i in 1:length(ss$date)){
  if (ss$date[i] == '2018-08-13'){
    ss$tide_height_at_sampling_time[i] <- 0.2
  }
}

for (i in 1:length(ss$date)){
  if (ss$date[i] == '2019-02-18'){
    ss$tide_height_at_sampling_time[i] <- 0.4
  }
}

for (i in 1:length(ss$date)){
  if (ss$date[i] == '2019-02-19'){
    ss$tide_height_at_sampling_time[i] <- 0.4
  }
}

for (i in 1:length(ss$date)){
  if (ss$date[i] == '2021-05-27'){
    ss$tide_height_at_sampling_time[i] <- 0.0
  }
}

for (i in 1:length(ss$date)){
  if (ss$date[i] == '2021-06-25'){
    ss$tide_height_at_sampling_time[i] <- 0.0
  }
}

for (i in 1:length(ss$date)){
  if (ss$date[i] == '2022-06-15'){
    ss$tide_height_at_sampling_time[i] <- 0.0
  }
}

# Make effective tide height the sampling tide height
for (i in i:length(ss$date)){
  if (ss$date[i] >= '2022-11-24'){
    ss$tide_height_at_sampling_time[i] <- ss$effective_tide_height[i]
  }
}


# Join to seastar data
ss <- left_join(ss, ss.areas)

# Katharina -------------------------------------------------------------------
kc <- ss %>% 
  subset(scientific_name == 'Katharina tunicata')

# Katharina Swaths - Total counts
kc.counts <- kc %>% 
  group_by(date, site, scientific_name) %>% 
  dplyr::summarise(total_count = sum(count)) %>% 
  ungroup()

# Katharina Swaths - Size distributions
kc.sizes <- kc %>% 
  subset(size_class > 0)

# Seastars --------------------------------------------------------------------
ss.sum <- ss %>%
  subset(scientific_name != 'Katharina tunicata') %>% 
  group_by(date, site, unit_area_2D, unit_area_3D, scientific_name, 
           description) %>% 
  dplyr::summarise(total_count = sum(count)) %>% 
  ungroup()

# Seastar sizes
ss.size <- ss %>% 
  drop_na(size_class) %>% 
  subset(scientific_name != 'Katharina tunicata') %>% 
  group_by(date, site, scientific_name, description, size_class) %>% 
  dplyr::summarise(total_count = sum(count)) %>%
  ungroup()

# Abalone ---------------------------------------------------------------------
haka <- ss %>% 
  subset(scientific_name == 'Haliotis kamtschatkana')

haka.sum <- haka %>%
  group_by(date, site, scientific_name) %>% 
  dplyr::summarise(total_count = sum(count)) %>% 
  ungroup()

haka.size <- haka %>% 
  drop_na(size_class) %>% 
  group_by(date, site, scientific_name, size_class) %>% 
  dplyr::summarise(total_count = sum(count)) %>%
  ungroup()