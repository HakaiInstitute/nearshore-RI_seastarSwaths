#'================== Introduction =============================================
#' This script is for producing basic seastar figures for use in reports,
#' documents, and posters.
#' 
#' Author: Tyrel Froese
#' Project: Nearshore
#' Survey: Rocky Intertidal
#' Date Created: 2024-04-12
#' Last Updated: 
#' ============================================================================
#================== Preamble ==================================================
# Load required libraries
library(tidyverse)
library(googlesheets4)
library(rairtable)

source('./scripts/seastar_swathCounts.R')

#================== RI Overview Figures =======================================
ss.over <- ss.sum %>% 
  subset(scientific_name == 'Pisaster ochraceus') %>% 
  select(date, site, scientific_name, total_count)

ss.over <- rbind(ss.over, kc.counts)

pika.abun.PLOT <- ggplot(ss.over, 
                         aes(x = date, y = total_count, 
                             colour = scientific_name)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        strip.text = element_text(size = 12, face = 'bold', vjust = 0.8),
        legend.text = element_text(size = 12)) +
  labs(title = expression(paste('Abundance of', 
                                italic(' Pisaster ochraceus'),
                                ' & ',
                                italic('Katharina tunicata'))),
       colour = 'Species') +
  xlab('Date') + ylab('Total Count') +
  geom_point(size = 3) +
  geom_line(linetype = 'dotted', size = 0.7) +
  scale_color_manual(values=c("black", "purple")) +
  facet_grid(site~.)

#================== Abundance by Season =======================================
ss.over$month <- month(ss.over$date)
ss.over$year <- as.factor(year(ss.over$date))



ss.over$season <- NA_character_

for (i in 1:length(ss.over$date)){
  if (ss.over$month[i] >= 1 & ss.over$month[i] < 3){
    ss.over$season[i] <- 'Late Winter'} 
  else if (ss.over$month[i] >= 3 & ss.over$month[i] < 6){
    ss.over$season[i] <- 'Spring'}
  else if (ss.over$month[i] >= 6 & ss.over$month[i] < 8){
    ss.over$season[i] <- 'Early Summer'}
  else if (ss.over$month[i] >= 8 & ss.over$month[i] < 10){
    ss.over$season[i] <- 'Late Summer'}
  else{ss.over$season[i] <- 'Early Winter' }}

ss.over$season <- factor(ss.over$season, levels = c('Late Winter',
                                                    'Spring',
                                                    'Early Summer',
                                                    'Late Summer',
                                                    'Early Winter'))

ss.pioc <- ss.over %>% 
  subset(scientific_name == 'Pisaster ochraceus') %>% 
  group_by(year, season, site) %>% 
  summarise(n = n(),
            avg_count = mean(total_count),
            sd = sd(total_count),
            se = sd/sqrt(n))

ggplot(ss.pioc, aes(x = season, y = avg_count)) +
  ggtitle('Average abundance of Pisaster ochraceus by Season') +
  theme_bw() +
  geom_point(colour = 'red') +
  geom_errorbar(aes(ymax = avg_count+se,
                    ymin = avg_count-se),
                width = 0,
                colour = 'red') +
  facet_grid(site~.)

# Dermasterias
ss.deim <- ss.over %>% 
  subset(scientific_name == 'Dermasterias imbricata') %>% 
  group_by(season, site) %>% 
  summarise(n = n(),
            avg_count = mean(total_count),
            sd = sd(total_count),
            se = sd/sqrt(n))

ggplot(ss.deim, aes(x = season, y = avg_count)) +
  ggtitle('Average abundance of Dermasterias imbricata by Season') +
  theme_bw() +
  geom_point(colour = 'red') +
  geom_errorbar(aes(ymax = avg_count+se,
                    ymin = avg_count-se),
                width = 0,
                colour = 'red') +
  facet_grid(site~.)

#================== Size Distributions ========================================
# Select relevant colmns and bind together
ss.size <- ss.size %>% 
  select(date, site, scientific_name, size_class, total_count)
                                                  
kc.sizes <- kc.sizes %>% 
  select(date, site, scientific_name, size_class, count)

names(kc.sizes)[names(kc.sizes) == 'count'] <- 'total_count'

ss.size <- rbind(ss.size, kc.sizes)

# Add seasons -----------------------------------------------------------------
ss.size$month <- month(ss.size$date)
ss.size$year <- as.factor(year(ss.size$date))

ss.size$season <- NA_character_

for (i in 1:length(ss.size$date)){
  if (ss.size$month[i] >= 1 & ss.size$month[i] < 3){
    ss.size$season[i] <- 'Late Winter'} 
  else if (ss.size$month[i] >= 3 & ss.size$month[i] < 6){
    ss.size$season[i] <- 'Spring'}
  else if (ss.size$month[i] >= 6 & ss.size$month[i] < 8){
    ss.size$season[i] <- 'Early Summer'}
  else if (ss.size$month[i] >= 8 & ss.size$month[i] < 10){
    ss.size$season[i] <- 'Late Summer'}
  else{ss.size$season[i] <- 'Early Winter' }}

ss.size$season <- factor(ss.size$season, levels = c('Late Winter',
                                                    'Spring',
                                                    'Early Summer',
                                                    'Late Summer',
                                                    'Early Winter'))

# Size distribution plots -----------------------------------------------------
# Pisaster - Fifth Beach
ggplot(subset(ss.size, scientific_name == 'Pisaster ochraceus' 
              & site == 'Fifth Beach'),
       aes(x = size_class, y = total_count, colour = year)) +
  geom_point() +
  geom_line() +
  facet_grid(year~season)

ggplot(subset(ss.size, scientific_name == 'Pisaster ochraceus'),
       aes(x = date, y = size_class, size = total_count)) +
  geom_point() +
  facet_grid(site~.)
