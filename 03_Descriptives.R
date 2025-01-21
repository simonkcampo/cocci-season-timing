library(tidyverse)
library(RColorBrewer)
library(sf)
library(cowplot)
library(ggbrace)
setwd('~/Desktop/Season Timing')
msm.data.census <- read.csv('msm.data.census2023.csv', colClasses = c('GEOID' = 'character'))
msm.data.county <- read.csv('msm.data.county2023.csv')
## Descriptive stats

# number of total seasonal census tract years
msm.data.census %>% dplyr::select(c, epi.start.srm) %>% distinct() %>% dplyr::filter(!is.na(epi.start.srm)) %>% nrow()
msm.data.census %>% group_by(c) %>% dplyr::summarise(cases = sum(N), seasonal = !all(is.na(epi.start.srm))) %>% dplyr::filter(cases >= 2) %>% dplyr::summarise(years = n(), seasonal = sum(seasonal))
msm.data.census %>% dplyr::select(c, county1, epi.start.srm) %>% distinct() %>% dplyr::filter(!is.na(epi.start.srm)) %>% 
  group_by(county1) %>% summarise(N = n()) %>% arrange(-N)

# Comparison of timing estimation methods
comp <- msm.data.county %>% dplyr::select(c, epi.start.srm, epi.end.srm, epi.duration.srm,
                                          epi.start.mcm, epi.end.mcm, epi.duration.mcm) %>% distinct()

msm.data.census %>% group_by(tyear) %>% dplyr::summarize(mean_onset = mean(epi.start.srm, na.rm = T),
                                                  mean_end = mean(epi.end.srm, na.rm = T),
                                                  mean_duration = mean(epi.duration.srm, na.rm = T)) %>% arrange(-mean_duration)

msm.data.census %>% group_by(county1) %>% dplyr::summarize(mean_onset = mean(epi.start.srm, na.rm = T),
                                                  mean_end = mean(epi.end.srm, na.rm = T),
                                                  mean_duration = mean(epi.duration.srm, na.rm = T)) %>% arrange(-mean_duration)


### Variability in season timing figure ######
msm.data.census <- msm.data.census %>% rename(onset.srm = epi.start.srm, end.srm = epi.end.srm)
msm.data.census %>% dplyr::select(onset.srm, end.srm) %>% distinct() %>% 
  pivot_longer(cols = c(onset.srm, end.srm), values_to = 'time', names_to = 'transition') %>%
  mutate(transition = factor(case_when(transition == 'onset.srm'~ 'Season Onset',
                                transition == 'end.srm' ~ 'Season End'),
                             levels = c('Season Onset', 'Season End'))) %>% 
  group_by(transition) %>% mutate(mean = mean(time, na.rm = T)) %>% ungroup() %>%
  ggplot() + 
    geom_vline(aes(xintercept = mean, col = transition), linewidth = 1.5, linetype = 'dashed') +
  geom_histogram(aes(time, fill = transition),alpha = 0.8, position = 'identity') + 
  scale_x_continuous(breaks = seq(0,53, 53/12)[-13], labels = month.abb[c(4:12,1:3)],
                     limits = c(2,54)) + 
  scale_y_continuous(breaks = c(0,50,100,150), labels = c(' ',' ',' ',' ')) +
  scale_color_manual(values = c('#1b9e77','#7570b3')) +
  scale_fill_manual(values = c('#1b9e77','#7570b3')) +
  labs(x = 'Month', y = ' ', fill = NULL, col = NULL) + 
  theme_bw() + 
  theme(text = element_text(size = 20),
        axis.ticks.y = element_blank())

ggsave('~/Desktop/CSG Figures/timing_means.png', width = 9, height = 6, units = 'in', dpi = 400)

# Fig 1
cts <- tigris::tracts(state = '06', year = 2017)
GEOIDs_in_div <- read.csv('Data/GEOIDs_in_Divided_Counties.csv',
                          colClasses = c('GEOID' = 'character'))
included_counties <- c('WFresno','WKern','Kings','WTulare','EKern','WMadera', 
                       'Monterey', 'San Luis Obispo', 'Santa Barbara', 'Ventura',
                       'Merced','San Joaquin','Stanislaus','NLA')
counties <- dplyr::filter(cts, GEOID %in% GEOIDs_in_div$GEOID) %>% 
  dplyr::filter(GEOID != '06083980100',GEOID != '06083990000', GEOID != '06111003612', GEOID != '06111980000') %>%
  dplyr::filter(ALAND != 0) %>% left_join(GEOIDs_in_div, by = 'GEOID') %>% 
  dplyr::filter(county1 %in% included_counties) %>% group_by(county1) %>% dplyr::summarize(geometry = st_union(geometry))


counties <- counties %>% mutate(Region = case_when(county1 %in% c('WFresno','WKern','Kings','WTulare','EKern','WMadera') ~ 'Southern SJV',
                                                   county1 %in% c('Monterey', 'San Luis Obispo', 'Santa Barbara', 'Ventura') ~ 'Central Coast',
                                                   county1 %in% c('Merced','San Joaquin','Stanislaus') ~ 'Northern SJV',
                                                   county1 %in% c('NLA') ~ 'Southern Coast'),
                                Region = factor(Region, levels = c('Northern SJV',
                                                                   'Central Coast',
                                                                   'Southern Coast',
                                                                   'Southern SJV')),
                                Study_Area = 'Study Area')

CA_counties <- tigris::counties(state = '06')
dataFrame <- read_rds('~/Desktop/Projections/full_projection_data2.rds')
county_IR <- dataFrame[year >= 2000][, .(N = sum(N_all, na.rm = T), pop = unique(CTpop)), .(county, GEOID, year)][, .(N = sum(N), pop = sum(pop)), .(county, year)][, IR := N/pop*100000][, .(mean_IR = mean(IR)), .(county)]
county_IR[, mean_IR := ifelse(mean_IR == 0, 0.4261860, mean_IR)]
CA_counties <- merge(CA_counties, county_IR, by.x = 'NAME', by.y = 'county')
GEOIDs_in_div <- read.csv('Data/GEOIDs_in_Divided_Counties.csv',
                          colClasses = c('GEOID' = 'character'))
census_tracts <- st_read('~/Desktop/Shapefiles/California_CensusTracts_tl_2017_06_tract/tl_2017_06_tract.shp') %>%
  dplyr::filter(GEOID %in% GEOIDs_in_div[GEOIDs_in_div$county1 %in% included_counties,]$GEOID) %>%
  dplyr::filter(GEOID != '06083980100',GEOID != '06083990000', GEOID != '06111003612', GEOID != '06111980000') %>%
  dplyr::filter(ALAND != 0)

p1 <- ggplot() +
  geom_sf(data = CA_counties, aes(fill = mean_IR)) +
  geom_sf(data = counties[counties$county1 %in% included_counties,], fill = 'transparent', col = 'white', linewidth = 0.5) +
  scale_fill_gradient(low = 'white', high = '#9246a3', trans = 'log', breaks = c(0,1,10, 50, 200), 
                      labels = c('0','<1','10', '50', '200')) +
  theme_void() +
  geom_sf(data = st_union(census_tracts), col = 'black', fill = NA, linewidth = 1) +
  labs(fill = 'Mean annual incidence\n(per 100,000 population)') +
  theme(text = element_text(size = 14),
        legend.position = 'bottom') ; p1


season_per_ct <- msm.data.census %>% group_by(GEOID, tyear) %>% dplyr::summarize(N = as.numeric(base::all(!is.na(epi.start.srm)))) %>% ungroup()%>%
  group_by(GEOID) %>% dplyr::summarize(N = base::sum(N))
census_tracts <- census_tracts %>%
  left_join(season_per_ct, by = 'GEOID') %>% mutate(N = case_when(N >0 ~ N,
                                                                  TRUE ~ NA_real_))

p2 <- ggplot(census_tracts) + geom_sf(aes(fill = N), col = 'gray', linewidth = 0.05) + 
  theme_void() + 
  scale_fill_gradient2(low = 'white',mid = 'darkolivegreen4', high = 'darkolivegreen',midpoint = 5, na.value = 'gray90', breaks = c(2,5,8,11)) + 
  geom_sf(data = counties[!is.na(counties$Region),], col = 'black', linewidth = 0.5, fill = NA) + 
  labs(fill ='Number of \nseasonal years') + 
  theme(legend.position = c(0.85,0.8),
        text = element_text(size = 14)) ; p2

x11(type = 'cairo')
ppt_ct <- msm.data.census %>% group_by(GEOID, tyear) %>% summarise(ppt = mean(ppt), tmean = mean(tmean)) %>% ungroup() %>% 
  group_by(GEOID) %>% summarise(ppt = mean(ppt), tmean = mean(tmean))
ppt_ct <- msm.data.census %>% group_by(GEOID) %>% summarise(ppt = mean(ppt), tmean = mean(tmean)) %>% ungroup() %>% 
  mutate(ppt = ifelse(ppt > 15, 15, ppt))
census_tracts <- merge(census_tracts, ppt_ct, by = 'GEOID')
p3 <- ggplot(census_tracts) + geom_sf(aes(fill = ppt), col = 'gray', linewidth = 0) + 
  theme_void() + 
  scale_fill_gradient(low = 'white',high = '#6f87c7',na.value = 'gray90',
                      breaks = c(0,5,10,15), labels = c('0','5','10','>15')) + 
  geom_sf(data = counties[!is.na(counties$Region),], col = 'black', linewidth = 0.5, fill = NA) + 
  labs(fill ='Mean weekly \nprecipitation (mm)') + 
  theme(legend.position = c(0.85,0.8),
        text = element_text(size = 14))

p4 <- ggplot(census_tracts) + geom_sf(aes(fill = tmean), col = 'gray', linewidth = 0) + 
  theme_void() + 
  scale_fill_gradient(low = 'white',high = '#bd5848',na.value = 'gray90') + 
  geom_sf(data = counties[!is.na(counties$Region),], col = 'black', linewidth = 0.5, fill = NA) + 
  labs(fill ='Mean weekly \ntemperature (\u00B0C)') + 
  theme(legend.position = c(0.85,0.8),
        text = element_text(size = 14))


png('Plots/Figure1.png', width = 14, height = 5, units = 'in', res = 600)
plot_grid(
  p1,
  p2 + theme(legend.position = 'bottom'),
  p3+ theme(legend.position = 'bottom'),
  p4+ theme(legend.position = 'bottom'),
  nrow = 1,
  ncol = 4,
  align = 'vh',
  #axis = 'tb',
  labels = c('A','B','C','D')
)
dev.off()

## Figure 3: Example of seasonal states
msm.data.county <- read.csv('Data/msm.data.county.csv')
msm.data.county %>% filter(county1 == 'WKern', tyear == '2016/2017') %>% 
  ggplot() + geom_line(aes(tweek, smooth7)) + 
  geom_vline(aes(xintercept = epi.start.srm), linetype = 'dashed') + 
  geom_vline(aes(xintercept = epi.end.srm), linetype = 'dashed') + theme_bw() +
  labs(x = 'Week (since Apr.1)', y = 'Incidence Rate (per 100,000)') + 
  annotate('text', label = 'Aseasonal \n(State 1)', x = 7, y = 7.5, size = 5) + 
  annotate('text', label = 'Seasonal \n(State 2)', x = 30, y = 7.5, size = 5) + 
  annotate('text', label = 'Aseasonal \n(State 1)', x = 50, y = 7.5, size = 5)+ 
  annotate('text', label = 'Onset', x = 15.31921, y = 2.25, size = 5)+ 
  annotate('text', label = 'End', x = 44.12462, y = 2.25, size = 5) + 
  coord_cartesian(ylim = c(3,13), clip = 'off')
ggsave('~/Desktop/CSG Figures/season_example.png', width = 9, height = 6, units = 'in', dpi = 300)


# Figure 4

descript <- msm.data.census %>% dplyr::select(GEOID, tyear, county1,
                                              epi.start.srm, epi.end.srm,
                                              epi.start.mcm, epi.end.mcm) %>% distinct()
descript <- descript %>% mutate(Region = case_when(county1 %in% c('WFresno','WKern','Kings','WTulare','EKern','WMadera') ~ 'Southern SJV',
                                                   county1 %in% c('Monterey', 'San Luis Obispo', 'Santa Barbara', 'Ventura') ~ 'Central Coast',
                                                   county1 %in% c('Merced','San Joaquin','Stanislaus') ~ 'Northern SJV',
                                                   county1 %in% c('NLA', 'Orange', 'San Diego') ~ 'Southern Coast'),
                                Region = factor(Region, levels = c('Northern SJV',
                                                                   'Central Coast',
                                                                   'Southern Coast',
                                                                   'Southern SJV')))
descript <- descript %>% dplyr::filter(!is.na(epi.start.srm))

#Trends by Region
descript %>% group_by(Region) %>% dplyr::summarize(mean.onset = mean(onset.srm, na.rm = T),
                                                  mean.peak = median(peak.srm, na.rm = T),
                                                  mean.end = mean(end.srm, na.rm = T),
                                                  mean.duration = mean(end.srm - onset.srm, na.rm = T))

# Lat long association
# summary(glm(onset.srm - end.srm ~ lat + long, data = descript))

## Plotting
# order.county <- descript %>% group_by(Region) %>% arrange(median.duration, .by_group = T) %>% pull(county2) %>% unique() 
# descript <- descript %>% mutate(county2 = factor(county2, levels = order.county))
# bins <- descript %>% dplyr::select(Region, county2, median.duration) %>% distinct() %>% 
#   mutate(median.bin = cut(median.duration, breaks = round(seq(0,52,52/12),2)[8:12],                                                                   labels = c('Nov','Dec','Jan','Feb')))


# descript.plot <- merge(descript, bins, by = c('Region','county2'))
# pal <- paletteer::paletteer_d("rcartocolor::Teal")[c(1,3,5,7)]
bounds <- descript %>% group_by(county1) %>% dplyr::summarize(onset_upperIQR = quantile(epi.start.srm, probs = 0.75, na.rm = T),
                                          onset_lowerIQR = quantile(epi.start.srm, probs = 0.25, na.rm = T),
                                          # onset_median = quantile(epi.start.srm, probs = 0.50, na.rm = T),
                                          onset_median = mean(epi.start.srm, na.rm = T),
                                          # peak_upperIQR = quantile(peak.srm, probs = 0.75, na.rm = T),
                                          # peak_lowerIQR = quantile(peak.srm, probs = 0.25, na.rm = T),
                                          # peak_median = quantile(peak.srm, probs = 0.50, na.rm = T),
                                          end_upperIQR = quantile(epi.end.srm, probs = 0.75, na.rm = T),
                                          end_lowerIQR = quantile(epi.end.srm, probs = 0.25, na.rm = T),
                                          #end_median = quantile(epi.end.srm, probs = 0.50, na.rm = T),
                                          end_median = mean(epi.end.srm, na.rm = T)) %>%
  mutate(county21 = factor(county1, levels = rev(c('Merced','San Joaquin','Stanislaus', 
                                              'Monterey', 'San Luis Obispo', 'Santa Barbara',  'Ventura',
                                              'WFresno', 'WKern','EKern', 'Kings', 'WMadera', 'WTulare',
                                              'NLA'))))

medians <- bounds %>% pivot_longer(cols = c(onset_median, end_median), values_to = 'median', names_to = 'transition') %>% 
  mutate(transition = case_when(substr(transition, 1,5) == 'onset' ~ 'Onset', substr(transition, 1,3) == 'end' ~ 'End')) %>% 
  dplyr::select(county1, transition, median)

lowerIQR <- bounds %>% pivot_longer(cols = c(onset_lowerIQR, end_lowerIQR), values_to = 'lowerIQR', names_to = 'transition') %>% 
  mutate(transition = case_when(substr(transition, 1,5) == 'onset' ~ 'Onset', substr(transition, 1,3) == 'end' ~ 'End')) %>% 
  dplyr::select(county1, transition, lowerIQR)

upperIQR <- bounds %>% pivot_longer(cols = c(onset_upperIQR, end_upperIQR), values_to = 'upperIQR', names_to = 'transition') %>% 
  mutate(transition = case_when(substr(transition, 1,5) == 'onset' ~ 'Onset', substr(transition, 1,3) == 'end' ~ 'End')) %>% 
  dplyr::select(county1, transition, upperIQR)

num_seasons <- msm.data.census %>% dplyr::select(GEOID, county1, epi.start.srm) %>% distinct() %>%
  group_by(county1) %>% dplyr::summarise(num_seasons = n())

bounds <- medians %>% left_join(lowerIQR, by = c('county1', 'transition')) %>% 
  left_join(upperIQR, by = c('county1', 'transition')) %>%
  left_join(num_seasons, by = 'county1') %>%
  dplyr::filter(transition != 'Duration') %>% 
  mutate(transition = factor(transition, levels = c('Onset','End'))) %>%
  mutate(county1 = case_when(county1 == 'NLA' ~ 'Northern Los Angeles',
                             county1 == 'WTulare' ~ 'Western Tulare',
                             county1 == 'WMadera' ~ 'Western Madera',
                             county1 == 'WKern' ~ 'Western Kern',
                             county1 == 'WFresno' ~ 'Western Fresno',
                             county1 == 'EKern' ~ 'Eastern Kern',
                             TRUE ~ as.character(county1))) %>% 
  mutate(county1 = factor(county1, levels = rev(c('Merced','San Joaquin','Stanislaus', 
                                                  'Monterey', 'San Luis Obispo', 'Santa Barbara',  'Ventura',
                                                  'Western Fresno', 'Western Kern','Eastern Kern', 'Kings', 'Western Madera', 'Western Tulare',
                                                  'Northern Los Angeles'))))

bounds <- bounds %>% mutate(Region = case_when(county1 %in% c('Western Fresno','Western Kern','Kings','Western Tulare','Eastern Kern','Western Madera') ~ 'Southern\nSan Joaquin\nValley',
                                    county1 %in% c('Monterey', 'San Luis Obispo', 'Santa Barbara', 'Ventura') ~ 'Central\nCoast',
                                    county1 %in% c('Merced','San Joaquin','Stanislaus') ~ 'Northern\nSan Joaquin\nValley',
                                    county1 %in% c('Northern Los Angeles', 'Orange', 'San Diego') ~ 'Southern\nCoast'),
                 Region = factor(Region, levels = c('Northern\nSan Joaquin\nValley',
                                                    'Central\nCoast',
                                                    'Southern\nSan Joaquin\nValley',
                                                    'Southern\nCoast')))

ggplot(data = bounds) + 
  geom_point(data = bounds, aes(x = county1, y = median, col = transition, size = num_seasons)) + 
  geom_errorbar(data = bounds, aes(x = county1, y = median, ymin = lowerIQR, ymax = upperIQR, 
                    col = transition), 
                width = 0, linewidth =1) + 
  scale_y_continuous(breaks = seq(0,52,by = 52/12)[1:12], 
                      labels = month.abb[c(4:12,1:3)]) +
  scale_color_manual(values = c('#4f7741','#7e4ac1')) +
  facet_grid(Region~., scales = 'free_y',space = 'free_y') +
  theme_classic() + labs(y = 'Month', col = 'Season Timing') + coord_flip() +
  labs(x = 'County', y = NULL, size = 'Number of Seasons') +  
  theme(text = element_text(size = 20), legend.position = 'top', panel.spacing=unit(1,"lines"),
        strip.text.y.right = element_text(angle = 0), strip.background = element_rect(color="black", fill="#f0f0f0"))

ggsave('Plots/Fig3.png', width = 14, height =8, units = 'in', dpi = 600)
