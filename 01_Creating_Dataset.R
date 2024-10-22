## Constructing weekly incidence dataset

#Census Tracts
geo <- read.csv('geo_2023.csv', colClasses = 'character')[,-1]
date <- read.csv('date_2023.csv')[,-1]
data <- data.frame(date = date, GEOID = geo) %>% as.data.table()
data[, date := as.Date(date)]
data <- data[, .(N = .N), .(date, GEOID)]

# Making full time series
CTs <- tigris::tracts(state = '06', year = 2017)
dates <- seq(as.Date('2000-01-01'),as.Date('2023-12-01'), by = 'day')
fullset <- expand.grid(CTs$GEOID, dates)
names(fullset) <- c('GEOID','date')

#Add dates
dates_df <- as.data.table(fullset)
dates_df[, ':='(week = lubridate::epiweek(date),
                                       year = lubridate::year(date),
                                       month = lubridate::month(date))]
dates_df[, wkyr := fcase(week < 20 & month == 1, paste0(week,'-', year),
                         week > 20 & month == 1, paste0(week,'-', year -1),
                         week == 1 & month > 1, paste0(week,'-', year +1),
                         week > 1 & month > 1, paste0(week, '-', year))]
dates_df <- dates_df[, first_day := first(date), .(wkyr)][order(GEOID, date)]
contweeks <- dates_df %>% dplyr::select(wkyr) %>% distinct() %>% dplyr::mutate(continuous_week_onset = 1:length(wkyr))
dates_df <- merge(dates_df, contweeks, by = 'wkyr')
dates_df <- dates_df[order(GEOID, date)]

data <- merge(data, dates_df, by = c('GEOID','date'), all.y = T)

GEOIDs <- read.csv('Data/GEOIDs_in_Divided_Counties.csv', colClasses = c('GEOID' = 'character'))[-1]
data <- merge(data, GEOIDs, by = 'GEOID')

#filtering to only endemic counties
subcounties <- c('WKern','WFresno','WTulare','San Luis Obispo','Ventura','Kings','Monterey','San Joaquin','NLA', 'EKern','Merced','Stanislaus','Santa Barbara','WMadera')
data <- data[county1 %in% subcounties]
data[is.na(N)]$N <- 0

## Aggregate
data <- data[, .(N = sum(N)), .(GEOID, wkyr, first_day, continuous_week_onset, county1)]
data[, ':='(week = as.numeric(str_split(wkyr,'-', simplify = T)[,1]),
            year = as.numeric(str_split(wkyr,'-', simplify = T)[,2]))]

## Adding population data
pop <- read.csv('~/Desktop/Projections/CA Population Denominators/CensusTract_Population_2000_2023.csv', colClasses = c('GEOID' = 'character'))[,-1]
data <- merge(data, pop, by = c('GEOID','year'))
data[CTpop < 100]$CTpop <- 1000
data[, IR := N/CTpop*100000]
data[, month := lubridate::month(first_day)]
data[GEOID == '06111990100'] %>% View()

data[, tyear := fcase(first_day >= as.Date('2000-04-01') & first_day < as.Date('2001-04-01'), 2000,
                      first_day >= as.Date('2001-04-01') & first_day < as.Date('2002-04-01'), 2001,
                      first_day >= as.Date('2002-04-01') & first_day < as.Date('2003-04-01'), 2002,
                      first_day >= as.Date('2003-04-01') & first_day < as.Date('2004-04-01'), 2003,
                      first_day >= as.Date('2004-04-01') & first_day < as.Date('2005-04-01'), 2004,
                      first_day >= as.Date('2005-04-01') & first_day < as.Date('2006-04-01'), 2005,
                      first_day >= as.Date('2006-04-01') & first_day < as.Date('2007-04-01'), 2006,
                      first_day >= as.Date('2007-04-01') & first_day < as.Date('2008-04-01'), 2007,
                      first_day >= as.Date('2008-04-01') & first_day < as.Date('2009-04-01'), 2008,
                      first_day >= as.Date('2009-04-01') & first_day < as.Date('2010-04-01'), 2009,
                      first_day >= as.Date('2010-04-01') & first_day < as.Date('2011-04-01'), 2010,
                      first_day >= as.Date('2011-04-01') & first_day < as.Date('2012-04-01'), 2011,
                      first_day >= as.Date('2012-04-01') & first_day < as.Date('2013-04-01'), 2012,
                      first_day >= as.Date('2013-04-01') & first_day < as.Date('2014-04-01'), 2013,
                      first_day >= as.Date('2014-04-01') & first_day < as.Date('2015-04-01'), 2014,
                      first_day >= as.Date('2015-04-01') & first_day < as.Date('2016-04-01'), 2015,
                      first_day >= as.Date('2016-04-01') & first_day < as.Date('2017-04-01'), 2016,
                      first_day >= as.Date('2017-04-01') & first_day < as.Date('2018-04-01'), 2017,
                      first_day >= as.Date('2018-04-01') & first_day < as.Date('2019-04-01'), 2018,
                      first_day >= as.Date('2019-04-01') & first_day < as.Date('2020-04-01'), 2019,
                      first_day >= as.Date('2020-04-01') & first_day < as.Date('2021-04-01'), 2020,
                      first_day >= as.Date('2021-04-01') & first_day < as.Date('2022-04-01'), 2021,
                      first_day >= as.Date('2022-04-01') & first_day < as.Date('2023-04-01'), 2022,
                      first_day >= as.Date('2023-04-01') & first_day < as.Date('2024-04-01'), 2023)]

# Creating transmission week variable (Apr - Mar)
data[, tweek := 1:length(wkyr), .(GEOID, tyear)]

# filtering out incomplete transmission years
data <- data[tyear != 1999 & tyear != 2023 & !is.na(tyear)]

# adding an indicator for GEOID-transmission year
data[, c := paste(GEOID,'-',tyear)]

write_rds(data, 'Data/timing_data.rds')


