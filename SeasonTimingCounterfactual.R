###################################################################################################
## Code script to estimate the change in coccidioidomycosis season onset and duration under counterfactual conditions
## Region is restricted to 14 counties/sub-counties within California 
## Onset and end dates are estimated using segmented regression
# 
## Written by Simon K Camponuri
## Last updated: March 10, 2024
###################################################################################################

# Set working directory and load necessary packages
setwd('~/Desktop/Season Timing')
library(msm); library(tidyverse); library(dlnm); library(data.table)

# Load data
dataFrame <- fread('Data/msm.data.census2023.csv', colClasses = c('GEOID' = 'character'))[order(GEOID, first_day)] # Timing dataset
msm_mod <- read_rds('ppt.tmean.two.state.2023.rds') # Model

m <- length(unique(dataFrame$GEOID))
# load necessary functions
source('Code/CounterfactualFunctions.R')

# format data
dataFrame[, month := lubridate::month(first_day)] # Month indicator taken from the first day of each week
dataFrame[, met_year := ifelse(month %in% 3:12, year, year -1)]
dataFrame[, rain_year := ifelse(month %in% 6:12, year, year -1)]
dataFrame[, season := fcase(month %in% c(3,4,5), 'Spring',
                            month %in% c(6,7,8), 'Summer',
                            month %in% c(9,10,11), 'Fall',
                            month %in% c(12,1,2), 'Winter')]

###################################################################################################

#### Step 1: Create CT-specific counterfactual covariates (e.g., spring experienced 25th vs 75th percentile rainfall/temperature) ####

#### Spring ####
# Identify dry and wet SPRING years (25th and 75th percentile of cumulative spring precipitaiton)
dry_spring_years <- identify_years(dataFrame, 'Spring', 0.1)
wet_spring_years <- identify_years(dataFrame, 'Spring', 0.9)

# Join year identifiers to full dataframe
dry_spring <- merge(dataFrame, dry_spring_years, by = 'GEOID') 
wet_spring <- merge(dataFrame, wet_spring_years, by = 'GEOID') 

#### Fall #### 

# Identify dry and wet FALL years (25th and 75th percentile of cumulative fall precipitaiton)
dry_fall_years <- identify_years(dataFrame, 'Fall', 0.1)
wet_fall_years <- identify_years(dataFrame, 'Fall', 0.9)

# Join year identifiers to full dataframe
dry_fall <- merge(dataFrame, dry_fall_years, by = 'GEOID') 
wet_fall <- merge(dataFrame, wet_fall_years, by = 'GEOID') 

###################################################################################################

###################################################################################################

#### Step 2: Format counterfactual dataframes for model ####

### Create basis functions ###

### Spring
dry_spring_ppt_cb <- crossbasis(dry_spring$ppt,lag=c(2,26), # 2-26 week lags
                                argvar=list(fun="lin"), # linear exposure-response
                                arglag=list(fun="ns", df = 3),
                                group = dry_spring$GEOID) # grouped by census tract
wet_spring_ppt_cb <- crossbasis(wet_spring$ppt,lag=c(2,26),
                                argvar=list(fun="lin"),
                                arglag=list(fun="ns",df = 3),
                                group = wet_spring$GEOID)
dry_spring_tmean_cb <- crossbasis(dry_spring$tmean,lag=c(2,26),
                                  argvar=list(fun="lin"),
                                  arglag=list(fun="ns", df = 3),
                                  group = dry_spring$GEOID)
wet_spring_tmean_cb <- crossbasis(wet_spring$tmean,lag=c(2,26),
                                  argvar=list(fun="lin"),
                                  arglag=list(fun="ns", df = 3),
                                  group = wet_spring$GEOID)

# Create counterfactual spring datasets #
dry_spring <- dry_spring[, ':='(ppt1 = dry_spring_ppt_cb[,1], # pull out basis variables and add to dataframe
                                ppt2 = dry_spring_ppt_cb[,2],
                                ppt3 = dry_spring_ppt_cb[,3],
                                tmean1 = dry_spring_tmean_cb[,1],
                                tmean2 = dry_spring_tmean_cb[,2],
                                tmean3 = dry_spring_tmean_cb[,3])][tyear == yoi][order(GEOID, tyear, tweek)]

wet_spring <- wet_spring[, ':='(ppt1 = wet_spring_ppt_cb[,1],
                                ppt2 = wet_spring_ppt_cb[,2],
                                ppt3 = wet_spring_ppt_cb[,3],
                                tmean1 = wet_spring_tmean_cb[,1],
                                tmean2 = wet_spring_tmean_cb[,2],
                                tmean3 = wet_spring_tmean_cb[,3])][tyear == yoi][order(GEOID, tyear, tweek)]

### Fall

dry_fall_ppt_cb <- crossbasis(dry_fall$ppt,lag=c(2,26),
                              argvar=list(fun="lin"),
                              arglag=list(fun="ns", df = 3),
                              group = dry_fall$GEOID)
wet_fall_ppt_cb <- crossbasis(wet_fall$ppt,lag=c(2,26),
                              argvar=list(fun="lin"),
                              arglag=list(fun="ns", df = 3),
                              group = wet_fall$GEOID)
dry_fall_tmean_cb <- crossbasis(dry_fall$tmean,lag=c(2,26),
                                argvar=list(fun="lin"),
                                arglag=list(fun="ns", df = 3),
                                group = dry_fall$GEOID)
wet_fall_tmean_cb <- crossbasis(wet_fall$tmean,lag=c(2,26),
                                argvar=list(fun="lin"),
                                arglag=list(fun="ns", df = 3),
                                group = wet_fall$GEOID)

# Create counterfactual fall datasets #

dry_fall <- dry_fall[, ':='(ppt1 = dry_fall_ppt_cb[,1],
                            ppt2 = dry_fall_ppt_cb[,2],
                            ppt3 = dry_fall_ppt_cb[,3],
                            tmean1 = dry_fall_tmean_cb[,1],
                            tmean2 = dry_fall_tmean_cb[,2],
                            tmean3 = dry_fall_tmean_cb[,3])][tyear == yoi][order(GEOID, tyear, tweek)]


wet_fall <- wet_fall[, ':='(ppt1 = wet_fall_ppt_cb[,1],
                                ppt2 = wet_fall_ppt_cb[,2],
                                ppt3 = wet_fall_ppt_cb[,3],
                                tmean1 = wet_fall_tmean_cb[,1],
                                tmean2 = wet_fall_tmean_cb[,2],
                                tmean3 = wet_fall_tmean_cb[,3])][tyear == yoi][order(GEOID, tyear, tweek)]


###################################################################################################

#### Step 3: Estimate hazards given new covariates ####

# Estimating the effect of counterfactual spring conditions on onset

# Calculate weekly hazards
dry_spring_hazard <- new_hazard(dry_spring, msm_mod)
wet_spring_hazard <- new_hazard(wet_spring, msm_mod)

# Calculate weekly hazards
dry_spring_fail <- dry_spring_hazard[order(GEOID, week)][, fail := 1- cumprod(1-onset.prob), .(GEOID)]
wet_spring_fail <- wet_spring_hazard[order(GEOID, week)][, fail := 1- cumprod(1-onset.prob), .(GEOID)]

# Calculate weekly hazards
dry_spring_fail <- dry_spring_fail[, fail_standard := fail/max(fail), .(GEOID)]
wet_spring_fail <- wet_spring_fail[, fail_standard := fail/max(fail), .(GEOID)]

dry_spring_fail[week == 52][, .(mean_fail = mean(fail))]
wet_spring_fail[week == 52][, .(mean_fail = mean(fail))]


# Take 100000 samples from failure functions to get onset weeks
set.seed(123)
samps <- runif(10000, 0,1)
dry_spring_onset <- matrix(NA, nrow = m, ncol = length(samps))
wet_spring_onset <- matrix(NA, nrow = m, ncol = length(samps))
# For each probability, pull out the corresponding onset week
for (i in 1:length(samps)) {
  idx <- dry_spring_fail[, dry_spring_fail[, .I[which.min(abs(fail_standard - samps[i]))], .(GEOID)]]$V1
  dry_spring_onset[,i] <- dry_spring_fail[idx]$week
  idx <- wet_spring_fail[, wet_spring_fail[, .I[which.min(abs(fail_standard - samps[i]))], .(GEOID)]]$V1
  wet_spring_onset[,i] <- wet_spring_fail[idx]$week
}

mean(apply(wet_spring_onset - dry_spring_onset, 2, mean))
quantile(apply(wet_spring_onset - dry_spring_onset, 2, mean), probs = 0.025)
quantile(apply(wet_spring_onset - dry_spring_onset, 2, mean), probs = 0.5)
quantile(apply(wet_spring_onset - dry_spring_onset, 2, mean), probs = 0.975)
hdi(apply(wet_spring_onset - dry_spring_onset, 2, mean))

# # Calculate average hazard and failure functions across census tracts
# haz <- merge(dry_spring_hazard, wet_spring_hazard, by = c('GEOID','week')) 
# haz <- haz[, .(dry_hazard = mean(onset.prob.x),
#         wet_hazard = mean(onset.prob.y)), .(week)][order(week)]
# 
# # Calculate failure function
# haz[, ':='(dry_fail = 1- cumprod(1-dry_hazard),
#            wet_fail = 1- cumprod(1-wet_hazard))]
# 
# ## Standardize failure functions by maximum
# haz[, ':='(dry_fail_standard = dry_fail/max(dry_fail),
#            wet_fail_standard = wet_fail/max(wet_fail))]
# 
# # Take 100000 samples from failure functions to get onset weeks
# set.seed(123)
# samps <- runif(10000, 0,1)
# dry_spring_onset <- rep(NA, length(samps))
# wet_spring_onset <- rep(NA, length(samps))
# # For each probability, pull out the corresponding onset week
# for (i in 1:length(samps)) {
#   dry_spring_onset[i] <- which.min(abs(haz$dry_fail_standard - samps[i]))
#   wet_spring_onset[i] <-  which.min(abs(haz$wet_fail_standard - samps[i]))
# }

# subtract weeks and calculate quantiles
hist(wet_spring_onset - dry_spring_onset)
quantile(wet_spring_onset - dry_spring_onset, probs = c(0.025, 0.50, 0.975))
mean(wet_spring_onset - dry_spring_onset)
hdi(wet_spring_onset - dry_spring_onset)

###################################################################################################

### Visualize Results ### 

colors <- c('Wet Spring' = '#2a5b6f',
            'Dry Spring' = '#c07763') 

# # Failure functions
# ggplot(haz) +
#   geom_line(aes(week, dry_fail_standard, col = 'Dry Spring'), linewidth = 1.2) +
#   geom_line(aes(week, wet_fail_standard, col = 'Wet Spring'), linewidth = 1.2) +
#     scale_color_manual(values = colors) +
#   theme_bw() + labs(x = 'Week (since Apr. 1)', 
#                     y = 'Cumulative Probability of Season Onset', 
#                     color = 'Conditions') + 
#   theme(legend.position = c(0.8, 0.2), text = element_text(size = 16))

# Onset week densities
p1 <- ggplot(data = data.frame(dry_spring_onset = apply(dry_spring_onset, 2, mean),
                         wet_spring_onset = apply(wet_spring_onset, 2, mean))) +
  geom_density(aes(dry_spring_onset, fill = 'Dry Spring', col = 'Dry Spring'), alpha = 0.5, bw = 2.5) +
  geom_density(aes(wet_spring_onset, fill = 'Wet Spring', col = 'Wet Spring'), alpha = 0.5, bw = 2.5) +
  scale_x_continuous(breaks = seq(0,52,by = 52/12)[1:13], 
                     labels = month.abb[c(4:12,1:4)]) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_classic() + 
  theme(legend.position = c(0.8, 0.9), text = element_text(size = 16)) + 
  labs(y = 'Density', x = 'Season Onset', fill = 'Conditions', col = 'Conditions'); p1

###################################################################################################

### Estimating the effect of counterfactual fall conditions on duration ###

## Extract each tyear's CT-specific onset week

# onsets <- unique(dataFrame[, .(GEOID = GEOID, tyear = tyear, onset = epi.start.srm)])

# # Filter to only weeks FOLLOWING the onset for that transmission year
# dry_fall <- merge(dry_fall, onsets, by = c('GEOID','tyear'))
# dry_fall <- dry_fall[, after := tweek >= onset, .(GEOID, tyear)][after == TRUE]
# wet_fall <- merge(wet_fall, onsets, by = c('GEOID','tyear'))
# wet_fall <- wet_fall[, after := tweek >= onset, .(GEOID, tyear)][after == TRUE]

# Calculate weekly hazards
dry_fall_hazard <- new_hazard(dry_fall, msm_mod)
wet_fall_hazard <- new_hazard(wet_fall, msm_mod)

# Calculate cumulative onset prob 
dry_fall_fail_onset <- dry_fall_hazard[order(GEOID, week)][, fail := 1- cumprod(1-onset.prob), .(GEOID)]
wet_fall_fail_onset <- wet_fall_hazard[order(GEOID, week)][, fail := 1- cumprod(1-onset.prob), .(GEOID)]

# Calculate weekly hazards
dry_fall_fail_onset <- dry_fall_fail_onset[, fail_standard := fail/max(fail), .(GEOID)]
wet_fall_fail_onset <- wet_fall_fail_onset[, fail_standard := fail/max(fail), .(GEOID)]

# Take 100000 samples from failure functions to get onset weeks
set.seed(123)
samps <- runif(10000, 0,1)
dry_fall_onset <- matrix(NA, nrow = length(unique(dry_fall_fail_onset$GEOID)), ncol = length(samps))
wet_fall_onset <- matrix(NA, nrow = length(unique(wet_fall_fail_onset$GEOID)), ncol = length(samps))
# For each probability, pull out the corresponding onset week
for (i in 1:length(samps)) {
  idx <- dry_fall_fail_onset[, dry_fall_fail_onset[, .I[which.min(abs(fail_standard - samps[i]))], .(GEOID)]]$V1
  dry_fall_onset[,i] <- dry_fall_fail_onset[idx]$week
  idx <- wet_fall_fail_onset[, wet_fall_fail_onset[, .I[which.min(abs(fail_standard - samps[i]))], .(GEOID)]]$V1
  wet_fall_onset[,i] <- wet_fall_fail_onset[idx]$week
}


onset <- data.frame(GEOID = unique(dry_fall_fail_onset$GEOID), 
                    dry_onset = apply(dry_fall_onset, 1, mean),
                    wet_onset  = apply(wet_fall_onset, 1, mean)) %>% group_by(GEOID) %>% mutate(onset = mean(c(dry_onset, wet_onset)))

# Calculate cumulative end prob 
dry_fall_fail_end <- merge(dry_fall_hazard, onset, by = 'GEOID')
wet_fall_fail_end <- merge(wet_fall_hazard, onset, by = 'GEOID')

# Calculate cumulative end prob 
dry_fall_fail_end <- dry_fall_fail_end[, after := week >= onset, .(GEOID)][after == TRUE]
wet_fall_fail_end <- wet_fall_fail_end[, after := week >= onset, .(GEOID)][after == TRUE]

# Calculate cumulative end prob 
dry_fall_fail_end[, week_since_onset := 1:.N, .(GEOID)]
wet_fall_fail_end[, week_since_onset := 1:.N, .(GEOID)]

# Calculate cumulative end prob 
dry_fall_fail_end <- dry_fall_fail_end[order(GEOID, week)][, fail := 1- cumprod(1-end.prob), .(GEOID)]
wet_fall_fail_end <- wet_fall_fail_end[order(GEOID, week)][, fail := 1- cumprod(1-end.prob), .(GEOID)]

# Calculate weekly hazards
dry_fall_fail_end <- dry_fall_fail_end[, fail_standard := fail/max(fail), .(GEOID)]
wet_fall_fail_end <- wet_fall_fail_end[, fail_standard := fail/max(fail), .(GEOID)]


# Take 100000 samples from failure functions to get onset weeks
set.seed(123)
samps <- runif(10000, 0,1)
dry_fall_end <- matrix(NA, nrow = length(unique(dry_fall_fail_end$GEOID)), ncol = length(samps))
wet_fall_end <- matrix(NA, nrow = length(unique(wet_fall_fail_end$GEOID)), ncol = length(samps))
# For each probability, pull out the corresponding onset week
for (i in 1:length(samps)) {
  idx <- dry_fall_fail_end[, dry_fall_fail_end[, .I[which.min(abs(fail_standard - samps[i]))], .(GEOID)]]$V1
  dry_fall_end[,i] <- dry_fall_fail_end[idx]$week_since_onset
  idx <- wet_fall_fail_end[, wet_fall_fail_end[, .I[which.min(abs(fail_standard - samps[i]))], .(GEOID)]]$V1
  wet_fall_end[,i] <- wet_fall_fail_end[idx]$week_since_onset
}

mean(apply(wet_fall_end, 2, mean))

quantile(apply(wet_fall_end - dry_fall_end, 2, mean), probs = 0.025)
quantile(apply(wet_fall_end - dry_fall_end, 2, mean), probs = 0.5)
quantile(apply(wet_fall_end - dry_fall_end, 2, mean), probs = 0.975)

# 
# # Calculate average hazard and failure functions across census tracts
# haz <- merge(dry_fall_hazard, wet_fall_hazard, by = c('GEOID','week')) 
# haz <- haz[, .(dry_hazard_onset = mean(onset.prob.x),
#                wet_hazard_onset = mean(onset.prob.y),
#                dry_hazard = mean(end.prob.x),
#                wet_hazard = mean(end.prob.y)), .(week)][order(week)]
# 
# # Calculate failure function
# haz[, ':='(dry_fail = 1- cumprod(1-dry_hazard),
#            wet_fail = 1- cumprod(1-wet_hazard))]
# 
# ## Standardize failure functions by maximum
# haz[, ':='(dry_fail_standard = dry_fail/max(dry_fail),
#            wet_fail_standard = wet_fail/max(wet_fail))]
# 
# ggplot(haz, aes(week)) +
#   geom_line(aes(y = dry_fail), col = '#c07763', linewidth = 1.2) +
#   geom_line(aes(y = wet_fail), col = '#2a5b6f', linewidth = 1.2) +
#   labs(x = 'Week (since season onset)', y = 'Cumulative Probability of Season End') +
#   theme_bw()


# # Take 100000 samples from failure functions to get onset weeks
# set.seed(123)
# samps <- runif(10000, 0,1)
# dry_fall_duration <- rep(NA, length(samps))
# wet_fall_duration <- rep(NA, length(samps))
# # For each probability, pull out the corresponding duration (i.e., week since season onset)
# for (i in 1:length(samps)) {
#   dry_fall_duration[i] <- which.min(abs(haz$dry_fail_standard - samps[i]))
#   wet_fall_duration[i] <-  which.min(abs(haz$wet_fail_standard - samps[i]))
# }
# 
# # subtract weeks and calculate quantiles
# hist(dry_fall_duration - wet_fall_duration)
# quantile(dry_fall_duration - wet_fall_duration, probs = c(0.025, 0.50, 0.975))
# mean(dry_fall_duration - wet_fall_duration)
# hdi(dry_fall_duration - wet_fall_duration)
###################################################################################################

### Visualize Results ### 

colors <- c('Wet Fall' = '#2a5b6f',
            'Dry Fall' = '#c07763') 

# Failure functions
ggplot(haz) +
  geom_line(aes(week, dry_fail, col = 'Dry Fall'), linewidth = 1.2) +
  geom_line(aes(week, wet_fail, col = 'Wet Fall'), linewidth = 1.2) +
  scale_color_manual(values = colors) +
  theme_bw() + labs(x = 'Week (since season onset)', 
                    y = 'Cumulative Probability of Season End', 
                    color = 'Conditions') + 
  theme(legend.position = c(0.8, 0.2), text = element_text(size = 16))

# Season duration densities
p2 <- ggplot(data = data.frame(dry_fall_duration = apply(dry_fall_end, 2, mean),
                         wet_fall_duration = apply(wet_fall_end, 2, mean))) +
  geom_density(aes(dry_fall_duration, fill = 'Dry Fall', col = 'Dry Fall'), alpha = 0.4, bw = 3) +
  geom_density(aes(wet_fall_duration, fill = 'Wet Fall', col = 'Wet Fall'), alpha = 0.4, bw = 3) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_classic() + 
  theme(legend.position = c(0.8, 0.9), text = element_text(size = 16)) + 
  labs(y = 'Density', x = 'Season Duration (weeks)', fill = 'Conditions', col = 'Conditions'); p2


# Combined effect
duration_diffs <-  apply(dry_fall_end, 2, mean) -  apply(wet_fall_end, 2, mean)
onset_diffs <- apply(wet_spring_onset, 2, mean) - apply(dry_spring_onset, 2, mean)
quantile(duration_diffs + onset_diffs, probs = c(0.025, 0.50, 0.975))
mean(duration_diffs + onset_diffs)
hdi(duration_diffs + onset_diffs)

mean(wet_fall_duration - onset_diffs/2)
mean(dry_fall_duration + onset_diffs/2)

colors <- c('Dry Spring and Fall' = '#c07763',
            'Wet Spring and Fall' = '#2a5b6f') 
p3 <- ggplot(data = data.frame(dry_seasons_durations = apply(dry_fall_end, 2, mean) + onset_diffs/2,
                               wet_seasons_durations = apply(wet_fall_end, 2, mean) - onset_diffs/2)) +
  geom_density(aes(dry_seasons_durations, fill = 'Dry Spring and Fall', col = 'Dry Spring and Fall'), alpha = 0.5, bw = 3) +
  geom_density(aes(wet_seasons_durations, fill = 'Wet Spring and Fall', col = 'Wet Spring and Fall'), alpha = 0.5, bw = 3) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.9),
        text = element_text(size = 16)) + 
  labs(y = 'Density', x = 'Season Duration (weeks)', fill = 'Conditions', col = 'Conditions') ; p3

plot_grid(
  p1, 
  p2 + theme(legend.position = c(0.2, 0.9)), 
  p3+ theme(legend.position = c(0.2, 0.9)), 
  ncol = 3, 
  nrow = 1,
  labels = c('A.','B.','C.')
)
ggsave('Plots/gcomp.png', width = 19, height = 6, units = 'in', dpi = 500)


