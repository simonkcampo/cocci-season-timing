library(tidyverse)
library(dplyr)
library(msm)
library(dlnm)
library(pbs)


# Setting lag dimension
minlag = 2
maxlag = 26

############ Running model ############

msm.data.census <- read.csv('msm.data.census2023.csv',
                            colClasses = c('GEOID' = 'character')) %>% arrange(GEOID, first_day)

# removing census tracts without seasons

ppt.census.cb <- crossbasis(msm.data.census$ppt,lag=c(minlag,maxlag), #creating lag-response variables
                            argvar=list(fun="lin"),
                            arglag=list(fun="ns", df = 3),
                            group = msm.data.census$GEOID)

tmean.census.cb <- crossbasis(msm.data.census$tmean,lag=c(minlag,maxlag), #creating lag-response variables
                              argvar=list(fun="lin"),
                              arglag=list(fun="ns", df = 3),
                              group = msm.data.census$GEOID)

numcol <- ncol(msm.data.census)
msm.data.census[,(numcol+1):(numcol+ncol(ppt.census.cb))] <- ppt.census.cb
names(msm.data.census)[(numcol+1):(numcol+ncol(ppt.census.cb))] <- paste0('ppt',1:ncol(ppt.census.cb))

numcol <- ncol(msm.data.census)

msm.data.census[,(numcol+1):(numcol+ncol(tmean.census.cb))] <- tmean.census.cb
names(msm.data.census)[(numcol+1):(numcol+ncol(tmean.census.cb))] <- paste0('tmean',1:ncol(tmean.census.cb))

msm.data.census <- msm.data.census %>% group_by(GEOID) %>%
  mutate(aseasonal = ifelse(all(is.na(epi.start.srm)),1,0)) %>%
  dplyr::filter(aseasonal == 0) %>% ungroup()

#two state transition matrix
Q <- rbind ( c(0.1, 0.1),
             c(0.1, 0.1))
#more accurate transition matrix
Q.crude <- crudeinits.msm(state.mcm ~ continuous_week_onset, subject=GEOID, data=msm.data.census, qmatrix=Q)

form <- paste0('~',paste0('ppt',1:ncol(ppt.census.cb), collapse = '+'),'+',
               paste0('tmean',1:ncol(tmean.census.cb), collapse = '+'), '+ county1 + year')
formula <- as.formula(form)
#
# print(formula)
msm.two.final <- msm(state.mcm ~ continuous_week_onset, subject=GEOID, data = msm.data.census, qmatrix = Q.crude,
               covariates =  formula,
               exacttimes = T, control=list(fnscale=17000, maxit = 10000), na.action = 'na.omit')


 write_rds(msm.two.final, 'ppt.tmean.two.state.2023.mcm.rds')

 ########### Running with pbs ###########

 ## Model with pbs weekly spline
 numcol <- ncol(msm.data.census)
 period.spline <- pbs(msm.data.census$tweek, df = 3)
 msm.data.census[,(numcol+1):(numcol+3)] <- period.spline[,1:3]
 names(msm.data.census)[(numcol+1):(numcol+3)] <- c('per1','per2','per3')

 form <- paste0('~',paste0('ppt',1:ncol(ppt.census.cb), collapse = '+'),'+',
                paste0('tmean',1:ncol(tmean.census.cb), collapse = '+'), '+ county1 + year + per1 + per2 + per3')
 formula <- as.formula(form)
 #
 # print(formula)
 msm.two.pbs <- msm(state.srm ~ continuous_week_onset, subject=GEOID,data = msm.data.census, qmatrix = Q.crude,
                covariates =  formula,
                exacttimes = T, control=list(fnscale=17000, maxit = 10000), na.action = 'na.omit')
 write_rds(msm.two.pbs, 'ppt.tmean.two.state.2023.pbs.rds')


