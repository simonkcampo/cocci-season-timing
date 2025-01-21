## Timing G-computation functions ## 

identify_years <- function(dataframe, season_of_interest, percentile) {
  require(data.table)
  seasonal_ppt <- dataframe[, .(ppt = sum(ppt, na.rm = T)), .(GEOID, met_year, season)] # Calculate total precip by CT, transmission year, and season
  if (season_of_interest == 'Spring'){
  seasonal_ppt <- seasonal_ppt[met_year != 2000 & met_year != 2023]
  } else if (season_of_interest == 'Fall') {
    seasonal_ppt <- seasonal_ppt[met_year != 2023 & met_year != 2000]
  } else{}
  seasonal_ppt[, precip_quantile := cume_dist(ppt), .(GEOID, season)] # Calculate cumulative distribution of total precip by CT and season
  seasonal_ppt <- seasonal_ppt[season == season_of_interest] # filter to just season of interest
  seasonal_ppt <- seasonal_ppt[seasonal_ppt[, .I[which.min(abs(precip_quantile - percentile))], .(GEOID)]$V1] # filter to quantile of interest
  CTyears <- seasonal_ppt[, c('GEOID','met_year'), with = F] # pull CTs and transmission years
  CTyears <- CTyears[, .(GEOID = GEOID, yoi = met_year)] # rename
  return(CTyears)
}

identify_sharp_years <- function(dataframe, season_of_interest, percentile) {
  require(data.table)
  # Make rain season indicators
  seasonal_ppt <- dataframe[, ':='(core_ppt = ifelse(month %in% c(11,12,1,2), 1, 0),
                                   marg_ppt = ifelse(month %in% c(9,10,3,4), 1, 0))] # Calculate total precip by CT, transmission year, and season
  seasonal_ppt[, total_rain := sum(ppt), .(GEOID, rain_year)]
  core_rain <- seasonal_ppt[, .(frac_core = sum(ppt)/unique(total_rain)), .(GEOID, rain_year, core_ppt)][core_ppt != 0][rain_year != 1999 & rain_year != 2000 & rain_year != 2023]
  marg_rain <- seasonal_ppt[, .(frac_marg = sum(ppt)/unique(total_rain)), .(GEOID, rain_year, marg_ppt)][marg_ppt != 0][rain_year != 1999 & rain_year != 2000 & rain_year != 2023]
  
  sharpness <- merge(core_rain, marg_rain, by = c('GEOID','rain_year'))
  sharpness[, sharpness := frac_core/frac_marg]
  sharpness <- sharpness[order(GEOID, sharpness)][, sharp_percentile := cume_dist(sharpness), .(GEOID)]
  idx <- sharpness[, .I[which.min(abs(sharp_percentile - percentile))], by = GEOID]$V1
  CTyears <- sharpness[idx] # filter to quantile of interest
  CTyears <- CTyears[, list(GEOID,rain_year)] # pull CTs and transmission years
  if (season_of_interest == 'Spring') {
  CTyears <- CTyears[, rain_year := rain_year+1] # rename
  } else {}
  CTyears <- CTyears[, .(GEOID = GEOID, yoi = rain_year)] # rename
  return(CTyears)
}

new_hazard <- function(df, model) {
  require(msm)
  all.probs <- data.frame()
  for(x in unique(df$GEOID)) {
    df2 <- df[GEOID == x]
    probs <- matrix(NA, nrow = nrow(df2), ncol = 4)
    for (i in 1:nrow(df2)) {
      pmat <- pmatrix.msm(model, covariates = list(ppt1 = df2[i,'ppt1'],
                                                           ppt2 = df2[i,'ppt2'],
                                                           ppt3 = df2[i,'ppt3'],
                                                           tmean1 = df2[i,'tmean1'],
                                                           tmean2 = df2[i,'tmean2'],
                                                           tmean3 = df2[i,'tmean3'],
                                                           year = df2[i,'year'], 
                                                           county1 = df2[i,'county1']))
      probs[i,1] <- x
      probs[i,2] <- i
      probs[i,3] <- pmat[1,2]
      probs[i,4] <- pmat[2,1]
    }
    all.probs <- rbind(all.probs, probs) 
  }
  names(all.probs) <- c('GEOID','week','onset.prob','end.prob')
  all.probs <- as.data.table(all.probs)
  all.probs[, ':='(week = as.numeric(week), onset.prob = as.numeric(onset.prob), end.prob = as.numeric(end.prob))]
  return(all.probs)
}
