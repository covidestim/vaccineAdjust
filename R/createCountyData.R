## County Data
## Specs:
## INPUT: Data frame with ndays_obs rows
##        with data Pct and count for 18+, 65+ and complete population

## OUTPUT: Data frame with ndays rows (na for missing days)
##        with data Pct and count for 0-18 18-64 and 65-99
##        for all dates between min date and max date

## Required manipulations
## 1) get all dates ready
## 2) 


## select variables: Date, FIPS, state, count census pct for all agegroup***

createCountyData <- function(nationalData, CountyCensus, maxCoverage, stateCensus, endDate = as.Date("2022-12-31"), nda = 14){

  # from https://healthdata.gov/dataset/COVID-19-Vaccinations-in-the-United-States-County/
  countyData <- read_csv(
    "https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD",
    col_types = cols(
      Date = col_date(format = "%m/%d/%Y"),
      FIPS = col_character(),
      MMWR_week = col_number(),
      Recip_County = col_character(),
      Recip_State = col_character(),
      SVI_CTGY = col_character(),
      Series_Complete_Pop_Pct_SVI = col_character(),
      Series_Complete_12PlusPop_Pct_SVI = col_character(),
      Series_Complete_18PlusPop_Pct_SVI = col_character(),
      Series_Complete_65PlusPop_Pct_SVI = col_character(),
      .default = col_number()
    )
  ) %>% rename(County = Recip_County, StateName = Recip_State)
  
  countyData %>% 
    group_by(FIPS, StateName) %>% 
    transmute(Date = as.Date(Date),
              County = County,
              # County = gsub(" County", "", County),
              # County = if(length(unique(County)) > 1){
              #   maxName = max(nchar(as.matrix(County)))
              #   firstLong = which(nchar(as.matrix(County)) == maxName)
              #   sufLocation = stringi::stri_locate_last(County[firstLong[1]], regex = " ")[1]
              #   sufName = substr(County[firstLong[1]], start = sufLocation, stop = maxName)
              #   if_else(endsWith(County, sufName),
              #           County,
              #           paste0(County, sufName))
              # } else { County },               
               Count_age18to99_ct = Series_Complete_18Plus,
              Count_age65to99_ct = Series_Complete_65Plus,
              Pct_age18to99_ct = Series_Complete_18PlusPop_Pct,
              Pct_age65to99_ct = Series_Complete_65PlusPop_Pct,
              Series_Complete_Yes = Series_Complete_Yes,
              Series_Complete_Pop_Pct = Series_Complete_Pop_Pct) %>%
    # rowwise() %>%
    mutate(
      census_age18to99_ct = round((Count_age18to99_ct/Pct_age18to99_ct)*100),
      census_age65to99_ct = round((Count_age65to99_ct/Pct_age65to99_ct)*100),
      census_total_ct = round((Series_Complete_Yes/Series_Complete_Pop_Pct)*100),
      census_age0to18_ct = census_total_ct - census_age18to99_ct,
      census_age18to64_ct = census_age18to99_ct - census_age65to99_ct,
      Count_age0to18_ct = Series_Complete_Yes - Count_age18to99_ct,
      Count_age18to64_ct = Count_age18to99_ct - Count_age65to99_ct,
      Count_total_ct = Series_Complete_Yes,
      Pct_age18to64_ct = Count_age18to64_ct/census_age18to64_ct*100,
      Pct_age0to18_ct = Count_age0to18_ct/census_age0to18_ct*100
    ) %>% 
    ungroup() %>% 
    group_by(FIPS, StateName) %>%
    mutate(census_age18to99_ct = round(mean(census_age18to99_ct, na.rm = TRUE)), 
           census_age65to99_ct = round(mean(census_age65to99_ct, na.rm = TRUE)),
           census_total_ct =     round(mean(census_total_ct, na.rm = TRUE)),
           census_age0to18_ct =  round(mean(census_age0to18_ct, na.rm = TRUE)),
           census_age18to64_ct = round(mean(census_age18to64_ct, na.rm = TRUE))) %>%
    filter(!duplicated(Date)) %>% 
    ungroup() %>%
    select(c(Date,
             FIPS,
             # County,
             StateName,
             Count_age0to18_ct, 
             Count_age18to64_ct, 
             Count_age65to99_ct
    )) -> CntDat
  
  
  ## all dates between min and max observed date
  allDates <- CntDat %>% 
    group_by(FIPS, StateName
             # , County
             ) %>%
    summarize(Date = seq.Date(min(Date), max(Date), by = 1), 
              .groups = 'drop')
  
  ## Create complete datase 
  completeCounty <- left_join(allDates, CntDat, 
                              by = c("Date","FIPS", "StateName"
                                     # , "County"
                                     )) %>% 
    left_join(CountyCensus, by = c("StateName", "FIPS"
                                   # , "County"
                                   )) %>% 
    group_by(FIPS, StateName) %>%
  # create variables to indicate the number of missings in the data  
    mutate(TotalMissings0to18 = sum(is.na(Count_age0to18_ct)),
           TotalMissings18to64 = sum(is.na(Count_age18to64_ct)),
           TotalMissings65to99 = sum(is.na(Count_age65to99_ct)),
           LengthDates = as.numeric(max(Date) - min(Date)),
           
           )  %>% 
    # if less than 3 observations are present, fill the count data of vaccine with national pct * county agegroup census
    mutate(
      Count_age0to18_ct = if_else(TotalMissings0to18 > (LengthDates - 2),
                                  {replacement <- as.numeric(
                                    as.matrix(
                                      nationalData[which(nationalData$Date %in% Date),"Pct_age0to18"]
                                    )
                                  ) 
                                  if(min(Date) < min(nationalData$Date)){
                                  out <-  c(rep(NA, as.numeric(min(nationalData$Date) - min(Date))),
                                      replacement);
                                  out * census_age0to18_ct;
                                  } else {replacement* census_age0to18_ct;}
                                  },
                                  as.double(Count_age0to18_ct)) ,
      Count_age18to64_ct = if_else(TotalMissings18to64 > (LengthDates - 2),
                                   {replacement <- as.numeric(
                                     as.matrix(
                                       nationalData[which(nationalData$Date %in% Date),"Pct_age18to64"]
                                     )
                                   )
                                   if(min(Date) < min(nationalData$Date)){
                                     out <-  c(rep(NA, as.numeric(min(nationalData$Date) - min(Date))),
                                               replacement);
                                     out * census_age18to64_ct;
                                   } else {replacement* census_age18to64_ct}},
                                   as.double(Count_age18to64_ct)),
      Count_age65to99_ct = if_else(TotalMissings65to99 > (LengthDates - 2),
                                  { replacement <- as.numeric(
                                     as.matrix(
                                       nationalData[which(nationalData$Date %in% Date),"Pct_age65to99"]
                                     )
                                   )
                                   if(min(Date) < min(nationalData$Date)){
                                     out <-  c(rep(NA, as.numeric(min(nationalData$Date) - min(Date))),
                                               replacement);
                                     out * census_age65to99_ct;
                                   } else { replacement * census_age65to99_ct}},
                                   as.double(Count_age65to99_ct))
    )  %>%
    ungroup() %>%
    # replace the remaining missing values with the moving average
    group_by(FIPS, StateName) %>%
    mutate(
      Count_age0to18_ct = imputeTS::na_ma(Count_age0to18_ct, k = 1),
      Count_age18to64_ct = imputeTS::na_ma(Count_age18to64_ct, k = 1),
      Count_age65to99_ct = imputeTS::na_ma(Count_age65to99_ct, k = 1),
    )  %>% 
     ungroup() %>%
    # drop the variables relating the missingness
     select(-c(TotalMissings0to18,
               TotalMissings18to64,
               TotalMissings65to99,
               LengthDates)) %>%
    ## compute the more finegrained agegroups, using  the national level agegroups percentages
    inner_join(nationalData, by = "Date") %>%
    group_by(FIPS, StateName) %>%
    mutate(Count_age0to18_ct_lag = c(Count_age0to18_ct[1], diff(Count_age0to18_ct, 1)),
           Count_age18to64_ct_lag = c(Count_age18to64_ct[1], diff(Count_age18to64_ct, 1)),
           Count_age65to99_ct_lag = c(Count_age65to99_ct[1], diff(Count_age65to99_ct, 1)) ,
           Count_age0to12_ct =relPct_age0to12 *Count_age0to18_ct_lag,
           Count_age12to15_ct = relPct_age12to15*Count_age0to18_ct_lag,
           Count_age16to17_ct = relPct_age16to17*Count_age0to18_ct_lag,
           Count_age18to24_ct = relPct_age18to24*Count_age18to64_ct_lag,
           Count_age25to39_ct = relPct_age25to39*Count_age18to64_ct_lag,
           Count_age40to49_ct = relPct_age40to49*Count_age18to64_ct_lag,
           Count_age50to64_ct = relPct_age50to64*Count_age18to64_ct_lag,
           Count_age65to74_ct = relPct_age65to74*Count_age65to99_ct_lag,
           Count_age75to99_ct = relPct_age75to99*Count_age65to99_ct_lag, 
           Count_age0to12_ctCS=   cumsum(Count_age0to12_ct),
           Count_age12to15_ctCS = cumsum(Count_age12to15_ct),
           Count_age16to17_ctCS = cumsum(Count_age16to17_ct),
           Count_age18to24_ctCS = cumsum(Count_age18to24_ct),
           Count_age25to39_ctCS = cumsum(Count_age25to39_ct),
           Count_age40to49_ctCS = cumsum(Count_age40to49_ct),
           Count_age50to64_ctCS = cumsum(Count_age50to64_ct),
           Count_age65to74_ctCS = cumsum(Count_age65to74_ct),
           Count_age75to99_ctCS = cumsum(Count_age75to99_ct),
           Date = Date)   %>% 
    ungroup()    %>%
    select(c(Date, FIPS,
             StateName, 
             ends_with("ctCS"),
             starts_with("census")))%>%
    select(-c("census_age0to18_ct", "census_age18to64_ct", "census_age65to99_ct")) %>% 
    group_by(Date)  %>%
    group_modify(~stateImpute(.x, stateCensus)) %>%
    ungroup()  %>% 
    group_by(FIPS, StateName) %>%
    # project the last observations from observed county estimates
    group_modify(~ projectionCounty(.x, maxVac = maxCoverage[1:9], nda = nda, endDate = endDate)) %>%
    group_by(FIPS, StateName)   %>%
    # prefill based on national data
    group_modify(~preFill(.x,natDat = nationalData))  %>%
    ungroup()   %>% 
    mutate(
      Pct_age0to12_ct =   Count_age0to12_ctCS /census_age0to12_ct,
      Pct_age12to15_ct = Count_age12to15_ctCS/census_age12to15_ct,
      Pct_age16to17_ct = Count_age16to17_ctCS/census_age16to17_ct,
      Pct_age18to24_ct = Count_age18to24_ctCS/census_age18to24_ct,
      Pct_age25to39_ct = Count_age25to39_ctCS/census_age25to39_ct,
      Pct_age40to49_ct = Count_age40to49_ctCS/census_age40to49_ct,
      Pct_age50to64_ct = Count_age50to64_ctCS/census_age50to64_ct,
      Pct_age65to74_ct = Count_age65to74_ctCS/census_age65to74_ct,
      Pct_age75to99_ct = Count_age75to99_ctCS/census_age75to99_ct,
      Pct_age0to12_ct = if_else(Pct_age0to12_ct > maxCoverage[1],
                                maxCoverage[1],
                                Pct_age0to12_ct), 
      Pct_age12to15_ct = if_else(Pct_age12to15_ct > maxCoverage[2],
                                 maxCoverage[2],
                                 Pct_age12to15_ct),
      Pct_age16to17_ct = if_else(Pct_age16to17_ct > maxCoverage[3],
                                 maxCoverage[3],
                                 Pct_age16to17_ct),
      Pct_age18to24_ct = if_else(Pct_age18to24_ct > maxCoverage[4],
                                 maxCoverage[4],
                                 Pct_age18to24_ct),
      Pct_age25to39_ct = if_else(Pct_age25to39_ct > maxCoverage[5],
                                 maxCoverage[5],
                                 Pct_age25to39_ct),
      Pct_age40to49_ct = if_else(Pct_age40to49_ct > maxCoverage[6],
                                 maxCoverage[6],
                                 Pct_age40to49_ct),
      Pct_age50to64_ct = if_else(Pct_age50to64_ct > maxCoverage[7],
                                 maxCoverage[7],
                                 Pct_age50to64_ct),
      Pct_age65to74_ct = if_else(Pct_age65to74_ct > maxCoverage[8],
                                 maxCoverage[8],
                                 Pct_age65to74_ct),
      Pct_age75to99_ct = if_else(Pct_age75to99_ct > maxCoverage[9],
                                 maxCoverage[9],
                                 Pct_age75to99_ct),
      Date = Date)  %>% 
    select(Date, 
                              FIPS,
                              StateName,
                              Pct_age0to12_ct ,
                              Pct_age12to15_ct,
                              Pct_age16to17_ct,
                              Pct_age18to24_ct,
                              Pct_age25to39_ct,
                              Pct_age40to49_ct,
                              Pct_age50to64_ct,
                              Pct_age65to74_ct,
                              Pct_age75to99_ct) -> final
  final
  
}

## function to prefill cntdata with natdata
preFill <- function(cntDat, natDat){
  
  firstNatDat <- min(natDat$Date)
  firstCntDat <- min(cntDat$Date)
  dayDiff   <- as.numeric(firstCntDat - firstNatDat)
  if(dayDiff <= 0){return(cntDat)}
  
  FirstObs <- cntDat[1,colnames(cntDat)[startsWith(colnames(cntDat), "Count_")]]
  NatTillFirst <- natDat[1:dayDiff+1, ] %>% select(starts_with("Pct_"))
  
  preVac <- matrix(NA, nrow = dayDiff, ncol = 9)
  
  for(j in 1:9){
    preVac[,j] <- as.numeric(FirstObs[,j])* (as.matrix(NatTillFirst[,j]) / as.numeric(as.matrix(NatTillFirst[dayDiff,j])))
  }
  census <- cntDat %>% ungroup() %>% select(starts_with("census_"))
  
  preDates <- as.Date(seq.Date(firstNatDat, firstCntDat - 1, 1))
  preDat <- data.frame(preDates,preVac,census[1,])
  colnames(preDat) <- colnames(cntDat)
  
  compDat <- rbind(preDat, cntDat)
  compDat
}

projectionCounty <- function(dat, maxVac = rep(.8, 9), nda = 14, endDate = as.Date("2022-12-31")){
  
  lastDate  <- dat[[nrow(dat),"Date"]]
  dayDiff   <- as.numeric(endDate - lastDate)
  nda <- if(nrow(dat) <= nda) {nrow(dat)-1} else {nda}
  Date      <- seq.Date(lastDate+1, endDate, by = 1)

  LastCen <- dat[1,colnames(dat)[startsWith(colnames(dat), "census_")]]

  LastObs <- dat[nrow(dat),colnames(dat)[startsWith(colnames(dat), "Count_")]]
  rateObs <- dat[nrow(dat) - nda, colnames(dat)[startsWith(colnames(dat), "Count_")]]
  obsVac <- LastObs/LastCen
  rateVac <- rateObs/LastCen
  vacRate <- (as.numeric(obsVac) - as.numeric(rateVac))/(nda-1)
  
  nGroup    <- length(vacRate)
  
  postVac   <- postCount<- matrix(NA, nrow = dayDiff, ncol = nGroup)
  
   vacRate[which(vacRate <= 0)] <- 0
  maxVac[which(maxVac <= obsVac)] <- as.numeric(as.matrix(obsVac[which(maxVac <= obsVac)]))
  
  for(j in 1:nGroup){
    values <- pexp(seq(0, dayDiff-1, by = 1), rate = vacRate[j])
    postVac[,j] <- as.numeric(as.matrix(obsVac[j])) + values*(maxVac[j] - as.numeric(as.matrix(obsVac[j])))
    postCount[,j] <- as.numeric(postVac[,j]) * as.numeric(as.matrix(LastCen[,j]))
  }
  census <- dat %>% ungroup() %>% select(starts_with("census_"))

  res       <- data.frame(Date, postCount, census[1,])
  colnames(res) <- colnames(dat)
  
  final <- rbind(dat, res)
  final
  
}

stateImpute <- function(groupdata, stateCensus){
  stateTotal <- groupdata %>% group_by(StateName) %>% summarise_at(vars(ends_with("_ctCS")), sum, na.rm = TRUE)
  stateCensus <- stateCensus %>% select(-c(County))
  allState <- right_join(stateCensus, stateTotal, by = "StateName")
  res <- data.frame("FIPS" = allState$StateName, allState$StateName, 
                    select(allState, starts_with("Count_")), 
                    select(allState, starts_with("census")))
  colnames(res) <- colnames(groupdata)
  
  final <- rbind(groupdata, res)
  final
}
