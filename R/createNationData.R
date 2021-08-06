## National Data ##
## Specs:
## Expected input: data frame with ndays rows (until today)
## with variables: Date, demographic_category, and some pct and count vars

## Desired output: 
## data frame with ndays rows (from beginning df untill 2022-12-31)
## variables: Date, pct per agegroup category

######## Manipulations needed
## sort by date ascending (date from first till last)
## compute composite age group variables
## make percentage a rate
## select and rename PctAgegroup*** variables
## check if all dates between max and min are observed, mean fill
## long to wide format
## project forward

## data required: 
# national data input
# maximum coverage per agegroup
# number of days used for projection
# end date
createNationData <- function(maxCoverage = c(.77, .77,.76, 
                                             .85, .82, .82,
                                             .87, .92, .92,
                                             mean(c(.5,.75,.75)), 
                                             mean(c(.85,.82,.82,.87)), 
                                             mean(c(.92,.92))),
                             nda = 14,
                             endDate = as.Date("2022-12-31")){

# download data from github
nationData  <- read_csv("https://raw.githubusercontent.com/covidestim/cdc-vaccine-data/master/national_agedist.csv")

# reformat and select variables
NatDat <- transmute(nationData, 
                 Demographic_category = factor(Demographic_category, 
                                               labels = c("age0to12", "age12to15", "age16to17", 
                                                          "age18to24", "age25to39", "age40to49",
                                                          "age50to64", "age65to74", "age75to99")),
                 Date = as.Date(Date),
                 Pct = Series_Complete_Pop_pct_agegroup/100,
                 census = census) %>%
  arrange(Date)

# all dates between min dat and max date
allDates <- NatDat %>%
  group_by(Demographic_category) %>%
  summarize(Date = seq.Date(min(Date), max(Date), by = 1), 
            .groups = 'drop')

# complete dataset if missing dates exist
complete <- left_join(allDates, NatDat, 
                      by = c("Date", "Demographic_category"))

# if any na's from missing dates; fill them with a moving average mean
if(any(is.na(complete$Pct))) {
  complete <- complete %>% 
    group_by("Demographic_category") %>%
    mutate(Pct = imputeTS::na_ma(Pct)) %>%
    ungroup()
}

# long to wide + project forward
complete %>% 
  pivot_wider(names_from = Demographic_category,
              values_from = c(Pct, census)) %>% 
  mutate(Pct_age0to18 = (Pct_age0to12*census_age0to12 +
                        Pct_age12to15*census_age12to15 +
                        Pct_age16to17*census_age16to17) / (census_age0to12 + 
                                                           census_age12to15 +
                                                           census_age16to17),
         Pct_age18to64 =  (Pct_age18to24*census_age18to24 +
                           Pct_age25to39*census_age25to39 +
                           Pct_age40to49*census_age40to49 +
                           Pct_age50to64*census_age50to64) /   (census_age18to24 + 
                                                                census_age25to39 +
                                                                census_age40to49 +
                                                                census_age50to64),
         Pct_age65to99 =  (Pct_age65to74*census_age65to74 +
                           Pct_age75to99*census_age75to99) /   (census_age65to74 +
                                                                census_age75to99)
         )  -> tempDat 
tempDat %>%
  select(c(Date, starts_with("Pct_"))) %>%
  projection(maxVac = maxCoverage, 
             nda = nda, 
             endDate = endDate) -> projectNation

ProjAndCens <- cbind(projectNation, tempDat[1,startsWith(colnames(tempDat),
                                                         "census")])  
ProjAndCens%>%
  mutate(relPct_age0to12 = Pct_age0to12*census_age0to12/((Pct_age0to12*census_age0to12 + 
                                                            Pct_age12to15*census_age12to15 +
                                                            Pct_age16to17*census_age16to17)+1),
         relPct_age12to15 = Pct_age12to15*census_age12to15/((Pct_age0to12*census_age0to12 + 
                                                               Pct_age12to15*census_age12to15 +
                                                               Pct_age16to17*census_age16to17)+1),
         relPct_age16to17 = Pct_age16to17*census_age16to17/((Pct_age0to12*census_age0to12 + 
                                                               Pct_age12to15*census_age12to15 +
                                                               Pct_age16to17*census_age16to17)+1),
         relPct_age18to24 = Pct_age18to24*census_age18to24/((Pct_age18to24*census_age18to24 + 
                                                               Pct_age25to39*census_age25to39 +
                                                               Pct_age40to49*census_age40to49 +
                                                               Pct_age50to64*census_age50to64)+1),
         relPct_age25to39 = Pct_age25to39*census_age25to39/((Pct_age18to24*census_age18to24 + 
                                                               Pct_age25to39*census_age25to39 +
                                                               Pct_age40to49*census_age40to49 +
                                                               Pct_age50to64*census_age50to64)+1),
         relPct_age40to49 = Pct_age40to49*census_age40to49/((Pct_age18to24*census_age18to24 + 
                                                               Pct_age25to39*census_age25to39 +
                                                               Pct_age40to49*census_age40to49 +
                                                               Pct_age50to64*census_age50to64)+1),
         relPct_age50to64 = Pct_age50to64*census_age50to64/((Pct_age18to24*census_age18to24 + 
                                                               Pct_age25to39*census_age25to39 +
                                                               Pct_age40to49*census_age40to49 +
                                                               Pct_age50to64*census_age50to64)+1),
         relPct_age65to74 = Pct_age65to74*census_age65to74/((Pct_age65to74*census_age65to74 + 
                                                               Pct_age75to99*census_age75to99)+1),
         relPct_age75to99 = Pct_age75to99*census_age75to99/((Pct_age65to74*census_age65to74 + 
                                                               Pct_age75to99*census_age75to99)+1)
         
  ) %>% select(c(Date, starts_with("Pct_"), starts_with("relPct_"))) -> final

final
}

## function to project data into future
projection <- function(dat, maxVac = rep(.8, 9), nda = 14, endDate = as.Date("2022-12-31")){
  
  lastDate  <- dat[[nrow(dat),"Date"]]
  dayDiff   <- as.numeric(endDate - lastDate)
  Date      <- seq.Date(lastDate+1, endDate, by = 1)
  lastVac   <- as.numeric(dat[nrow(dat),startsWith(colnames(dat), "Pct")])
  nGroup    <- length(lastVac)
  
  postVac   <- matrix(NA, nrow = dayDiff, ncol = nGroup)
  
  vacRate   <- as.numeric(lastVac - (dat[(nrow(dat) - nda),
                                         startsWith(colnames(dat), "Pct")]))/(nda+1)
  vacRate[which(vacRate <= 0)] <- 0.001
  maxVac[which(maxVac <= lastVac)] <- lastVac[which(maxVac <= lastVac)]
  
  for(j in 1:nGroup){
    values <- pexp(seq(0, dayDiff-1, by = 1), rate = vacRate[j])
    postVac[,j] <- lastVac[j] + values*(maxVac[j] - lastVac[j])
  }
  
  res       <- data.frame(Date, postVac)
  colnames(res) <- colnames(dat)
  
  rbind(dat, res)
  
}
