### source
# setwd("") ### set wd to location file
library("tidyverse")
# source("nationalData.R")
# source("countyCensus.R")
# source("countyData.R")
# source("deathData.R")
# source("ifr.R")

##### INPUTS #####
run <- function() {
  maxCoverage <- c(.5, .75,.75, # age0-12,12-15,16-17kids  https://www.abcactionnews.com/news/coronavirus/kff-research-3-in-10-parents-of-kids-12-15-will-get-their-child-vaccinated-for-covid-19-right-away
                   .85, .82, .82, # age 18-24,25-39,40-49adults https://www.kff.org/coronavirus-covid-19/dashboard/kff-covid-19-vaccine-monitor-dashboard/
                   .87, .92, .92, # age 50-64,65-74,75-99
                   mean(c(.5,.75,.75)), mean(c(.87,.85,.82,.82)), .92) #composites, for the age groups 0-18;18-65 and 65-99)


  nda  <- 14
  endDate <- as.Date("2022-12-31")
  vacEff <- .8
  sevEff <- .598

  deathsfile <- system.file(
    "csv", "Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State_01-10-2021.csv",
    package = "vaccineAdjust"
  )

  # createCountyCensus() ### don't run if you have StateCensus.RDS and CountyCensus.RDS files!
  # StateCensus   <- readRDS("StateCensus.RDS")
  # Census        <- readRDS("CountyCensus.RDS")
  Census$County <- gsub(" city", " City", Census$County) # fix to match the county names that end in city to City

  #### Process of the different datasources.
  deathsData    <- createDeaths(deathsfile)

  createNationData(maxCoverage, nda, endDate) %>% ## this step creates national level projected vaccines data
    createCountyData(Census, maxCoverage, StateCensus) %>% # this step create county level projected vaccines data by fine grained agegroup
    left_join(deathsData, by = "StateName") %>% # add the deaths data to the dataframe
    group_by(FIPS, StateName) %>% # compute for each FIPS/StateName combination the relative Risk
    computeRelativeRisk(vacEff = vacEff, sevEff = sevEff) %>% 
    ungroup() -> completeCounty

  completeCounty

  ## view to see if result makes sens
  # completeCounty %>% 
  #   filter(StateName == "Arizona" & FIPS == "Arizona") %>% 
  #   ggplot(aes(x = Date, y = RR)) + geom_line()
}
