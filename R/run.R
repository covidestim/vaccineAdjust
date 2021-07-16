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

  ps <- cli_process_start
  pd <- cli_process_done

  maxCoverage <- c(.5, .75,.75, # age0-12,12-15,16-17kids  https://www.abcactionnews.com/news/coronavirus/kff-research-3-in-10-parents-of-kids-12-15-will-get-their-child-vaccinated-for-covid-19-right-away
                   .85, .82, .82, # age 18-24,25-39,40-49adults https://www.kff.org/coronavirus-covid-19/dashboard/kff-covid-19-vaccine-monitor-dashboard/
                   .87, .92, .92, # age 50-64,65-74,75-99
                 mean(c(.5,.75,.75)), mean(c(.87,.85,.82,.82)), .92) #composites, for the age groups 0-18;18-65 and 65-99)


  nda  <- 14
  endDate <- as.Date("2022-12-31")
  vacEff <- .8
  sevEff <- .598

  provisionalDeathsPath <- "Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State_01-10-2021.csv"

  ps("Reading internal file {.file {provisionalDeathsPath}}")
  deathsfile <- system.file(
    "csv", # Refers to subdirectory of 'inst/'
    provisionalDeathsPath,
    package = "vaccineAdjust"
  )
  pd()

  # `Census` is a package-local object generated in 'data-raw/'
  modifiedCensus <- Census 

  # fix to match the county names that end in city to City
  modifiedCensus$County <- gsub(" city", " City", modifiedCensus$County) 

  ### Process the different datasources.
  ps("Creating age-stratified deaths by state")
  deathsData <- createDeaths(deathsfile)
  pd()

  ps("Creating national-level vax projections, assuming {.code maxCoverage} = {.emph {maxCoverage}}")
  ## this step creates national level projected vaccines data
  nationData <- createNationData(maxCoverage, nda, endDate) 
  pd()

  # this step create county level projected vaccines data by fine grained agegroup
  ps("Creating county-level vax projections, assuming {.code maxCoverage} = {.emph {maxCoverage}}")
  countyData <- createCountyData(nationData, Census, maxCoverage, StateCensus) 
  pd()

  # add the deaths data to the dataframe
  ps("Computing state-level relative risks, {.code vacEff} = {.emph {vacEff}}, {.code sevEff} = {.emph {sevEff}}")
  joined <- left_join(countyData, deathsData, by = "StateName") %>% 
    # compute for each FIPS/StateName combination the relative Risk
    group_by(FIPS, StateName) %>% 
    computeRelativeRisk(vacEff = vacEff, sevEff = sevEff) %>% 
    ungroup() -> completeCounty
  pd()

  completeCounty
}

## view to see if result makes sens
# completeCounty %>% 
#   filter(StateName == "Arizona" & FIPS == "Arizona") %>% 
#   ggplot(aes(x = Date, y = RR)) + geom_line()
