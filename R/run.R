library("tidyverse")

#' Run the script
#'
#' Description of what the script does
#'
#' @return Description of the return value
#'
#' @examples
#' result <- run()
#'
#' @export
run <- function() {

  ps <- cli_process_start
  pd <- cli_process_done

  maxCoverage <- c(
    # age0-12,12-15,16-17kids
    #https://www.abcactionnews.com/news/coronavirus/kff-research-3-in-10-parents-of-kids-12-15-will-get-their-child-vaccinated-for-covid-19-right-away
    .77, .77,.76, 

    # age 18-24,25-39,40-49adults
    # https://www.kff.org/coronavirus-covid-19/dashboard/kff-covid-19-vaccine-monitor-dashboard/
    .85, .82, .82, 

    # age 50-64,65-74,75-99
    .87, .92, .92, 

    #composites, for the age groups 0-18;18-65 and 65-99)
    mean(c(.77,.77,.76)), mean(c(.85,.82,.82,.87)), mean(c(.92,.92))
  ) 


  nda  <- 14
  endDate <- as.Date("2022-12-31")
  
  ## Refernces for Vaccine effectiveness.
  ## Overview of Vaccine effectiveness research
  ## https://www.cdc.gov/coronavirus/2019-ncov/science/science-briefs/fully-vaccinated-people.html#ref15
  
  ## .875 is the average of .89 (ref 1 below) and .86 (ref 2 below) reported 
  ## for reduced Covid infection in general US population
  ## .68 is derived from the vaccine effectiveness for hospitalization 
  ## in the US general population of .96 (ref 3 below), which is used as
  ## a substitute for reduced deaths
  ## (1-.875)*(1-.68) = (1-.96)
  ### 1 ### Pawlowski C LP, Puranik A, et. al. FDA-authorized COVID-19 vaccines are effective per real-world evidence synthesized across a multi-state health system. medRxiv. 2021;https://www.medrxiv.org/content/10.1101/2021.02.15.21251623v1.full.pdfpdf iconexternal icon.
  ### 2 ### Andrejko K. PJ, Myers JF., et al. Early evidence of COVID-19 vaccine effectiveness within the general population of California. MedRxiv. 2021;https://www.medrxiv.org/content/10.1101/2021.04.08.21255135v1external icon.
  ### 3 ### Vahidy FS. PL, Tano ME., et al. Real World Effectiveness of COVID-19 mRNA Vaccines against Hospitalizations and Deaths in the United States. medRxiv. 2021;https://www.medrxiv.org/content/10.1101/2021.04.21.21255873v1external icon.
  vacEff <- .875 ##
  sevEff <- .68

  provisionalDeathsSystemPath <-
    "Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State_01-10-2021.csv"

  ps("Reading internal file {.file {provisionalDeathsSystemPath}}")
  deathsfile <- system.file(
    "csv", # Refers to subdirectory of 'inst/'
    provisionalDeathsSystemPath,
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
  countyData <- createCountyData(nationData, modifiedCensus, maxCoverage, StateCensus) 
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
