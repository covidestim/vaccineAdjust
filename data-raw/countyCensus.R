## County census data
## 
library(magrittr)
library(tidyverse)
tmp = tempfile(fileext = ".xlsx")
download.file(url = "https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/detail/sc-est2019-agesex-01.xlsx", destfile = tmp, mode = "wb")

stateCensus  <- readxl::read_excel(tmp, range = "AI7:AI24", col_names = FALSE)
stateCensus  <- data.frame(t(stateCensus), "StateName" = state.name[1])
stateName_DC <- c(state.name[1:8], "District of Columbia", state.name[9:50])

countyCensus <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-01.csv",
                         col_types = cols(SUMLEV = col_number(),
                                          STATE = col_character(),
                                          COUNTY = col_character(),
                                          STNAME = col_character(),
                                          CTYNAME = col_character(),
                                          .default = col_number()))
validN <- c(2,4:6,8:13,15:42,44:51,53:56)

 for(i in 1:length(validN)){
  if(validN[i] < 10){
  download.file(url = paste0("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/detail/sc-est2019-agesex-0",validN[i],".xlsx"), destfile = tmp,
                mode = "wb")
    temp <- read_csv(paste0("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-0",validN[i],".csv"),
                     col_types = cols(SUMLEV = col_number(),
                                      STATE = col_character(),
                                      COUNTY = col_character(),
                                      STNAME = col_character(),
                                      CTYNAME = col_character(),
                                      .default = col_number()))
  } else {
    download.file(url = paste0("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/detail/sc-est2019-agesex-",validN[i],".xlsx"), destfile = tmp,
                  mode = "wb")
  temp <- read_csv(paste0("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-",validN[i],".csv"),
                   col_types = cols(SUMLEV = col_number(),
                                    STATE = col_character(),
                                    COUNTY = col_character(),
                                    STNAME = col_character(),
                                    CTYNAME = col_character(),
                                    .default = col_number()))
  }
   
   tempState  <- readxl::read_excel(tmp, range = "AI7:AI24", col_names = FALSE)
   tempState  <- data.frame(t(tempState), "StateName" = stateName_DC[i+1])

   
  stateCensus  <- rbind(stateCensus, tempState)
  countyCensus <- rbind(countyCensus, temp)
}

countyCensus %>% filter(YEAR == 12) %>%
group_by(COUNTY, STATE, CTYNAME, STNAME) %>%
transmute(census_age0to12_ct = AGE04_TOT + AGE59_TOT + AGE1014_TOT/5*3,
          census_age12to15_ct = AGE1014_TOT/5*2 + AGE1519_TOT/5*1,
          census_age16to17_ct = AGE1519_TOT/5*2,
          census_age18to24_ct = AGE1824_TOT,
          census_age25to39_ct = AGE2529_TOT + AGE3034_TOT + AGE3539_TOT,
          census_age40to49_ct = AGE4044_TOT+ AGE4549_TOT,
          census_age50to64_ct = AGE5054_TOT + AGE5559_TOT + AGE6064_TOT,
          census_age65to74_ct = AGE6569_TOT + AGE7074_TOT,
          census_age75to99_ct = AGE7579_TOT + AGE8084_TOT + AGE85PLUS_TOT,
          census_age0to18_ct = AGE04_TOT + AGE59_TOT + 
            AGE1014_TOT + AGE1519_TOT/5*3,
          census_age18to64_ct = AGE1824_TOT + AGE2529_TOT + 
            AGE3034_TOT + AGE3539_TOT + AGE4044_TOT+ AGE4549_TOT +
            AGE5054_TOT + AGE5559_TOT + AGE6064_TOT,
          census_age65to99_ct = AGE6569_TOT + AGE7074_TOT +
            AGE7579_TOT + AGE8084_TOT + AGE85PLUS_TOT) %>%
mutate(County = gsub(" County", "", CTYNAME),
       StateName = usdata::state2abbr(STNAME),
       FIPS = paste0(STATE,COUNTY)) %>%
ungroup() %>%
select(-c(STNAME, CTYNAME)) -> Census # County census

stateCensus %>% 
transmute(census_age0to12_ct = round(stateCensus[,1] + stateCensus[,2] + stateCensus[,3]/5*3),
    census_age12to15_ct = round(stateCensus[,3]/5*2 + stateCensus[,4]/5*1),
    census_age16to17_ct = round(stateCensus[,4]/5*2),
    census_age18to24_ct = round(stateCensus[,4]/5*2+ stateCensus[,5]),
    census_age25to39_ct = round(stateCensus[,6] + stateCensus[,7] + stateCensus[,8]),
    census_age40to49_ct = round(stateCensus[,9] + stateCensus[,10]),
    census_age50to64_ct = round(stateCensus[,11] + stateCensus[,12] + stateCensus[,13]),
    census_age65to74_ct = round(stateCensus[,14]+stateCensus[,15]),
    census_age75to99_ct = round(stateCensus[,16] + stateCensus[,17] + stateCensus[,18]),
    County = StateName,
    StateName = usdata::state2abbr(StateName)
) %>%
ungroup() -> StateCensus

usethis::use_data(Census, StateCensus, internal = TRUE, overwrite = TRUE)

