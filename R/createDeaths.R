### Deaths

createDeaths <- function(deathsfile) {

  age_deaths <- read.csv(deathsfile)

  # remove overlapping age-groups
  kp_ag1 <- unique(age_deaths$Age.group)[c(2,4,5,6,8,10,11,13,14,15,16)]
  kp_ag2 <- age_deaths$Age.group%in%kp_ag1

  # remove 'all' sex group, as that only available at nat level.
  kp_sx1 <- unique(age_deaths$Sex)[2:3]
  kp_sx2 <- age_deaths$Sex%in%kp_sx1
  age_deaths1 <-  age_deaths[kp_ag2 & kp_sx2,4:7]

  # implement a simple mean imputation for interval-censored data
  tot_death1 <- age_deaths[age_deaths$Sex=="All Sexes" & age_deaths$Age.group=="All Ages",7][1]
  tot_death2 <- sum(age_deaths1$COVID.19.Deaths[!age_deaths1$State%in%c("United States","Puerto Rico")],na.rm=T)
  imp_death_n <- (tot_death1-tot_death2)/sum(is.na(age_deaths1$COVID.19.Deaths[!age_deaths1$State%in%c("United States","Puerto Rico")]))
  age_deaths1$COVID.19.Deaths_imp <- age_deaths1$COVID.19.Deaths
  age_deaths1$COVID.19.Deaths_imp[is.na(age_deaths1$COVID.19.Deaths)] <- imp_death_n

  # removeUnited States total, and add NYC into NY state
  age_deaths1$COVID.19.Deaths_imp[age_deaths1$State=="New York"] <- age_deaths1$COVID.19.Deaths_imp[age_deaths1$State=="New York"]+
    age_deaths1$COVID.19.Deaths_imp[age_deaths1$State=="New York City"]
  age_deaths1 <- age_deaths1[!age_deaths1$State%in%c("New York City","United States"),]

  age_deaths2 <- aggregate(COVID.19.Deaths_imp ~State+Age.group,age_deaths1,sum)


  age_deaths2$Age.group2 <- NA
  age_deaths2$Age.group2[age_deaths2$Age.group=="Under 1 year"] <- "age1"
  age_deaths2$Age.group2[age_deaths2$Age.group=="1-4 years"]    <- "age2"
  age_deaths2$Age.group2[age_deaths2$Age.group=="5-14 years"]   <- "age3"
  age_deaths2$Age.group2[age_deaths2$Age.group=="15-24 years"]  <- "age4"
  age_deaths2$Age.group2[age_deaths2$Age.group=="25-34 years"]  <- "age5"
  age_deaths2$Age.group2[age_deaths2$Age.group=="35-44 years"]  <- "age6"
  age_deaths2$Age.group2[age_deaths2$Age.group=="45-54 years"]  <- "age7"
  age_deaths2$Age.group2[age_deaths2$Age.group=="55-64 years"]  <- "age8"
  age_deaths2$Age.group2[age_deaths2$Age.group=="65-74 years"]  <- "age9"
  age_deaths2$Age.group2[age_deaths2$Age.group=="75-84 years"]  <- "age10"
  age_deaths2$Age.group2[age_deaths2$Age.group=="85 years and over"] <- "age11"

  age_deaths2 %>% 
    group_by(State) %>%
    select(-c(Age.group)) %>%
    pivot_wider(names_from = Age.group2, values_from = COVID.19.Deaths_imp) %>%
    select(c("age1","age2","age3","age4", "age5","age6","age7","age8","age9","age10","age11", State)) %>%
    # group_by(State) %>%
    transmute(StateName = State,
              Deaths_age0to12 = age1 + age2+ age3/10*8, 
           Deaths_age12to15 = age3/10*2 + age4/10*1, 
           Deaths_age16to17 = age4/10*2, 
           Deaths_age18to24 = age4/10*7,
           Deaths_age25to39 = age5 + age6/10*5, 
           Deaths_age40to49 = age6/10*5+age7/10*5, 
           Deaths_age50to64 = age7/10*5+age8,
           Deaths_age65to74 = age9, 
           Deaths_age75to99 = age10 + age11) %>%
    ungroup() %>%
    select(-c(State)) -> Deaths

  Deaths
}  
