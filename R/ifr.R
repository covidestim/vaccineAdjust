## IFR
computeRelativeRisk <- function(data, vacEff = .8, sevEff = .598){

Deaths <- data %>% ungroup() %>% select(starts_with("Deaths"))
Deaths <- as.numeric(Deaths[1,])
obsVac <- data %>% ungroup() %>% select(starts_with("Pct"))
groups <- data %>% select(-c(starts_with("Deaths"), starts_with("Pct")))
  
ifr_yoa <- 10^(-3.27 + 0.0524*(1:105-0.5))
ifrs <- sapply(list(1:12,13:15,16:17,18:24,25:39,40:49,50:64,65:74,75:105),function(x) sum(ifr_yoa[x]))
ifrsinv <- 1/ifrs

ImpInf  <-  Deaths/ifrs
ImpInfTotal <- sum(ImpInf) #sim rowSums(ImpInf)

ImpFrac <- ImpInf/ImpInfTotal

ImpIFR <- ImpFrac*(Deaths/sum(Deaths))
IFR <- ImpIFR

nAgeGroups <- length(Deaths)
if(length(vacEff) != nAgeGroups) vacEff <- rep(vacEff, nAgeGroups)
if(length(sevEff) != nAgeGroups) sevEff <- rep(sevEff, nAgeGroups)

# FracVac <- as.matrix(obsVac) %*% ImpFrac

IFRslevin <- comp_IFR(obsVac, ImpFrac, vacEff, sevEff, IFR)
RRvac <- IFRslevin[,1]/IFRslevin[1,1]

cbind(groups, "RR"= RRvac)

}

#### for IFR ######
comp_IFR <- function(obsVac, ImpFrac, vacEff, sevEff, IFR){
  ## Potential infections by age Vaccinated
  FracVac     <- sweep(as.matrix(obsVac), MARGIN = 2, ImpFrac, FUN = '*')
  ## Realized infections by age Vaccinated (1-vacEff is the reduced realized infections)
  FracInfVac  <- sweep(FracVac, MARGIN = 2, (1-vacEff), '*')
  
  # totale realized infections is a function of a reducation of
  # vacination coverage and vacination effect
  FracvacEff  <- 1-sweep(as.matrix(obsVac), MARGIN = 2, vacEff, '*')
  # proportion of infections in vaccinated agegroups relative to all infections
  inf_age_vac <- sweep(FracInfVac, MARGIN = 1, FracvacEff%*%ImpFrac, "/")
  
  ##Infections by age Unvaccinated
  FracUnVac   <- sweep(as.matrix(1-obsVac), MARGIN = 2, ImpFrac, '*')
  
  inf_age_unvac <- sweep(FracUnVac, MARGIN = 1, FracvacEff%*%ImpFrac, "/")
  avIFR   <- inf_age_vac%*%(IFR*(1-sevEff)) + inf_age_unvac%*%IFR
  avIFR2  <- (inf_age_vac+inf_age_unvac)%*%IFR
  
  return(cbind(avIFR, avIFR2))
}