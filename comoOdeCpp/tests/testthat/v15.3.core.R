# require("deSolve")
# library("ggplot2")
# library("dplyr")
# library("reshape2")
# require(gridExtra)
# library(ggpubr)
# library(bsplus)
# library(deSolve)
# library(DT)
# library(highcharter)
# library(lubridate)
# library(pushbar)
# library(readxl)
# library(reshape2)
# library(scales)
# library(shiny)
# library(shinyBS)
# library(shinycssloaders)
# library(shinyhelper)
# library(shinythemes)
# library(shinyWidgets)
# library(tidyverse)
# library(XLConnect)
# # library("comoOdeCpp")

# #read data from excel file
# setwd("C:/covid19/covid_age")
# load("data_CoMo.RData")
# file_path <- paste0(getwd(),"/Template_CoMoCOVID-19App_new.xlsx")  
country_name<-"United Kingdom of Great Britain"

# Cases
dta <- read_excel(file_path, sheet = "Cases")
names(dta) <- c("date", "cases", "deaths")

cases_rv <- dta %>%
  mutate(date = as.Date(date), cumulative_death = cumsum(deaths)) %>%
  as.data.frame()

# Severity/Mortality
dta <- read_excel(file_path, sheet = "Severity-Mortality") 
names(dta) <- c("age_category",	"ifr",	"ihr")

mort_sever_rv <- dta %>%
  mutate(ihr = ihr/100) %>% # starting unit should be % - scaling to a value between 0 and 1
  mutate(ifr = ifr/max(ifr))  # starting unit should be % - scaling to a value between 0 and 1

# Population
dta <- read_excel(file_path, sheet = "Population")
names(dta) <- c("age_category",	"pop",	"birth",	"death")

population_rv <- dta %>%
  transmute(country = NA, age_category, pop, birth, death)

# Parameters
param <- bind_rows(read_excel(file_path, sheet = "Parameters"),
                   read_excel(file_path, sheet = "Country Area Param"),
                   read_excel(file_path, sheet = "Virus Param"),
                   read_excel(file_path, sheet = "Hospitalisation Param"),
                   read_excel(file_path, sheet = "Interventions Param"),
                   read_excel(file_path, sheet = "Interventions")) %>%
  mutate(Value_Date = as.Date(Value_Date))

# START Bridge ----
popstruc <- population_rv %>% 
  select(age_category, pop) %>% 
  rename(agefloor = age_category) %>% 
  as.data.frame()

popbirth <- population_rv %>% 
  select(age_category, birth) %>% 
  as.data.frame() # unit should be per person per day

mort <- population_rv %>% 
  pull(death) # unit should be per person per day

ihr <- mort_sever_rv %>% 
  select(age_category, ihr) %>% 
  as.data.frame()

ifr <- mort_sever_rv %>% 
  select(age_category, ifr) %>% 
  as.data.frame()


#########    POP AGEING
# per year ageing matrix
A<-length(popstruc[,2])
dd<-seq(1:A)/seq(1:A)
ageing <- t(diff(diag(dd),lag=1)/(5*365.25))
ageing<-cbind(ageing,0*seq(1:A)) # no ageing from last compartment

#
pop<-population$country==country_name
pp<-population$pop[pop]
###  CONTACT MATRICES
c_home <- contact_home[[country_name]] %>% as.matrix()
c_school <- contact_school[[country_name]] %>% as.matrix()
c_work <- contact_work[[country_name]] %>% as.matrix()
c_other <- contact_other[[country_name]] %>% as.matrix()
nce <-A-length(c_home[1,])

contact_home<-matrix(0,nrow=A,ncol=A)
contact_school<-matrix(0,nrow=A,ncol=A)
contact_work<-matrix(0,nrow=A,ncol=A)
contact_other<-matrix(0,nrow=A,ncol=A)

for (i in 1:(A-nce)){
  for (j in 1:(A-nce)){
    contact_home[i,j]<-c_home[i,j]
    contact_school[i,j]<-c_school[i,j]
    contact_work[i,j]<-c_work[i,j]
    contact_other[i,j]<-c_other[i,j]
  }
}

for (i in (A+1-nce):A){
  for (j in 1:(A-nce)){
    contact_home[i,j]<-c_home[(A-nce),j]
    contact_school[i,j]<-c_school[(A-nce),j]
    contact_work[i,j]<-c_work[(A-nce),j]
    contact_other[i,j]<-c_other[(A-nce),j]
  }
}
for (i in 1:(A-nce)){
  for (j in (A+1-nce):A){
    contact_home[i,j]<-c_home[i,(A-nce)]
    contact_school[i,j]<-c_school[i,(A-nce)]
    contact_work[i,j]<-c_work[i,(A-nce)]
    contact_other[i,j]<-c_other[i,(A-nce)]
  }
}
for (i in (A+1-nce):A){
  for (j in (A+1-nce):A){
    contact_home[i,j]<-c_home[(A-nce),(A-nce)]
    contact_school[i,j]<-c_school[(A-nce),(A-nce)]
    contact_work[i,j]<-c_work[(A-nce),(A-nce)]
    contact_other[i,j]<-c_other[(A-nce),(A-nce)]
  }
}



#########   INITIALISE SIMULATION/INTERVENTION START TIMES
startdate <- param$Value_Date[param$Parameter == "date_range_simul_start"]
stopdate <- param$Value_Date[param$Parameter == "date_range_simul_end"]
startdate <- startdate[1]
stopdate <- stopdate[1]


day_start <- as.numeric(startdate-startdate)
day_stop <- as.numeric(stopdate-startdate)
times <- seq(day_start, day_stop)

tin<-as.numeric(startdate-as.Date("2020-01-01"))/365.25
initP<-sum(popstruc[,2])       # population size 
ageindcase<-20                 # age of index case (years)
aci <- floor((ageindcase/5)+1) # age class of index case


#############   DEFINE PARAMETERS
parameters <- c(

  ###  Transmission instrinsic
  p = param$Value[param$Parameter=="p"][1],
  rho = param$Value[param$Parameter=="rho"][1],
  omega = param$Value[param$Parameter=="omega"][1],
  gamma = param$Value[param$Parameter=="gamma"][1],
  nui = param$Value[param$Parameter=="nui"][1],
  report = param$Value[param$Parameter=="report"][1],
  reportc = param$Value[param$Parameter=="reportc"][1],
  reporth = param$Value[param$Parameter=="reporth"][1],
  beds_available = param$Value[param$Parameter=="beds_available"][1],
  icu_beds_available = param$Value[param$Parameter=="icu_beds_available"][1],
  ventilators_available = param$Value[param$Parameter=="ventilators_available"][1],
  give = 95,
  pdeath_h = mean( param$Value[param$Parameter=="pdeath_h"],na.rm=T),
  pdeath_ho = mean( param$Value[param$Parameter=="pdeath_ho"],na.rm=T),
  pdeath_hc = mean( param$Value[param$Parameter=="pdeath_hc"],na.rm=T),
  pdeath_hco = mean( param$Value[param$Parameter=="pdeath_hco"],na.rm=T),
  pdeath_icu = mean( param$Value[param$Parameter=="pdeath_icu"],na.rm=T),
  pdeath_icuo = mean( param$Value[param$Parameter=="pdeath_icuo"],na.rm=T),
  pdeath_icuc = mean( param$Value[param$Parameter=="pdeath_icuc"],na.rm=T),
  pdeath_icuco = mean( param$Value[param$Parameter=="pdeath_icuco"],na.rm=T),
  pdeath_vent = mean( param$Value[param$Parameter=="pdeath_vent"],na.rm=T),
  pdeath_ventc = mean( param$Value[param$Parameter=="pdeath_ventc"],na.rm=T),
  ihr_scaling = param$Value[param$Parameter=="ihr_scaling"][1],
  nus = param$Value[param$Parameter=="nus"][1],
  nusc = param$Value[param$Parameter=="nus"][1], # nusc = nus
  nu_icu = param$Value[param$Parameter=="nu_icu"][1],
  nu_icuc = param$Value[param$Parameter=="nu_icu"][1],  # nu_icuc = nu_icu
  nu_vent = param$Value[param$Parameter=="nu_vent"][1],
  nu_ventc = param$Value[param$Parameter=="nu_vent"][1], # nu_ventc = nu_vent
  rhos = param$Value[param$Parameter=="rhos"][1],
  amp = param$Value[param$Parameter=="amp"][1],
  phi = param$Value[param$Parameter=="phi"][1],
  pclin = param$Value[param$Parameter=="pclin"][1],
  prob_icu = param$Value[param$Parameter=="prob_icu"][1],
  prob_vent = param$Value[param$Parameter=="prob_vent"][1],
  propo2 = param$Value[param$Parameter=="propo2"][1],
  dexo2 = mean( param$Value[param$Parameter=="dexo2"],na.rm=T),
  dexo2c = mean( param$Value[param$Parameter=="dexo2c"],na.rm=T),
  dexv = mean( param$Value[param$Parameter=="dexvc"],na.rm=T),
  dexvc = mean( param$Value[param$Parameter=="dexvc"],na.rm=T),
  vent_dex = mean(param$Value[param$Parameter=="vent_dex"],na.rm=T),
  
  ###  INTERVENTIONS
  # self isolation
  selfis_eff = mean(param$Value[param$Parameter=="selfis_eff"],na.rm=T),
  # social distancing
  dist_eff = mean(param$Value[param$Parameter=="dist_eff"],na.rm=T),
  # hand washing
  hand_eff = mean(param$Value[param$Parameter=="hand_eff"],na.rm=T),
  # mask wearing
  mask_eff = mean(param$Value[param$Parameter=="mask_eff"],na.rm=T),
  # working at home
  work_eff = mean(param$Value[param$Parameter=="work_eff"],na.rm=T),
  w2h = mean(param$Value[param$Parameter=="w2h"],na.rm=T),
  # school closures
  school_eff = mean(param$Value[param$Parameter=="school_eff"],na.rm=T),
  s2h = mean(param$Value[param$Parameter=="s2h"],na.rm=T),
  # cocooning the elderly
  cocoon_eff = mean(param$Value[param$Parameter=="cocoon_eff"],na.rm=T),
  age_cocoon = mean(param$Value[param$Parameter=="age_cocoon"],na.rm=T),
  # vaccination campaign
  # vaccine_on = as.numeric(param$Value_Date[param$Parameter=="date_vaccine_on"] - startdate),
  vaccine_eff = mean(param$Value[param$Parameter=="vaccine_eff"],na.rm=T),
  age_vaccine_min = mean(param$Value[param$Parameter=="age_vaccine_min"],na.rm=T),
  # vaccine_cov = param$Value[param$Parameter=="vaccine_cov"],
  vac_campaign = mean(param$Value[param$Parameter=="vac_campaign"],na.rm=T),
  # travel ban
  mean_imports = mean(param$Value[param$Parameter=="mean_imports"],na.rm=T),
  # screening
  screen_test_sens = mean(param$Value[param$Parameter=="screen_test_sens"],na.rm=T),
  # screen_contacts = mean(param$Value[param$Parameter=="screen_contacts"],na.rm=T),
  screen_overdispersion = mean(param$Value[param$Parameter=="screen_overdispersion"],na.rm=T),
  # voluntary home quarantine
  quarantine_days = mean(param$Value[param$Parameter=="quarantine_days"],na.rm=T),
  quarantine_effort = mean(param$Value[param$Parameter=="quarantine_effort"],na.rm=T),
  quarantine_eff_home = mean(param$Value[param$Parameter=="quarantine_eff_home"],na.rm=T),
  quarantine_eff_other = mean(param$Value[param$Parameter=="quarantine_eff_other"],na.rm=T),
  # mass testing
  age_testing_min = mean(param$Value[param$Parameter=="age_testing_min"],na.rm=T),
  age_testing_max = mean(param$Value[param$Parameter=="age_testing_max"],na.rm=T),
  mass_test_sens = mean(param$Value[param$Parameter=="mass_test_sens"],na.rm=T),
  isolation_days = mean(param$Value[param$Parameter=="isolation_days"],na.rm=T),
  
  ###  Initialisation
  # init = param$Value[param$Parameter=="init"][1],
  
  ### Others
  household_size = param$Value[param$Parameter=="household_size"][1],
  noise = param$Value[param$Parameter=="noise"][1],
  iterations = param$Value[param$Parameter=="iterations"][1],
  confidence = param$Value[param$Parameter=="confidence"][1]
)
ihr[,2]<- parameters["ihr_scaling"]*ihr[,2]   

# Scale parameters to percentages/ rates
parameters["rho"]<-parameters["rho"]/100
parameters["omega"]<-(1/(parameters["omega"]*365))
parameters["gamma"]<-1/parameters["gamma"]
parameters["nui"]<-1/parameters["nui"]
parameters["report"]<-parameters["report"]/100
parameters["reportc"]<-parameters["reportc"]/100
parameters["reporth"]<-parameters["reporth"]/100
parameters["nus"]<-1/parameters["nus"]
parameters["rhos"]<-parameters["rhos"]/100
parameters["amp"]<-parameters["amp"]/100
parameters["selfis_eff"]<-parameters["selfis_eff"]/100
parameters["dist_eff"]<-parameters["dist_eff"]/100
parameters["hand_eff"]<-parameters["hand_eff"]/100
parameters["mask_eff"]<-parameters["mask_eff"]/100
parameters["work_eff"]<-parameters["work_eff"]/100
parameters["w2h"]<-parameters["w2h"]/100
parameters["school_eff"]<-parameters["school_eff"]/100
parameters["s2h"]<-parameters["s2h"]/100
parameters["cocoon_eff"]<-parameters["cocoon_eff"]/100
parameters["age_cocoon"]<-floor((parameters["age_cocoon"]/5)+1)
parameters["vaccine_eff"]<-parameters["vaccine_eff"]/100
age_vaccine_min<-(parameters["age_vaccine_min"])
# parameters["vaccine_cov"]<-parameters["vaccine_cov"]/100
# parameters["vac_campaign"]<-parameters["vac_campaign"]*7
parameters["screen_test_sens"]<-parameters["screen_test_sens"]/100
parameters["quarantine_days"]<-parameters["quarantine_days"]
parameters["quarantine_effort"]<-1/parameters["quarantine_effort"]
parameters["quarantine_eff_home"]<-parameters["quarantine_eff_home"]/-100
parameters["quarantine_eff_other"]<-parameters["quarantine_eff_other"]/100
parameters["give"]<-parameters["give"]/100
parameters["pdeath_h"]<-parameters["pdeath_h"]/100
parameters["pdeath_ho"]<-parameters["pdeath_ho"]/100
parameters["pdeath_hc"]<-parameters["pdeath_hc"]/100
parameters["pdeath_hco"]<-parameters["pdeath_hco"]/100
parameters["pdeath_icu"]<-parameters["pdeath_icu"]/100
parameters["pdeath_icuo"]<-parameters["pdeath_icuo"]/100
parameters["pdeath_icuc"]<-parameters["pdeath_icuc"]/100
parameters["pdeath_icuco"]<-parameters["pdeath_icuco"]/100
parameters["pdeath_vent"]<-parameters["pdeath_vent"]/100
parameters["pdeath_ventc"]<-parameters["pdeath_ventc"]/100
parameters["nusc"]<-1/parameters["nusc"]
parameters["nu_icu"]<-1/parameters["nu_icu"]
parameters["nu_icuc"]<-1/parameters["nu_icuc"]
parameters["nu_vent"]<-1/parameters["nu_vent"]
parameters["nu_ventc"]<-1/parameters["nu_ventc"]
parameters["pclin"]<-parameters["pclin"]/100
parameters["prob_icu"]<-parameters["prob_icu"]/100
parameters["prob_vent"]<-parameters["prob_vent"]/100
iterations<-parameters["iterations"]
noise<-parameters["noise"]
confidence<-parameters["confidence"]/100
parameters["mass_test_sens"]<-parameters["mass_test_sens"]/100
age_testing_min<-(parameters["age_testing_min"])
age_testing_max<-(parameters["age_testing_max"])
parameters["isolation_days"]<-parameters["isolation_days"]
parameters["propo2"]<-parameters["propo2"]/100
parameters["dexo2"]<-parameters["dexo2"]/100
parameters["dexo2c"]<-parameters["dexo2c"]/100
parameters["dexv"]<-parameters["dexv"]/100
parameters["dexvc"]<-parameters["dexvc"]/100
parameters["vent_dex"]<-parameters["vent_dex"]/100
# parameters_noise<-c(1:5,19:20,22,24,26,32:39,43,45,47:49,66)
parameters_noise <- c("p", "rho", "omega", "gamma", "nui", "ihr_scaling","nus", "nu_icu","nu_vent",
                      "rhos", "selfis_eff", "dist_eff", "hand_eff", "mask_eff", "work_eff", 
                      "w2h", "school_eff", "s2h", "cocoon_eff", "mean_imports", "screen_overdispersion", 
                      "quarantine_effort", "quarantine_eff_home", "quarantine_eff_other")

###########################################################################
# Define the indices for each variable
Sindex<-1:A
Eindex<-(A+1):(2*A)
Iindex<-(2*A+1):(3*A)
Rindex<-(3*A+1):(4*A)
Xindex<-(4*A+1):(5*A)
Hindex<-(5*A+1):(6*A)
HCindex<-(6*A+1):(7*A)
Cindex<-(7*A+1):(8*A)
CMindex<-(8*A+1):(9*A)
Vindex<-(9*A+1):(10*A)
QSindex<-(10*A+1):(11*A)
QEindex<-(11*A+1):(12*A)
QIindex<-(12*A+1):(13*A)
QRindex<-(13*A+1):(14*A)
CLindex<-(14*A+1):(15*A)
QCindex<-(15*A+1):(16*A)
ICUindex<-(16*A+1):(17*A)
ICUCindex<-(17*A+1):(18*A)
ICUCVindex<-(18*A+1):(19*A)
Ventindex<-(19*A+1):(20*A)
VentCindex<-(20*A+1):(21*A)
CMCindex<-(21*A+1):(22*A)
Zindex<-(22*A+1):(23*A)

###########################################################################
# MODEL INITIAL CONDITIONS
initI<-0*popstruc[,2]  # Infected and symptomatic
initE<-0*popstruc[,2]  # Incubating
initE[aci]<-1          # place random index case in E compartment
# initE[aci]<-round(sum(popstruc[,2])/parameters["init"])     # place random index case in E compartment
initR<-0*popstruc[,2]  # Immune
initX<-0*popstruc[,2]  # Isolated 
initV<-0*popstruc[,2]  # Vaccinated 
initQS<-0*popstruc[,2] # quarantined S 
initQE<-0*popstruc[,2] # quarantined E  
initQI<-0*popstruc[,2] # quarantined I  
initQR<-0*popstruc[,2] # quarantined R  
initH<-0*popstruc[,2]  # hospitalised 
initHC<-0*popstruc[,2] # hospital critical 
initC<-0*popstruc[,2]  # Cumulative cases (true)
initCM<-0*popstruc[,2] # Cumulative deaths (true)
initCL<-0*popstruc[,2] # symptomatic cases
initQC<-0*popstruc[,2] # quarantined C 
initICU<-0*popstruc[,2]   # icu
initICUC<-0*popstruc[,2]  # icu critical
initICUCV<-0*popstruc[,2]  # icu critical
initVent<-0*popstruc[,2]  # icu vent
initVentC<-0*popstruc[,2] # icu vent crit
initCMC<-0*popstruc[,2]   # Cumulative deaths (true)
initZ<-0*popstruc[,2]   # Cumulative deaths (true)
initS<-popstruc[,2]-initE-initI-initR-initX-initZ-initV-initH-initHC-initQS-initQE-initQI-initQR-initCL-initQC-initICU-initICUC-initICUCV-initVent-initVentC  # Susceptible (non-immune)


inp <- read_excel(file_path, sheet = "Interventions")
inputs<-function(inp, run){
  # cap intervention end dates with simulation end date
  # inp$`Date End` = pmin(stopdate, inp$`Date End`)
  inp$`Date End` = pmin(stopdate, as.Date(inp$`Date End`))
  
  tv<-which(inp$`Apply to`==run)
  
  si<-intersect(which(inp$Intervention=="Self-isolation if Symptomatic"),tv)
  scr<-intersect(which(inp$Intervention=="Screening (when S.I.)"),tv)
  sd<-intersect(which(inp$Intervention=="Social Distancing"),tv)
  hw<-intersect(which(inp$Intervention=="Handwashing"),tv)
  msk<-intersect(which(inp$Intervention=="Masking"),tv)
  wah<-intersect(which(inp$Intervention=="Working at Home"),tv)
  sc<-intersect(which(inp$Intervention=="School Closures"),tv)
  cte<-intersect(which(inp$Intervention=="Shielding the Elderly"),tv)
  q<-intersect(which(inp$Intervention=="Household Isolation (when S.I.)"),tv)
  tb<-intersect(which(inp$Intervention=="International Travel Ban"),tv)
  vc<-intersect(which(inp$Intervention=="Vaccination"),tv)
  mt<-intersect(which(inp$Intervention=="Mass Testing"),tv)
  minas<-intersect(which(inp$Intervention=="Age Testing Minimum"),tv)
  maxas<-intersect(which(inp$Intervention=="Age Testing Maximum"),tv)
  vc<-intersect(which(inp$Intervention=="Vaccination"),tv)
  minav<-intersect(which(inp$Intervention=="Age Vaccine Minimum"),tv)
  dx<-intersect(which(inp$Intervention=="Dexamethasone"),tv)
  
  v<-(format(as.POSIXct(inp$`Date Start`,format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
  v2<-as.Date(v,format="%d/%m/%y")
  inp$`Date Start`<-v2
  
  v<-(format(as.POSIXct(inp$`Date End`,format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
  v2<-as.Date(v,format="%d/%m/%y")
  inp$`Date End`<-v2
  
  ##  self isolation
  f<-c()
  si_vector<-c()
  isolation<-c()
  if (length(si)>=1){
    for (i in 1:length(si)){
      f<-c(f,as.numeric(inp$`Date Start`[si[i]]-startdate),as.numeric(inp$`Date End`[si[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[si[i]]>startdate){
          si_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[si[i]],(f[i+1]-f[i])*20))
          isolation<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          si_vector<-c(rep(inp$`Value`[si[i]],(f[i+1])*20))
          isolation<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        si_vector<-c(si_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        si_vector<-c(si_vector,rep(inp$`Value`[si[i]],(f[i*2]-f[i*2-1])*20))
        isolation<-c(isolation,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        isolation<-c(isolation,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(si) && f[i*2]<tail(times,1)){
        si_vector<-c(si_vector,rep(0,(tail(times,1)-f[i*2])*20))
        isolation<-c(isolation,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    si_vector<-rep(0,tail(times,1)*20)
    isolation<-rep(0,tail(times,1)*20)
  }
  ## social distancing
  f<-c()
  sd_vector<-c()
  distancing<-c()
  if (length(sd)>=1){
    for (i in 1:length(sd)){
      
      f<-c(f,as.numeric(inp$`Date Start`[sd[i]]-startdate),as.numeric(inp$`Date End`[sd[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[sd[i]]>startdate){
          sd_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[sd[i]],(f[i+1]-f[i])*20))
          distancing<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          sd_vector<-c(rep(inp$`Value`[sd[i]],(f[i+1])*20))
          distancing<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        sd_vector<-c(sd_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        sd_vector<-c(sd_vector,rep(inp$`Value`[sd[i]],(f[i*2]-f[i*2-1])*20))
        distancing<-c(distancing,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        distancing<-c(distancing,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(sd)&& f[i*2]<tail(times,1)){
        sd_vector<-c(sd_vector,rep(0,(tail(times,1)-f[i*2])*20))
        distancing<-c(distancing,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    sd_vector<-rep(0,tail(times,1)*20)
    distancing<-rep(0,tail(times,1)*20)
  }
  ## screening
  f<-c()
  scr_vector<-c()
  screen<-c()
  if (length(scr)>=1){
    for (i in 1:length(scr)){
      
      f<-c(f,as.numeric(inp$`Date Start`[scr[i]]-startdate),as.numeric(inp$`Date End`[scr[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[scr[i]]>startdate){
          scr_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[scr[i]],(f[i+1]-f[i])*20))
          screen<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          scr_vector<-c(rep(inp$`Value`[scr[i]],(f[i+1])*20))
          screen<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        scr_vector<-c(scr_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        scr_vector<-c(scr_vector,rep(inp$`Value`[scr[i]],(f[i*2]-f[i*2-1])*20))
        screen<-c(screen,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        screen<-c(screen,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(scr)&& f[i*2]<tail(times,1)){
        scr_vector<-c(scr_vector,rep(0,(tail(times,1)-f[i*2])*20))
        screen<-c(screen,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    scr_vector<-rep(0,tail(times,1)*20)
    screen<-rep(0,tail(times,1)*20)
  }
  ## handwashing
  f<-c()
  hw_vector<-c()
  handwash<-c()
  if (length(hw)>=1){
    for (i in 1:length(hw)){
      
      f<-c(f,as.numeric(inp$`Date Start`[hw[i]]-startdate),as.numeric(inp$`Date End`[hw[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[hw[i]]>startdate){
          hw_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[hw[i]],(f[i+1]-f[i])*20))
          handwash<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          hw_vector<-c(rep(inp$`Value`[hw[i]],(f[i+1])*20))
          handwash<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        hw_vector<-c(hw_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        hw_vector<-c(hw_vector,rep(inp$`Value`[hw[i]],(f[i*2]-f[i*2-1])*20))
        handwash<-c(handwash,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        handwash<-c(handwash,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(hw)&& f[i*2]<tail(times,1)){
        hw_vector<-c(hw_vector,rep(0,(tail(times,1)-f[i*2])*20))
        handwash<-c(handwash,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    hw_vector<-rep(0,tail(times,1)*20)
    handwash<-rep(0,tail(times,1)*20)
  }
  ## masking
  f<-c()
  msk_vector<-c()
  masking<-c()
  if (length(msk)>=1){
    for (i in 1:length(msk)){
      
      f<-c(f,as.numeric(inp$`Date Start`[msk[i]]-startdate),as.numeric(inp$`Date End`[msk[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[msk[i]]>startdate){
          msk_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[msk[i]],(f[i+1]-f[i])*20))
          masking<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          msk_vector<-c(rep(inp$`Value`[msk[i]],(f[i+1])*20))
          masking<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        msk_vector<-c(msk_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        msk_vector<-c(msk_vector,rep(inp$`Value`[msk[i]],(f[i*2]-f[i*2-1])*20))
        masking<-c(masking,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        masking<-c(masking,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(msk)&& f[i*2]<tail(times,1)){
        msk_vector<-c(msk_vector,rep(0,(tail(times,1)-f[i*2])*20))
        masking<-c(masking,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    msk_vector<-rep(0,tail(times,1)*20)
    masking<-rep(0,tail(times,1)*20)
  }
  ## dexamethasone
  f<-c()
  dex<-c()
  if (length(dx)>=1){
    for (i in 1:length(dx)){
      f<-c(f,as.numeric(inp$`Date Start`[dx[i]]-startdate),as.numeric(inp$`Date End`[dx[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[dx[i]]>startdate){
          dex<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          dex<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        dex<-c(dex,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        dex<-c(dex,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(dx)&& f[i*2]<tail(times,1)){
        dex<-c(dex,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    dex<-rep(0,tail(times,1)*20)
  }
  ## working at home
  f<-c()
  wah_vector<-c()
  workhome<-c()
  if (length(wah)>=1){
    for (i in 1:length(wah)){
      
      f<-c(f,as.numeric(inp$`Date Start`[wah[i]]-startdate),as.numeric(inp$`Date End`[wah[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[wah[i]]>startdate){
          wah_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[wah[i]],(f[i+1]-f[i])*20))
          workhome<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          wah_vector<-c(rep(inp$`Value`[wah[i]],(f[i+1])*20))
          workhome<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        wah_vector<-c(wah_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        wah_vector<-c(wah_vector,rep(inp$`Value`[wah[i]],(f[i*2]-f[i*2-1])*20))
        workhome<-c(workhome,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        workhome<-c(workhome,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(wah)&& f[i*2]<tail(times,1)){
        wah_vector<-c(wah_vector,rep(0,(tail(times,1)-f[i*2])*20))
        workhome<-c(workhome,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    wah_vector<-rep(0,tail(times,1)*20)
    workhome<-rep(0,tail(times,1)*20)
  }
  ## school closure
  f<-c()
  sc_vector<-c()
  schoolclose<-c()
  if (length(sc)>=1){
    for (i in 1:length(sc)){
      
      f<-c(f,as.numeric(inp$`Date Start`[sc[i]]-startdate),as.numeric(inp$`Date End`[sc[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[sc[i]]>startdate){
          sc_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[sc[i]],(f[i+1]-f[i])*20))
          schoolclose<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          sc_vector<-c(rep(inp$`Value`[sc[i]],(f[i+1])*20))
          schoolclose<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        sc_vector<-c(sc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        sc_vector<-c(sc_vector,rep(inp$`Value`[sc[i]],(f[i*2]-f[i*2-1])*20))
        schoolclose<-c(schoolclose,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        schoolclose<-c(schoolclose,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(sc)&& f[i*2]<tail(times,1)){
        sc_vector<-c(sc_vector,rep(0,(tail(times,1)-f[i*2])*20))
        schoolclose<-c(schoolclose,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    sc_vector<-rep(0,tail(times,1)*20)
    schoolclose<-rep(0,tail(times,1)*20)
  }
  ## cocooning the elderly
  f<-c()
  cte_vector<-c()
  cocoon<-c()
  if (length(cte)>=1){
    for (i in 1:length(cte)){
      
      f<-c(f,as.numeric(inp$`Date Start`[cte[i]]-startdate),as.numeric(inp$`Date End`[cte[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[cte[i]]>startdate){
          cte_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[cte[i]],(f[i+1]-f[i])*20))
          cocoon<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          cte_vector<-c(rep(inp$`Value`[cte[i]],(f[i+1])*20))
          cocoon<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        cte_vector<-c(cte_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        cte_vector<-c(cte_vector,rep(inp$`Value`[cte[i]],(f[i*2]-f[i*2-1])*20))
        cocoon<-c(cocoon,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        cocoon<-c(cocoon,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(cte)&& f[i*2]<tail(times,1)){
        cte_vector<-c(cte_vector,rep(0,(tail(times,1)-f[i*2])*20))
        cocoon<-c(cocoon,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    cte_vector<-rep(0,tail(times,1)*20)
    cocoon<-rep(0,tail(times,1)*20)
  }
  ## quarantine
  f<-c()
  q_vector<-c()
  quarantine<-c()
  if (length(q)>=1){
    for (i in 1:length(q)){
      
      f<-c(f,as.numeric(inp$`Date Start`[q[i]]-startdate),as.numeric(inp$`Date End`[q[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[q[i]]>startdate){
          q_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[q[i]],(f[i+1]-f[i])*20))
          quarantine<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          q_vector<-c(rep(inp$`Value`[q[i]],(f[i+1])*20))
          quarantine<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        q_vector<-c(q_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        q_vector<-c(q_vector,rep(inp$`Value`[q[i]],(f[i*2]-f[i*2-1])*20))
        quarantine<-c(quarantine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        quarantine<-c(quarantine,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(q)&& f[i*2]<tail(times,1)){
        q_vector<-c(q_vector,rep(0,(tail(times,1)-f[i*2])*20))
        quarantine<-c(quarantine,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    q_vector<-rep(0,tail(times,1)*20)
    quarantine<-rep(0,tail(times,1)*20)
  }
  ## travel ban
  f<-c()
  tb_vector<-c()
  travelban<-c()
  if (length(tb)>=1){
    for (i in 1:length(tb)){
      
      f<-c(f,as.numeric(inp$`Date Start`[tb[i]]-startdate),as.numeric(inp$`Date End`[tb[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[tb[i]]>startdate){
          tb_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[tb[i]],(f[i+1]-f[i])*20))
          travelban<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          tb_vector<-c(rep(inp$`Value`[tb[i]],(f[i+1])*20))
          travelban<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        tb_vector<-c(tb_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        tb_vector<-c(tb_vector,rep(inp$`Value`[tb[i]],(f[i*2]-f[i*2-1])*20))
        travelban<-c(travelban,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        travelban<-c(travelban,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(tb)&& f[i*2]<tail(times,1)){
        tb_vector<-c(tb_vector,rep(0,(tail(times,1)-f[i*2])*20))
        travelban<-c(travelban,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    tb_vector<-rep(0,tail(times,1)*20)
    travelban<-rep(0,tail(times,1)*20)
  }
  ## mass testing
  f<-c()
  mt_vector<-c()
  masstesting<-c()
  if (length(mt)>=1){
    for (i in 1:length(mt)){
      
      f<-c(f,as.numeric(inp$`Date Start`[mt[i]]-startdate),as.numeric(inp$`Date End`[mt[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[mt[i]]>startdate){
          mt_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[mt[i]],(f[i+1]-f[i])*20))
          masstesting<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          mt_vector<-c(rep(inp$`Value`[mt[i]],(f[i+1])*20))
          masstesting<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        mt_vector<-c(mt_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        mt_vector<-c(mt_vector,rep(inp$`Value`[mt[i]],(f[i*2]-f[i*2-1])*20))
        masstesting<-c(masstesting,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        masstesting<-c(masstesting,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(mt)&& f[i*2]<tail(times,1)){
        mt_vector<-c(mt_vector,rep(0,(tail(times,1)-f[i*2])*20))
        masstesting<-c(masstesting,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    mt_vector<-rep(0,tail(times,1)*20)
    masstesting<-rep(0,tail(times,1)*20)
  }
  ## min age testing
  f<-c()
  minas_vector<-c()
  if (length(minas)>=1){
    for (i in 1:length(minas)){
      f<-c(f,as.numeric(inp$`Date Start`[minas[i]]-startdate),as.numeric(inp$`Date End`[minas[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[minas[i]]>startdate){
          minas_vector<-c(rep(age_testing_min,f[i]*20),rep(inp$`Value`[minas[i]],(f[i+1]-f[i])*20))
        }
        else{
          minas_vector<-c(rep(inp$`Value`[minas[i]],(f[i+1])*20))
        }
      }
      else{
        minas_vector<-c(minas_vector,rep(age_testing_min,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        minas_vector<-c(minas_vector,rep(inp$`Value`[minas[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(minas)&& f[i*2]<tail(times,1)){
        minas_vector<-c(minas_vector,rep(age_testing_min,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    minas_vector<-rep(age_testing_min,tail(times,1)*20)
  }
  ## max age testing
  f<-c()
  maxas_vector<-c()
  if (length(maxas)>=1){
    for (i in 1:length(maxas)){
      f<-c(f,as.numeric(inp$`Date Start`[maxas[i]]-startdate),as.numeric(inp$`Date End`[maxas[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[maxas[i]]>startdate){
          maxas_vector<-c(rep(age_testing_max,f[i]*20),rep(inp$`Value`[maxas[i]],(f[i+1]-f[i])*20))
        }
        else{
          maxas_vector<-c(rep(inp$`Value`[maxas[i]],(f[i+1])*20))
        }
      }
      else{
        maxas_vector<-c(maxas_vector,rep(age_testing_max,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        maxas_vector<-c(maxas_vector,rep(inp$`Value`[maxas[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(maxas)&& f[i*2]<tail(times,1)){
        maxas_vector<-c(maxas_vector,rep(age_testing_max,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    maxas_vector<-rep(age_testing_max,tail(times,1)*20)
  }
  ## vaccine
  f<-c()
  vc_vector<-c()
  vaccine<-c()
  if (length(vc)>=1){
    for (i in 1:length(vc)){
      
      f<-c(f,as.numeric(inp$`Date Start`[vc[i]]-startdate),as.numeric(inp$`Date End`[vc[i]]-startdate))
      
      if(i==1){
        if (inp$`Date Start`[vc[i]]>startdate){
          vc_vector<-c(rep(0,f[i]*20),rep(inp$`Value`[vc[i]],(f[i+1]-f[i])*20))
          vaccine<-c(rep(0,f[i]*20),rep(1,(f[i+1]-f[i])*20))
        }
        else{
          vc_vector<-c(rep(inp$`Value`[vc[i]],(f[i+1])*20))
          vaccine<-c(rep(1,(f[i+1])*20))
        }
      }
      else{
        vc_vector<-c(vc_vector,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        vc_vector<-c(vc_vector,rep(inp$`Value`[vc[i]],(f[i*2]-f[i*2-1])*20))
        vaccine<-c(vaccine,rep(0,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        vaccine<-c(vaccine,rep(1,(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(vc)&& f[i*2]<tail(times,1)){
        vc_vector<-c(vc_vector,rep(0,(tail(times,1)-f[i*2])*20))
        vaccine<-c(vaccine,rep(0,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    vc_vector<-rep(0,tail(times,1)*20)
    vaccine<-rep(0,tail(times,1)*20)
  }
  ## min age vaccine
  f<-c()
  minav_vector<-c()
  if (length(minav)>=1){
    for (i in 1:length(minav)){
      f<-c(f,as.numeric(inp$`Date Start`[minav[i]]-startdate),as.numeric(inp$`Date End`[minav[i]]-startdate))
      if(i==1){
        if (inp$`Date Start`[minav[i]]>startdate){
          minav_vector<-c(rep(age_vaccine_min,f[i]*20),rep(inp$`Value`[minav[i]],(f[i+1]-f[i])*20))
        }
        else{
          minav_vector<-c(rep(inp$`Value`[minav[i]],(f[i+1])*20))
        }
      }
      else{
        minav_vector<-c(minav_vector,rep(age_vaccine_min,(f[(i-1)*2+1]-f[(i-1)*2])*20))
        minav_vector<-c(minav_vector,rep(inp$`Value`[minav[i]],(f[i*2]-f[i*2-1])*20))
      }
      if(i==length(minav)&& f[i*2]<tail(times,1)){
        minav_vector<-c(minav_vector,rep(age_vaccine_min,(tail(times,1)-f[i*2])*20))
      }
    }
  }else{
    minav_vector<-rep(age_vaccine_min,tail(times,1)*20)
  }
  return(list(si_vector=si_vector,sd_vector=sd_vector,scr_vector=scr_vector,hw_vector=hw_vector,msk_vector=msk_vector,wah_vector=wah_vector,
              sc_vector=sc_vector,tb_vector=tb_vector,mt_vector=mt_vector,cte_vector=cte_vector,q_vector=q_vector,vc_vector=vc_vector,isolation=isolation,
              screen=screen,cocoon=cocoon,schoolclose=schoolclose,workhome=workhome,handwash=handwash,masking=masking,
              quarantine=quarantine,vaccine=vaccine,travelban=travelban,distancing=distancing,masstesting=masstesting,
              maxas_vector=maxas_vector,minas_vector=minas_vector,minav_vector=minav_vector, dex=dex))
}
vectors0<-inputs(inp,'Baseline (Calibration)')
vectors<-inputs(inp,'Hypothetical Scenario')


f <- c(1,(1+parameters["give"])/2,(1-parameters["give"])/2,0)
KH<-parameters["beds_available"]
KICU<- parameters["icu_beds_available"]+parameters["ventilators_available"]
Kvent<- parameters["ventilators_available"]
x.H <- c(0,(1+parameters["give"])*KH/2,(3-parameters["give"])*KH/2,2*KH)
x.ICU <- c(0,(1+parameters["give"])*KICU/2,(3-parameters["give"])*KICU/2,2*KICU)
x.Vent <- c(0,(1+parameters["give"])*Kvent/2,(3-parameters["give"])*Kvent/2,2*Kvent)
fH <- splinefun(x.H, f, method = "hyman")
fICU <- splinefun(x.ICU, f, method = "hyman")
fVent<- splinefun(x.Vent, f, method = "hyman")


# set up a function to solve the equations
covid<-function(t, Y, parameters,input) 
{
  with(as.list(c(Y, parameters)),
       {
         S <- Y[Sindex]
         E <- Y[Eindex]
         I <- Y[Iindex]
         R <- Y[Rindex]
         X <- Y[Xindex]
         Z <- Y[Zindex]
         H <- Y[Hindex]
         HC <- Y[HCindex]
         C <- Y[Cindex]
         CM <- Y[CMindex]
         V <- Y[Vindex]
         QS <- Y[QSindex]
         QE <- Y[QEindex]
         QI <- Y[QIindex]
         QR <- Y[QRindex]
         CL <- Y[CLindex]
         QC <- Y[QCindex]
         ICU <- Y[ICUindex]
         ICUC <- Y[ICUCindex]
         ICUCV <-Y[ICUCVindex]
         Vent <- Y[Ventindex]
         VentC <- Y[VentCindex]
         CMC <- Y[CMCindex]
         P <- (S+E+I+R+X+Z+V+H+HC+QS+QE+QI+QR+CL+QC+ICU+ICUC+ICUCV+Vent+VentC)
         Q <- (sum(QS)+sum(QE)+sum(QI)+sum(QC)+sum(QR))/sum(P)
         # print(sum(P))
         
         # health system performance
         critH<-min(1-fH(sum(H)+sum(ICUC)+sum(ICUCV)),1)
         crit<-min(1-fICU(sum(ICU)+sum(Vent)+sum(VentC)),1)
         critV<-min(1-fVent(sum(Vent)),1)
         
         # interventions
         isolation<-input$isolation[t*20+1]
         distancing<-input$distancing[t*20+1]
         handwash<-input$handwash[t*20+1]
         masking<-input$masking[t*20+1]
         workhome<-input$workhome[t*20+1]
         schoolclose<-input$schoolclose[t*20+1]
         cocoon<-input$cocoon[t*20+1]
         vaccine<-input$vaccine[t*20+1]
         travelban<-input$travelban[t*20+1]
         screen<-input$screen[t*20+1]
         quarantine<-input$quarantine[t*20+1]
         masstesting<-input$masstesting[t*20+1]
         dexamethasone<-input$dex[t*20+1]
         
         screen_eff<-0
         selfis<-0
         school<-1
         dist<-1
         hand<-0
         mask<-0
         vaccinate<-0
         trvban_eff<-0
         quarantine_rate<-0
         tests_per_day<-0
         
         selfis_cov<-(input$si_vector[t*20+1])/100
         screen_contacts<-(input$scr_vector[t*20+1])/10
         school_eff<-(input$sc_vector[t*20+1])/100
         dist_cov<-(input$sd_vector[t*20+1])/100
         hand_cov<-(input$hw_vector[t*20+1])/100
         mask_cov<-(input$msk_vector[t*20+1])/100
         cocoon<-(input$cte_vector[t*20+1])/100
         work_cov<-(input$wah_vector[t*20+1])/100
         travelban_eff<-(input$tb_vector[t*20+1])/100
         vaccine_cov<-(input$vc_vector[t*20+1])/100
         quarantine_cov<-(input$q_vector[t*20+1])/100
         tests_per_day<-(input$mt_vector[t*20+1])
         min_age_testing<-floor((input$minas_vector[t*20+1]/5)+1)
         max_age_testing<-floor((input$maxas_vector[t*20+1]/5)+1)
         age_testing_vector<-c(rep(0,min_age_testing-1),rep(1,max_age_testing-min_age_testing+1),rep(0,21-max_age_testing))
         min_age_vaccine<-floor((input$minav_vector[t*20+1]/5)+1)
         if (vaccine){
           age_vaccine_vector<-c(rep(0,min_age_vaccine-1),rep(1,21-min_age_vaccine+1))
         }else{age_vaccine_vector<-rep(0,A)}
         # print(age_testing_vector)
         
         if (workhome){
           work<-work_cov*work_eff
         }else{work<-1}
         if (isolation){
           selfis<-selfis_cov
           if(screen){
             screen_eff<-min(sum(report*I+reportc*(CL)+H+ICU+Vent+reporth*(HC+ICUC+ICUCV+VentC))*screen_contacts*(screen_overdispersion*I/P)*screen_test_sens/P,1) 
           }
         }
         if (schoolclose){
           school<-school_eff
         }
         if(distancing){
           dist<-dist_cov*dist_eff
         }
         if(handwash){
           hand<-hand_eff*hand_cov
         }
         if(masking){
           mask<-mask_eff*mask_cov
         }
         if(vaccine){
           vac_rate<-(-log(1-vaccine_cov)/vac_campaign)
           vaccinate<-vac_rate
         }
         if(travelban){
           trvban_eff<-travelban_eff
         }
         if(quarantine){
           rate_q<-min((min(sum((I+CL+H+ICU+Vent+HC+ICUC+ICUCV+VentC))*(household_size-1)/sum(P),1)*quarantine_effort),quarantine_cov/2)
           quarantine_rate<-rate_q/(1+exp(-10*(quarantine_cov/2-Q)))
         }
         if(dexamethasone){
           prob_v<-prob_vent*vent_dex
         }else{
           dexo2<-1;dexo2c<-1;dexv<-1;dexvc<-1;prob_v<-prob_vent;
         }
         # print(paste(dexo2,dexo2c,dexv,dexvc,prob_v))
         
         # testing rates
         propI<-sum(I)/sum(P)
         propC<-sum(CL)/sum(P)
         propE<-sum(E)/sum(P)
         testE<-tests_per_day*propE
         testI<-tests_per_day*propI
         testC<-tests_per_day*propC
         if(sum(I)>1){
           ratetestI<-mass_test_sens*testI/sum(I)
           # print(paste('rateI: ',ratetestI))
         }else{ratetestI<-0}
         if(sum(CL)>1){
           ratetestC<-mass_test_sens*testC/sum(CL)
           # print(paste('rateC: ',ratetestC))
         }else{ratetestC<-0}
         if(sum(E)>1){
           ratetestE<-mass_test_sens*testE/sum(E)
           # print(paste('rateC: ',ratetestC))
         }else{ratetestE<-0}
         
         # print(mass_test_sens)
         # print(ratetestI*sum(I) + ratetestC*sum(CL) - (1/isolation_days)*sum(Z) )
         # print(propC)
         # print(testI)
         # print(testC)
         # 
         # cocooning the elderly
         cocoon_mat<-matrix((1-cocoon_eff),nrow = length(popstruc$pop),ncol = length(popstruc$pop))
         cocoon_mat[1:(age_cocoon-1),1:(age_cocoon-1)]<-1
         
         # contact matrices
         cts<-(contact_home+distancing*(1-dist)*contact_other+(1-distancing)*contact_other
               +(1-schoolclose)*contact_school # school on
               +schoolclose*(1-school)*contact_school # school close
               +schoolclose*contact_home*school*s2h # inflating contacts at home when school closes
               +(1-workhome)*contact_work  # normal work
               +workhome*(1-work)*contact_work # people not working from home when homework is active
               +contact_home*workhome*work*w2h # inflating contacts at home when working from home
         )
         
         # Final transmission related parameters
         contacts <- (1-cocoon)*cts+cocoon*cts*cocoon_mat+cocoon*(1+schoolclose*(1-school_eff)+workhome*(1-work_eff))*contact_home*(1-cocoon_mat)
         seas <- 1+amp*cos(2*3.14*(t-(phi*365.25/12))/365.25)
         importation <- mean_imports*(1-trvban_eff)
         HH<-H+ICU+Vent+ICUC+ICUCV+VentC
         HHC<-HC
         lam <- (1-max(hand,mask))*p*seas*(contacts%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC)+rhos*(HH))/P))+
           (1-hand)*p*seas*(1-quarantine*quarantine_eff_other)*(contact_other%*%((rho*QE+QI+QC)/P))
         # contacts under home quarantine
         lamq<-(1-max(hand,mask))*p*seas*((1-quarantine_eff_home)*contact_home%*%(((1-selfis_eff)*(X+HHC+rho*QE+QI+QC))/P))+
           (1-hand)*p*seas*(1-quarantine_eff_other)*(contact_other%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC+rho*QE+QI+QC)+rhos*(HH))/P))
         
         # birth/death
         b1<-sum(popbirth[,2]*popstruc[,2])
         birth<-0*popbirth[,2]
         birth[1]<-b1
         
         # ODE system
         dSdt <- -S*lam-vaccinate*age_vaccine_vector*S+omega*R+ageing%*%S-mort*S+birth-quarantine_rate*S +(1/quarantine_days)*QS
         dEdt <- S*lam-gamma*E+ageing%*%E-mort*E + (1-vaccine_eff)*lam*V-quarantine_rate*E+(1/quarantine_days)*QE
         dIdt <- gamma*(1-pclin)*(1-age_testing_vector*ratetestE)*(1-screen_eff)*(1-ihr[,2])*E-nui*I+ageing%*%I-mort*I + (1/quarantine_days)*QI - quarantine_rate*I - ratetestI*age_testing_vector*I
         dCLdt<- gamma*pclin*(1-age_testing_vector*ratetestE)*(1-selfis)*(1-ihr[,2])*(1-quarantine_rate)*E-nui*CL+ageing%*%CL-mort*CL  + (1/quarantine_days)*QC - ratetestC*age_testing_vector*CL
         dRdt <- nui*I-omega*R+nui*X+nui*CL+ageing%*%R-mort*R + (1/isolation_days)*Z+(1/quarantine_days)*QR+ 
           nus*propo2*(1-dexo2*pdeath_ho)*ifr[,2]*H+nus*(1-propo2)*(1-pdeath_h)*ifr[,2]*H+nusc*propo2*(1-pdeath_hco)*ifr[,2]*HC+nusc*(1-propo2)*(1-pdeath_hc)*ifr[,2]*HC+           
           nu_icu*propo2*(1-dexo2*pdeath_icuo)*ifr[,2]*ICU+nu_icu*(1-propo2)*(1-pdeath_icu)*ifr[,2]*ICU+nu_icuc*propo2*(1-dexo2c*pdeath_icuco)*ifr[,2]*ICUC+nu_icuc*(1-propo2)*(1-pdeath_icuc)*ifr[,2]*ICUC+
           nu_vent*(1-dexv*pdeath_vent)*ifr[,2]*Vent+nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*VentC+nu_ventc*(1-dexvc*pdeath_ventc)*ifr[,2]*ICUCV 
         dXdt <- gamma*selfis*(1-age_testing_vector*ratetestE)*pclin*(1-ihr[,2])*E+gamma*(1-pclin)*(1-age_testing_vector*ratetestE)*screen_eff*(1-ihr[,2])*E-nui*X+ageing%*%X-mort*X 
         dVdt <- vaccinate*age_vaccine_vector*S - (1-vaccine_eff)*lam*V + ageing%*%V - mort*V
         
         dQSdt <- quarantine_rate*S+ ageing%*%QS-mort*QS - (1/quarantine_days)*QS - lamq*QS
         dQEdt <- quarantine_rate*E - gamma*QE + ageing%*%QE-mort*QE - (1/quarantine_days)*QE + lamq*QS 
         dQIdt <- quarantine_rate*I + gamma*(1-ihr[,2])*(1-pclin)*QE-nui*QI+ageing%*%QI-mort*QI - (1/quarantine_days)*QI
         dQCdt <- gamma*pclin*(1-selfis)*(1-age_testing_vector*ratetestE)*(1-ihr[,2])*quarantine_rate*E+gamma*(1-ihr[,2])*pclin*QE-nui*QC+ageing%*%QC-mort*QC - (1/quarantine_days)*QC
         dQRdt <- nui*QI+nui*QC+ageing%*%QR-mort*QR - (1/quarantine_days)*QR
         
         dHdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*reporth*E + gamma*ihr[,2]*(1-prob_icu)*(1-critH)*QE - nus*H + ageing%*%H-mort*H  
         dHCdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*(1-reporth)*E+gamma*ihr[,2]*(1-prob_icu)*critH*E + gamma*ihr[,2]*(1-prob_icu)*critH*QE - nusc*HC + ageing%*%HC-mort*HC 
         dICUdt <- gamma*ihr[,2]*prob_icu*(1-crit)*(1-prob_v)*E + gamma*ihr[,2]*prob_icu*(1-crit)*(1-prob_v)*QE - nu_icu*ICU +ageing%*%ICU - mort*ICU +(1-crit)*ICUC*1/2
         dICUCdt <- gamma*ihr[,2]*prob_icu*crit*(1-prob_v)*E + gamma*ihr[,2]*prob_icu*crit*(1-prob_v)*QE - 
           nu_icuc*ICUC -(1-crit)*ICUC*1/2 +ageing%*%ICUC - mort*ICUC 
         dICUCVdt <- gamma*ihr[,2]*prob_icu*prob_v*crit*E +gamma*ihr[,2]*prob_icu*prob_v*crit*QE -nu_ventc*ICUCV +ageing%*%ICUCV - mort*ICUCV - (1-critV)*ICUCV*1/2
         dVentdt <- gamma*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_v*E + gamma*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_v*QE +(1-critV)*VentC*1/2 +(1-critV)*ICUCV*1/2 -nu_vent*Vent +ageing%*%Vent - mort*Vent 
         dVentCdt <- gamma*ihr[,2]*prob_icu*prob_v*(1-crit)*critV*E +gamma*ihr[,2]*prob_icu*prob_v*(1-crit)*critV*QE - 
           (1-critV)*VentC*1/2 -nu_ventc*VentC +ageing%*%VentC - mort*VentC 
         
         dCdt <- report*gamma*(1-age_testing_vector*ratetestE)*(1-pclin)*(1-ihr[,2])*(E+QE)+reportc*gamma*pclin*(1-age_testing_vector*ratetestE)*(1-ihr[,2])*(E+QE)+
           gamma*ihr[,2]*(1-critH)*(1-prob_icu)*(E+QE)+gamma*ihr[,2]*critH*reporth*(1-prob_icu)*(E+QE)+
           gamma*ihr[,2]*prob_icu*(E+QE)+ratetestI*age_testing_vector*I+ratetestC*age_testing_vector*CL+gamma*age_testing_vector*ratetestE*(1-ihr[,2])*E
         dCMdt<- nus*propo2*dexo2*pdeath_ho*ifr[,2]*H+nus*(1-propo2)*pdeath_h*ifr[,2]*H+nusc*propo2*pdeath_hco*ifr[,2]*HC+nusc*(1-propo2)*pdeath_hc*ifr[,2]*HC+
           nu_icu*propo2*dexo2*pdeath_icuo*ifr[,2]*ICU+nu_icu*(1-propo2)*pdeath_icu*ifr[,2]*ICU+nu_icuc*propo2*dexo2c*pdeath_icuco*ifr[,2]*ICUC+nu_icuc*(1-propo2)*pdeath_icuc*ifr[,2]*ICUC+
           nu_vent*dexv*pdeath_vent*ifr[,2]*Vent+nu_ventc*dexvc*pdeath_ventc*ifr[,2]*VentC +nu_ventc*dexvc*pdeath_ventc*ifr[,2]*ICUCV+ 
           mort*H + mort*HC + mort*ICU + mort*ICUC + mort*ICUCV + mort*Vent + mort*VentC  + mort*Z + mort*V +
           mort*S +
           mort*E +
           mort*I +
           mort*CL +
           mort*X +
           mort*QS +
           mort*QE +
           mort*QI + 
           mort*QC + 
           mort*QR + 
           mort*R 
         dCMCdt <- nusc*propo2*pdeath_hco*ifr[,2]*HC+nusc*(1-propo2)*pdeath_hc*ifr[,2]*HC+
           nu_icuc*propo2*dexo2c*pdeath_icuco*ifr[,2]*ICUC+nu_icuc*(1-propo2)*pdeath_icuc*ifr[,2]*ICUC+
           nu_ventc*dexvc*pdeath_ventc*ifr[,2]*VentC+nu_ventc*dexvc*pdeath_ventc*ifr[,2]*ICUCV+
           mort*HC + mort*ICUC + mort*VentC + mort*ICUCV + mort*V
         
         dZdt <- gamma*ratetestE*age_testing_vector*(1-ihr[,2])*E+ratetestI*age_testing_vector*I+ratetestC*age_testing_vector*CL-(1/isolation_days)*Z-mort*Z
         
         # return the rate of change
         list(c(dSdt,dEdt,dIdt,dRdt,dXdt,dHdt,dHCdt,dCdt,dCMdt,dVdt,dQSdt,dQEdt,dQIdt,dQRdt,dCLdt,dQCdt,dICUdt,dICUCdt,dICUCVdt,dVentdt,dVentCdt,dCMCdt,dZdt))
       }
  ) 
}

###########    RUN BASELINE MODEL - start time for interventions is set to day 1e5, i.e. interventions are always off

Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initV, initQS, initQE, initQI, initQR, initCL, initQC, initICU, initICUC, initICUCV, initVent, initVentC, initCMC, initZ) # initial conditions for the main solution vector
