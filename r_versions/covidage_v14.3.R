require("deSolve")
library("ggplot2")
library("dplyr")
library("reshape2")
require(gridExtra)
library(ggpubr)
library(bsplus)
library(deSolve)
library(DT)
library(highcharter)
library(lubridate)
library(pushbar)
library(readxl)
library(reshape2)
library(scales)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyhelper)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(XLConnect)
# library("comoOdeCpp")

#read data from excel file
setwd("C:/covid19/covid_age")
load("data_CoMo.RData")
file_path <- paste0(getwd(),"/Template_CoMoCOVID-19App_new.xlsx")  
country_name<-"Cambodia"

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
  pdeath_h = param$Value[param$Parameter=="pdeath_h"][1],
  pdeath_hc = param$Value[param$Parameter=="pdeath_hc"][1],
  pdeath_icu = param$Value[param$Parameter=="pdeath_icu"][1],
  pdeath_icuc = param$Value[param$Parameter=="pdeath_icuc"][1],
  pdeath_vent = param$Value[param$Parameter=="pdeath_vent"][1],
  pdeath_ventc = param$Value[param$Parameter=="pdeath_ventc"][1],
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
  
  ###  INTERVENTIONS
  # self isolation
  selfis_eff = mean(param$Value[param$Parameter=="selfis_eff"],na.rm=T),
  # social distancing
  dist_eff = mean(param$Value[param$Parameter=="dist_eff"],na.rm=T),
  # hand washing
  hand_eff = mean(param$Value[param$Parameter=="hand_eff"],na.rm=T),
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
parameters["pdeath_hc"]<-parameters["pdeath_hc"]/100
parameters["pdeath_icu"]<-parameters["pdeath_icu"]/100
parameters["pdeath_icuc"]<-parameters["pdeath_icuc"]/100
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
parameters_noise<-c(1:5,19:26,32:39,43,45,47:49)
iterations<-parameters["iterations"]
noise<-parameters["noise"]
confidence<-parameters["confidence"]/100
parameters["mass_test_sens"]<-parameters["mass_test_sens"]/100
age_testing_min<-(parameters["age_testing_min"])
age_testing_max<-(parameters["age_testing_max"])
parameters["isolation_days"]<-parameters["isolation_days"]

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
  inp$`Date End` = pmin(stopdate, inp$`Date End`)
  
  tv<-which(inp$`Apply to`==run)
  
  si<-intersect(which(inp$Intervention=="Self-isolation if Symptomatic"),tv)
  scr<-intersect(which(inp$Intervention=="Screening (when S.I.)"),tv)
  sd<-intersect(which(inp$Intervention=="Social Distancing"),tv)
  hw<-intersect(which(inp$Intervention=="Handwashing"),tv)
  wah<-intersect(which(inp$Intervention=="Working at Home"),tv)
  sc<-intersect(which(inp$Intervention=="School Closures"),tv)
  cte<-intersect(which(inp$Intervention=="Shielding the Elderly"),tv)
  q<-intersect(which(inp$Intervention=="Household Isolation (when S.I.)"),tv)
  tb<-intersect(which(inp$Intervention=="International Travel Ban"),tv)
  mt<-intersect(which(inp$Intervention=="Mass Testing"),tv)
  minas<-intersect(which(inp$Intervention=="Age Testing Minimum"),tv)
  maxas<-intersect(which(inp$Intervention=="Age Testing Maximum"),tv)
  vc<-intersect(which(inp$Intervention=="Vaccination"),tv)
  minav<-intersect(which(inp$Intervention=="Age Vaccine Minimum"),tv)
  
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
  return(list(si_vector=si_vector,sd_vector=sd_vector,scr_vector=scr_vector,hw_vector=hw_vector,wah_vector=wah_vector,
              sc_vector=sc_vector,tb_vector=tb_vector,mt_vector=mt_vector,cte_vector=cte_vector,q_vector=q_vector,vc_vector=vc_vector,isolation=isolation,
              screen=screen,cocoon=cocoon,schoolclose=schoolclose,workhome=workhome,handwash=handwash,
              quarantine=quarantine,vaccine=vaccine,travelban=travelban,distancing=distancing,masstesting=masstesting,
              maxas_vector=maxas_vector,minas_vector=minas_vector,minav_vector=minav_vector))
}
vectors<-inputs(inp,'Hypothetical Scenario')
vectors0<-inputs(inp,'Baseline (Calibration)')


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
         workhome<-input$workhome[t*20+1]
         schoolclose<-input$schoolclose[t*20+1]
         cocoon<-input$cocoon[t*20+1]
         vaccine<-input$vaccine[t*20+1]
         travelban<-input$travelban[t*20+1]
         screen<-input$screen[t*20+1]
         quarantine<-input$quarantine[t*20+1]
         masstesting<-input$masstesting[t*20+1]
         
         screen_eff<-0
         selfis<-0
         school<-1
         dist<-1
         hand<-0
         vaccinate<-0
         trvban_eff<-0
         quarantine_rate<-0
         tests_per_day<-0
         
         selfis_cov<-(input$si_vector[t*20+1])/100
         screen_contacts<-(input$scr_vector[t*20+1])/10
         school_eff<-(input$sc_vector[t*20+1])/100
         dist_cov<-(input$sd_vector[t*20+1])/100
         hand_cov<-(input$hw_vector[t*20+1])/100
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
         age_vaccine_vector<-c(rep(0,min_age_vaccine-1),rep(1,21-min_age_vaccine))
         
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
         if(vaccine){
           vac_rate <- (-log(1-vaccine_cov)/vac_campaign)
           vaccinate <- vac_rate
         }
         if(travelban){
           trvban_eff<-travelban_eff
         }
         if(quarantine){
           rate_q<-min((min(sum((I+CL+H+ICU+Vent+HC+ICUC+ICUCV+VentC))*(household_size-1)/sum(P),1)*quarantine_effort),quarantine_cov/2)
           quarantine_rate<-rate_q/(1+exp(-10*(quarantine_cov/2-Q)))
         }
         
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
         lam <- (1-hand)*p*seas*(contacts%*%((rho*E+(I+CL+importation)+(1-selfis_eff)*(X+HHC)+rhos*(HH))/P))+
           (1-hand)*p*seas*(1-quarantine*quarantine_eff_other)*(contact_other%*%((rho*QE+QI+QC)/P))
         # contacts under home quarantine
         lamq<-(1-hand)*p*seas*((1-quarantine_eff_home)*contact_home%*%(((1-selfis_eff)*(X+HHC+rho*QE+QI+QC))/P))+
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
         dRdt <- nui*I-omega*R+nui*X+nui*CL+ageing%*%R-mort*R + (1/isolation_days)*Z+(1/quarantine_days)*QR + nus*(1-pdeath_h*ifr[,2])*H + (1-pdeath_icu*ifr[,2])*nu_icu*ICU + (1-pdeath_icuc*ifr[,2])*nu_icuc*ICUC + (1-pdeath_ventc*ifr[,2])*nu_ventc*ICUCV + (1-pdeath_hc*ifr[,2])*nusc*HC + (1-pdeath_vent*ifr[,2])*nu_vent*Vent+ (1-pdeath_ventc*ifr[,2])*nu_ventc*VentC
         dXdt <- gamma*selfis*(1-age_testing_vector*ratetestE)*pclin*(1-ihr[,2])*E+gamma*(1-pclin)*(1-age_testing_vector*ratetestE)*screen_eff*(1-ihr[,2])*E-nui*X+ageing%*%X-mort*X 
         dVdt <- vaccinate*age_vaccine_vector*S -(1-vaccine_eff)*lam*V +ageing%*%V - mort*V
         
         dQSdt <- quarantine_rate*S+ ageing%*%QS-mort*QS - (1/quarantine_days)*QS - lamq*QS
         dQEdt <- quarantine_rate*E - gamma*QE + ageing%*%QE-mort*QE - (1/quarantine_days)*QE + lamq*QS 
         dQIdt <- quarantine_rate*I + gamma*(1-ihr[,2])*(1-pclin)*QE-nui*QI+ageing%*%QI-mort*QI - (1/quarantine_days)*QI
         dQCdt <- gamma*pclin*(1-selfis)*(1-age_testing_vector*ratetestE)*(1-ihr[,2])*quarantine_rate*E+gamma*(1-ihr[,2])*pclin*QE-nui*QC+ageing%*%QC-mort*QC - (1/quarantine_days)*QC
         dQRdt <- nui*QI+nui*QC+ageing%*%QR-mort*QR - (1/quarantine_days)*QR
         
         dHdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*reporth*E + gamma*ihr[,2]*(1-prob_icu)*(1-critH)*QE - nus*H + ageing%*%H-mort*H  
         dHCdt <- gamma*ihr[,2]*(1-prob_icu)*(1-critH)*(1-reporth)*E+gamma*ihr[,2]*(1-prob_icu)*critH*E + gamma*ihr[,2]*(1-prob_icu)*critH*QE - nusc*HC + ageing%*%HC-mort*HC 
         dICUdt <- gamma*ihr[,2]*prob_icu*(1-crit)*(1-prob_vent)*E + gamma*ihr[,2]*prob_icu*(1-crit)*(1-prob_vent)*QE - nu_icu*ICU +ageing%*%ICU - mort*ICU +(1-crit)*ICUC*1/2
         dICUCdt <- gamma*ihr[,2]*prob_icu*crit*(1-prob_vent)*E + gamma*ihr[,2]*prob_icu*crit*(1-prob_vent)*QE - 
           nu_icuc*ICUC -(1-crit)*ICUC*1/2 +ageing%*%ICUC - mort*ICUC 
         dICUCVdt <- gamma*ihr[,2]*prob_icu*prob_vent*crit*E +gamma*ihr[,2]*prob_icu*prob_vent*crit*QE -nu_ventc*ICUCV +ageing%*%ICUCV - mort*ICUCV - (1-critV)*ICUCV*1/2
         dVentdt <- gamma*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_vent*E + gamma*ihr[,2]*prob_icu*(1-crit)*(1-critV)*prob_vent*QE +(1-critV)*VentC*1/2 +(1-critV)*ICUCV*1/2 -nu_vent*Vent +ageing%*%Vent - mort*Vent 
         dVentCdt <- gamma*ihr[,2]*prob_icu*prob_vent*(1-crit)*critV*E +gamma*ihr[,2]*prob_icu*prob_vent*(1-crit)*critV*QE - 
           (1-critV)*VentC*1/2 -nu_ventc*VentC +ageing%*%VentC - mort*VentC 
         
         dCdt <- report*gamma*(1-age_testing_vector*ratetestE)*(1-pclin)*(1-ihr[,2])*(E+QE)+reportc*gamma*pclin*(1-age_testing_vector*ratetestE)*(1-ihr[,2])*(E+QE)+
           gamma*ihr[,2]*(1-critH)*(1-prob_icu)*(E+QE)+gamma*ihr[,2]*critH*reporth*(1-prob_icu)*(E+QE)+
           gamma*ihr[,2]*prob_icu*(E+QE)+ratetestI*age_testing_vector*I+ratetestC*age_testing_vector*CL+gamma*age_testing_vector*ratetestE*(1-ihr[,2])*E
         dCMdt<- nus*pdeath_h*ifr[,2]*H + nusc*pdeath_hc*ifr[,2]*HC + nu_icu*pdeath_icu*ifr[,2]*ICU +nu_icuc*pdeath_icuc*ifr[,2]*ICUC +nu_vent*pdeath_vent*ifr[,2]*Vent +nu_ventc*pdeath_ventc*ifr[,2]*VentC +nu_ventc*pdeath_ventc*ifr[,2]*ICUCV+ 
           mort*H + mort*HC + mort*ICU + mort*ICUC + mort*ICUCV + mort*Vent + mort*VentC  + mort*Z
         dCMCdt <- nusc*pdeath_hc*ifr[,2]*HC+nu_icuc*pdeath_icuc*ifr[,2]*ICUC + nu_ventc*pdeath_ventc*ifr[,2]*VentC + nu_ventc*pdeath_ventc*ifr[,2]*ICUCV+
           mort*HC + mort*ICUC + mort*VentC + mort*ICUCV 
         
         dZdt <- gamma*ratetestE*age_testing_vector*(1-ihr[,2])*E+ratetestI*age_testing_vector*I+ratetestC*age_testing_vector*CL-(1/isolation_days)*Z-mort*Z
         
         # return the rate of change
         list(c(dSdt,dEdt,dIdt,dRdt,dXdt,dHdt,dHCdt,dCdt,dCMdt,dVdt,dQSdt,dQEdt,dQIdt,dQRdt,dCLdt,dQCdt,dICUdt,dICUCdt,dICUCVdt,dVentdt,dVentCdt,dCMCdt,dZdt))
       }
  ) 
}

###########    RUN BASELINE MODEL - start time for interventions is set to day 1e5, i.e. interventions are always off

Y<-c(initS,initE,initI,initR,initX,initH,initHC,initC,initCM,initV, initQS, initQE, initQI, initQR, initCL, initQC, initICU, initICUC, initICUCV, initVent, initVentC, initCMC, initZ) # initial conditions for the main solution vector


process_ode_outcome <- function(out, iterations){
  out_min<-out$min
  out_max<-out$max
  out_mean<-out$mean

  critH<-c()
  crit<-c()
  critV<-c()
  
  for (i in 1:length(times)){
    critH[i]<-min(1-fH((sum(out_mean[i,(Hindex+1)]))+sum(out_mean[i,(ICUCindex+1)])+sum(out_mean[i,(ICUCVindex+1)])),1)
    crit[i]<-min(1-fICU((sum(out_mean[i,(ICUindex+1)]))+(sum(out_mean[i,(Ventindex+1)]))+(sum(out_mean[i,(VentCindex+1)]))))
    critV[i]<-min(1-fVent((sum(out_mean[i,(Ventindex+1)]))),1)
  }
  
  # total population
  pop1<-out_mean[,(Sindex+1)]+out_mean[,(Eindex+1)]+out_mean[,(Iindex+1)]+out_mean[,(CLindex+1)]+out_mean[,(Rindex+1)]+out_mean[,(Xindex+1)]+out_mean[,(Vindex+1)]+
    out_mean[,(Zindex+1)]+out_mean[,(QSindex+1)]+out_mean[,(QEindex+1)]+out_mean[,(QIindex+1)]+out_mean[,(QCindex+1)]+out_mean[,(QRindex+1)]+
    out_mean[,(Hindex+1)]+out_mean[,(HCindex+1)]+out_mean[,(ICUindex+1)]+out_mean[,(ICUCindex+1)]+out_mean[,(ICUCVindex+1)]+out_mean[,(Ventindex+1)]+out_mean[,(VentCindex+1)]
  tpop1<-rowSums(pop1)
  time<-as.Date(out_mean[,1]+startdate)

  dailyinc1<-out$mean_cases      # daily incidence
  cuminc1<-out$mean_cum_cases       # cumulative incidence
  previcureq1<-rowSums(out_mean[,(Hindex+1)])+ rowSums(out_mean[,(ICUCindex+1)])+rowSums(out_mean[,(ICUCVindex+1)]) # surge beds occupancy
  previcureq21<-rowSums(out_mean[,(ICUindex+1)])+rowSums(out_mean[,(VentCindex+1)])   # icu beds occupancy
  previcureq31<-rowSums(out_mean[,(Ventindex+1)])   # ventilator occupancy
  cmortality1<-rowSums(out_mean[,(CMindex+1)])      # cumulative mortality
  overloadH1<-rowSums(out_mean[,(HCindex+1)])       # requirement for beds
  overloadICU1<-rowSums(out_mean[,(ICUCindex+1)])   # requirement for icu beds
  overloadICUV1<-rowSums(out_mean[,(ICUCVindex+1)]) # requirement for ventilators
  overloadVent1<-rowSums(out_mean[,(VentCindex+1)]) # requirement for ventilators
  ccases1<-rowSums(out_mean[,(Cindex+1)])           # cumulative cases
  reqsurge1<-rowSums(out_mean[,(Hindex+1)])+overloadH1
  reqicu1<-rowSums(out_mean[,(ICUindex+1)])+overloadICU1
  reqvent1<-rowSums(out_mean[,(Ventindex+1)])+overloadICUV1+overloadVent1
  

  ##########################    CALCULATE MORTALITY 
  pdeath_hc<-parameters["pdeath_hc"]
  prob_icu<-parameters["prob_icu"]
  prob_vent<-parameters["prob_vent"]
  pdeath_icuc<-parameters["pdeath_icuc"]
  pdeath_ventc<-parameters["pdeath_ventc"]
  
  cinc_mort_H1 <- cumsum(rowSums(parameters["nus"]*parameters["pdeath_h"]*(out_mean[,(Hindex+1)]%*%ifr[,2])))
  cinc_mort_HC1 <- cumsum(rowSums(parameters["nusc"]*parameters["pdeath_hc"]*(out_mean[,(HCindex+1)]%*%ifr[,2])))
  cinc_mort_ICU1 <- cumsum(rowSums(parameters["nu_icu"]*parameters["pdeath_icu"]*out_mean[,(ICUindex+1)]%*%ifr[,2]))
  cinc_mort_ICUC1 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["pdeath_icuc"]*out_mean[,(ICUCindex+1)]%*%ifr[,2]))
  cinc_mort_ICUCV1 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out_mean[,(ICUCVindex+1)]%*%ifr[,2]))
  cinc_mort_Vent1 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*out_mean[,(Ventindex+1)]%*%ifr[,2]))
  cinc_mort_VentC1 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*out_mean[,(VentCindex+1)]%*%ifr[,2]))
  base_mort_H1 <- cumsum(rowSums(out_mean[,(Hindex+1)]%*%mort))
  base_mort_HC1 <- cumsum(rowSums(out_mean[,(HCindex+1)]%*%mort))
  base_mort_ICU1 <- cumsum(rowSums(out_mean[,(ICUindex+1)]%*%mort))
  base_mort_ICUC1 <- cumsum(rowSums(out_mean[,(ICUCindex+1)]%*%mort))
  base_mort_ICUCV1 <- cumsum(rowSums(out_mean[,(ICUCVindex+1)]%*%mort))
  base_mort_Vent1 <- cumsum(rowSums(out_mean[,(Ventindex+1)]%*%mort))
  base_mort_VentC1 <- cumsum(rowSums(out_mean[,(VentCindex+1)]%*%mort))
  base_mort_S1 <- cumsum(rowSums(out_mean[,(Sindex+1)]%*%mort))
  base_mort_E1 <- cumsum(rowSums(out_mean[,(Eindex+1)]%*%mort))
  base_mort_I1 <- cumsum(rowSums(out_mean[,(Iindex+1)]%*%mort))
  base_mort_CL1 <- cumsum(rowSums(out_mean[,(CLindex+1)]%*%mort))
  base_mort_X1 <- cumsum(rowSums(out_mean[,(Xindex+1)]%*%mort))
  base_mort_QS1 <- cumsum(rowSums(out_mean[,(QSindex+1)]%*%mort))
  base_mort_QE1 <- cumsum(rowSums(out_mean[,(QEindex+1)]%*%mort))
  base_mort_QI1 <- cumsum(rowSums(out_mean[,(QIindex+1)]%*%mort))
  base_mort_QC1 <- cumsum(rowSums(out_mean[,(QCindex+1)]%*%mort))
  base_mort_QR1 <- cumsum(rowSums(out_mean[,(QRindex+1)]%*%mort))
  base_mort_R1 <- cumsum(rowSums(out_mean[,(Rindex+1)]%*%mort))
  

  # Export in a cohesive format ----
  results <- list()
  results$time <- startdate + times  # dates
  results$Rt <- out$mean_Rt
  results$cum_mortality <- round(cmortality1)  # cumulative mortality
  results$pct_total_pop_infected <- out$mean_infections
  results$doubling_time <- round(log(2)*7 / (log(dailyinc1[2+7] / dailyinc1[2])), 2)  # (Baseline only) to double the number of infections at inception
  results$required_beds <- round(previcureq1)  # required beds
  results$saturation <- parameters["beds_available"]  # saturation
  results$daily_incidence <- round(dailyinc1)  # daily incidence (Reported)
  results$daily_total_cases <- round(out$mean_daily_infection) # daily incidence (Reported + Unreported)  # daily incidence (Reported + Unreported)
  results$hospital_surge_beds <- round(previcureq1)
  results$icu_beds <- round(previcureq21)
  results$ventilators <- round(previcureq31)
  results$normal_bed_requirement <- round(reqsurge1) #real required beds. previcureq1 above is the occupancy
  results$icu_bed_requirement <- round(reqicu1)
  results$icu_ventilator_requirement <- round(reqvent1)
  
  results$death_natural_non_exposed <- round(base_mort_S1)
  results$death_natural_exposed <- round(base_mort_E1 + base_mort_I1 + base_mort_CL1 + base_mort_X1 + base_mort_QS1 + 
                                           base_mort_QE1 + base_mort_QI1 + base_mort_QC1 + base_mort_QR1 + base_mort_R1+
                                           base_mort_H1+base_mort_HC1+base_mort_ICU1+base_mort_ICUC1+base_mort_ICUCV1+
                                           base_mort_Vent1+base_mort_VentC1)
  results$death_treated_hospital <- round(cinc_mort_H1)
  results$death_treated_icu <- round(cinc_mort_ICU1)
  results$death_treated_ventilator <- round(cinc_mort_Vent1)
  results$death_untreated_hospital <- round(cinc_mort_HC1)
  results$death_untreated_icu <- round(cinc_mort_ICUC1)
  results$death_untreated_ventilator <- round(cinc_mort_VentC1)+round(cinc_mort_ICUCV1)
  results$attributable_deaths <- results$death_treated_hospital + results$death_treated_icu + results$death_treated_ventilator +
    results$death_untreated_hospital + results$death_untreated_icu + results$death_untreated_ventilator
  results$attributable_deaths_end <- last(results$attributable_deaths)
  results$total_deaths <- results$attributable_deaths + results$death_natural_non_exposed + results$death_natural_exposed
  results$total_deaths_end <- last(results$total_deaths)
  results$total_reported_deaths_end <- last(results$cum_mortality)
  results$base_mort_H <- base_mort_H1
  results$base_mort_HC <- base_mort_HC1
  results$base_mort_ICU <- base_mort_ICU1
  results$base_mort_ICUC <- base_mort_ICUC1
  results$base_mort_ICUCV <- base_mort_ICUCV1
  results$base_mort_Vent <- base_mort_Vent1
  results$base_mort_VentC <- base_mort_VentC1
  results$base_mort_S <- base_mort_S1
  results$base_mort_E <- base_mort_E1
  results$base_mort_I <- base_mort_I1
  results$base_mort_CL <- base_mort_CL1
  results$base_mort_X <- base_mort_X1
  results$base_mort_QS <- base_mort_QS1
  results$base_mort_QE <- base_mort_QE1
  results$base_mort_QI <- base_mort_QI1
  results$base_mort_QC <- base_mort_QC1
  results$base_mort_QR <- base_mort_QR1
  results$base_mort_R <- base_mort_R1
  
  ## AGE DEPENDENT MORTALITY
  cinc_mort_H1 <- parameters["nus"]*parameters["pdeath_h"]*(out_mean[,(Hindex+1)])
  cinc_mort_HC1 <- parameters["nusc"]*parameters["pdeath_hc"]*(out_mean[,(HCindex+1)])
  cinc_mort_ICU1 <- parameters["nu_icu"]*parameters["pdeath_icu"]*out_mean[,(ICUindex+1)]
  cinc_mort_ICUC1 <- parameters["nu_icuc"]*parameters["pdeath_icuc"]*out_mean[,(ICUCindex+1)] 
  cinc_mort_ICUCV1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out_mean[,(ICUCVindex+1)]
  cinc_mort_Vent1 <- parameters["nu_vent"]*parameters["pdeath_vent"]*out_mean[,(Ventindex+1)] 
  cinc_mort_VentC1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out_mean[,(VentCindex+1)] 
  totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+cinc_mort_ICUCV1+cinc_mort_Vent1+cinc_mort_VentC1)
  basemort_H1<-(out_mean[,(Hindex+1)])
  basemort_HC1<-(out_mean[,(HCindex+1)])
  basemort_ICU1<-(out_mean[,(ICUindex+1)])
  basemort_ICUC1<-(out_mean[,(ICUCindex+1)])
  basemort_ICUCV1<-(out_mean[,(ICUCVindex+1)])
  basemort_Vent1<-(out_mean[,(Ventindex+1)])
  basemort_VentC1<-(out_mean[,(VentCindex+1)])
  totbase1<-as.data.frame(basemort_H1+basemort_HC1+basemort_ICU1+basemort_ICUC1+basemort_ICUCV1+basemort_Vent1+basemort_VentC1)
  tc<-c()
  
  for (i in 1:dim(cinc_mort_H1)[1]) {
    for (j in 1:dim(cinc_mort_H1)[2]) {
      tc<-rbind(tc,c(i, j, totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j])) 
    }
  }
  tc<-as.data.frame(tc)
  colnames(tc)<-c("Day","Age","value")
  
  results$tc <- tc %>%
    mutate(Date = startdate + Day,
           age_cat = case_when(
             Age >=  1 & Age <= 6   ~ "≤ 30 y.o.",
             Age >  6 & Age <= 8    ~ "30-40 y.o.",
             Age >  8 & Age <= 10    ~ "40-50 y.o.",
             Age >  10 & Age <= 12    ~ "50-60 y.o.",
             Age >  12 & Age <= 14    ~ "60-70 y.o.",
             Age >=  15  ~ "≥ 70 y.o.")) %>%
    mutate(age_cat = factor(age_cat, levels = rev(c("≤ 30 y.o.", "30-40 y.o.",
                                                    "40-50 y.o.", "50-60 y.o.", "60-70 y.o.", "≥ 70 y.o."))))
  
  mortality_lag <- data.frame(Age = popstruc$agefloor)
  if(nrow(out_mean) >= 30)  mortality_lag <- bind_cols(mortality_lag, 
                                                  data.frame(day30 = out_mean[30,CMindex+1]/out_mean[30,Cindex+1]) %>%
                                                    mutate(day30 = ifelse(is.infinite(day30), 0, day30)) %>%
                                                    rename(`Day 30` = day30))
  if(nrow(out_mean) >= 60)  mortality_lag <- bind_cols(mortality_lag, 
                                                  data.frame(day60 = out_mean[60,CMindex+1]/out_mean[60,Cindex+1]) %>%
                                                    mutate(day60 = ifelse(is.infinite(day60), 0, day60)) %>%
                                                    rename(`Day 60` = day60))
  if(nrow(out_mean) >= 90)  mortality_lag <- bind_cols(mortality_lag, 
                                                  data.frame(day90 = out_mean[90,CMindex+1]/out_mean[90,Cindex+1]) %>%
                                                    mutate(day90 = ifelse(is.infinite(day90), 0, day90)) %>%
                                                    rename(`Day 90` = day90))
  if(nrow(out_mean) >= 120)  mortality_lag <- bind_cols(mortality_lag, 
                                                   data.frame(day120 = out_mean[120,CMindex+1]/out_mean[120,Cindex+1]) %>%
                                                     mutate(day120 = ifelse(is.infinite(day120), 0, day120)) %>%
                                                     rename(`Day 120` = day120))
  
  results$mortality_lag <- mortality_lag
  
  
  if(iterations>1){

    previcureq1_max<-rowSums(out_max[,(Hindex+1)])+ rowSums(out_max[,(ICUCindex+1)])+rowSums(out_max[,(ICUCVindex+1)]) # surge beds occupancy
    previcureq21_max<-rowSums(out_max[,(ICUindex+1)])+rowSums(out_max[,(VentCindex+1)])   # icu beds occupancy
    previcureq31_max<-rowSums(out_max[,(Ventindex+1)])   # ventilator occupancy
    cmortality1_max<-rowSums(out_max[,(CMindex+1)])      # cumulative mortality
    overloadH1_max<-rowSums(out_max[,(HCindex+1)])       # requirement for beds
    overloadICU1_max<-rowSums(out_max[,(ICUCindex+1)])   # requirement for icu beds
    overloadICUV1_max<-rowSums(out_max[,(ICUCVindex+1)]) # requirement for ventilators
    overloadVent1_max<-rowSums(out_max[,(VentCindex+1)]) # requirement for ventilators
    ccases1_max<-rowSums(out_max[,(Cindex+1)])           # cumulative cases
    reqsurge1_max<-rowSums(out_max[,(Hindex+1)])+overloadH1  # surge beds total requirements
    reqicu1_max<-rowSums(out_max[,(ICUindex+1)])+overloadICU1 # ICU beds total requirements
    reqvent1_max<-rowSums(out_max[,(Ventindex+1)])+overloadICUV1+overloadVent1 # ventilator beds total requirements
    
    previcureq1_min<-rowSums(out_min[,(Hindex+1)])+ rowSums(out_min[,(ICUCindex+1)])+rowSums(out_min[,(ICUCVindex+1)]) # surge beds occupancy
    previcureq21_min<-rowSums(out_min[,(ICUindex+1)])+rowSums(out_min[,(VentCindex+1)])   # icu beds occupancy
    previcureq31_min<-rowSums(out_min[,(Ventindex+1)])   # ventilator occupancy
    cmortality1_min<-rowSums(out_min[,(CMindex+1)])      # cumulative mortality
    overloadH1_min<-rowSums(out_min[,(HCindex+1)])       # requirement for beds
    overloadICU1_min<-rowSums(out_min[,(ICUCindex+1)])   # requirement for icu beds
    overloadICUV1_min<-rowSums(out_min[,(ICUCVindex+1)]) # requirement for ventilators
    overloadVent1_min<-rowSums(out_min[,(VentCindex+1)]) # requirement for ventilators
    ccases1_min<-rowSums(out_min[,(Cindex+1)])           # cumulative cases
    reqsurge1_min<-rowSums(out_min[,(Hindex+1)])+overloadH1  # surge beds total requirements
    reqicu1_min<-rowSums(out_min[,(ICUindex+1)])+overloadICU1 # ICU beds total requirements
    reqvent1_min<-rowSums(out_min[,(Ventindex+1)])+overloadICUV1+overloadVent1 # ventilator beds total requirements
    
    results$Rt_max <- out$max_Rt
    results$Rt_min <- out$min_Rt
    
    results$daily_incidence_max <- out$max_cases
    results$daily_incidence_min <- out$min_cases  
    
    results$daily_total_cases_max <- out$max_daily_infection
    results$daily_total_cases_min <- out$min_daily_infection
    
    results$total_reported_deaths_end_min <- last(cmortality1_min)
    results$total_reported_deaths_end_max <- last(cmortality1_max)
    
    results$pct_total_pop_infected_min <- out$min_infections  # proportion of the  population that has been infected at the end of the simulation
    results$pct_total_pop_infected_max <- out$max_infections  # proportion of the  population that has been infected at the end of the simulation
  }
  return(results)
}

# covidOdeCpp_reset()
# out <- ode(y = Y, times = times, func = covidOdeCpp, parms = parameters,
#            input=vectors, A=A,
#            contact_home=contact_home, contact_school=contact_school,
#            contact_work=contact_work, contact_other=contact_other,
#            popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
#            ageing=ageing,
#            ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)

multi_runs<-function(Y,times,parameters,input,iterations,noise,confidence){
  
  results <- list()
  aux<-array(0, dim=c(length(times),23*A+1,iterations))
  results$mean<-matrix(0,nrow = length(times),ncol = 23*A+1)
  results$min<-matrix(0,nrow = length(times),ncol = 23*A+1)
  results$max<-matrix(0,nrow = length(times),ncol = 23*A+1)
  results$mean_cases<-matrix(0,nrow = length(times),ncol = 23*A+1)
  results$min_cases<-matrix(0,nrow = length(times),ncol = 23*A+1)
  results$max_cases<-matrix(0,nrow = length(times),ncol = 23*A+1)
  results$mean_cum_cases<-matrix(0,nrow = length(times),ncol = 1)
  results$min_cum_cases<-matrix(0,nrow = length(times),ncol = 1)
  results$max_cum_cases<-matrix(0,nrow = length(times),ncol = 1)
  results$mean_daily_infection<-matrix(0,nrow = length(times),ncol = 1)
  results$min_daily_infection<-matrix(0,nrow = length(times),ncol = 1)
  results$max_daily_infection<-matrix(0,nrow = length(times),ncol = 1)
  cases<-matrix(0, nrow=length(times),ncol=iterations)
  cum_cases<-matrix(0, nrow=length(times),ncol=iterations)
  day_infections<-matrix(0, nrow=length(times),ncol=iterations)
  Rt_aux<-matrix(0, nrow=length(times),ncol=iterations)
  infections<-matrix(0, nrow=iterations,ncol=1)
  Rt <- NULL

  param_vector<-parameters
  if(iterations>1){
    for (i in 1:iterations){
      param_vector[parameters_noise]<-parameters[parameters_noise]+rnorm(length(parameters_noise),mean=0,sd=noise*abs(parameters[parameters_noise]))
      out0 <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = param_vector, input=vectors0)
      aux[,,i]<-out0

      critH<-c()
      crit<-c()
      critV<-c()
      for (ii in 1:length(times)){
        critH[ii]<-min(1-fH((sum(out0[ii,(Hindex+1)]))+sum(out0[ii,(ICUCindex+1)])+sum(out0[ii,(ICUCVindex+1)])),1)
        crit[ii]<-min(1-fICU((sum(out0[ii,(ICUindex+1)]))+(sum(out0[ii,(Ventindex+1)]))+(sum(out0[ii,(VentCindex+1)]))))
        critV[ii]<-min(1-fVent((sum(out0[ii,(Ventindex+1)]))),1)
      }
      
      # daily incidence
      incidence<-param_vector["report"]*param_vector["gamma"]*(1-param_vector["pclin"])*out0[,(Eindex+1)]%*%(1-ihr[,2])+
        param_vector["reportc"]*param_vector["gamma"]*param_vector["pclin"]*out0[,(Eindex+1)]%*%(1-ihr[,2])+
        param_vector["report"]*param_vector["gamma"]*(1-param_vector["pclin"])*out0[,(QEindex+1)]%*%(1-ihr[,2])+
        param_vector["reportc"]*param_vector["gamma"]*param_vector["pclin"]*out0[,(QEindex+1)]%*%(1-ihr[,2])
      
      incidenceh<- param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu"])*param_vector["reporth"]+
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu"])*(1-param_vector["reporth"])+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*(1-critH)*(1-param_vector["prob_icu"])+
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*critH*param_vector["reporth"]*(1-param_vector["prob_icu"])+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*critH*param_vector["reporth"]*(1-param_vector["prob_icu"])+
        param_vector["gamma"]*out0[,(Eindex+1)]%*%ihr[,2]*param_vector["prob_icu"]+
        param_vector["gamma"]*out0[,(QEindex+1)]%*%ihr[,2]*param_vector["prob_icu"]

      cases[,i]<-(rowSums(incidence)+rowSums(incidenceh))           # daily incidence cases
      cum_cases[,i]<-colSums(incidence)+colSums(incidenceh)         # cumulative incidence cases
      day_infections[,i]<- round(rowSums(param_vector["gamma"]*out0[,(Eindex+1)]+param_vector["gamma"]*out0[,(QEindex+1)]))
      
      # daily infections
      infections[i] <- round(100*tail(cumsum(rowSums(param_vector["gamma"]*out0[,(Eindex+1)])),1)/sum(popstruc[,2]), 1)  # proportion of the  population that has been infected at the end of the simulation
      for (w in (ceiling(1/param_vector["nui"])+1):length(times)){
        Rt_aux[w,i]<-cumsum(sum(param_vector["gamma"]*out0[w,(Eindex+1)]))/cumsum(sum(param_vector["gamma"]*out0[(w-1/param_vector["nui"]),(Eindex+1)]))
        if(Rt_aux[w,i] >= 7) {Rt_aux[w,i]  <- NA}
      }
    } 
    qq <- quantile(infections, c(confidence, 0.5, (1-confidence)))
    results$mean_infections<-qq[2]
    results$min_infections<-qq[1]
    results$max_infections<-qq[3]
    
    for(i in 1:length(out0[,1])){
      qq <- quantile(cases[i,], c(confidence, 0.5, (1-confidence)))
      results$mean_cases[i]<-qq[2]
      results$min_cases[i]<-qq[1]
      results$max_cases[i]<-qq[3]
      
      qq <- quantile(cum_cases[i,], c(confidence, 0.5, (1-confidence)))
      results$mean_cum_cases[i]<-qq[2]
      results$min_cum_cases[i]<-qq[1]
      results$max_cum_cases[i]<-qq[3]
      
      qq <- quantile(day_infections[i,], c(confidence, 0.5, (1-confidence)))
      results$mean_daily_infection[i]<-qq[2]
      results$min_daily_infection[i]<-qq[1]
      results$max_daily_infection[i]<-qq[3]
      
      qq <- quantile(Rt_aux[i,], c(confidence, 0.5, (1-confidence)),na.rm = T)
      results$mean_Rt[i]<-qq[2]
      results$min_Rt[i]<-qq[1]
      results$max_Rt[i]<-qq[3]
      
      for (j in 1:length(out0[1,])){
        qq <- quantile(aux[i,j,], c(confidence, 0.5, (1-confidence)))
        results$mean[i,j]<-qq[2]
        results$min[i,j]<-qq[1]
        results$max[i,j]<-qq[3]
      }
    }
  }else{
    results$mean <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters, input=vectors0)
  }
  return(results)
}
out0 <-multi_runs(Y, times, parameters, vectors0, iterations, noise, confidence)
out0$min_infections
out0$max_infections

plot(times,rowSums(out0$mean[,Iindex+1]),type = 'l')
polygon(c(times, rev(times)), c(rowSums(out0$max[,Iindex+1]), rev(rowSums(out0$min[,Iindex+1]))),
        col=rgb(0, 0, 0,0.25), border = NA)

plot(times,out0$mean_Rt,type = 'l')
polygon(c(times, rev(times)), c(out0$max_Rt, rev(out0$min_Rt)),
        col=rgb(0, 0, 0,0.25), border = NA)


# out0 <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters, input=vectors0)
# out <- ode(y = Y, times = times, method = "euler", hini = 0.05, func = covid, parms = parameters, input=vectors)

simul_baseline <- process_ode_outcome(out0,iterations)
# # write.csv(simul_baseline, paste0(hilo,"_baseline_",gsub(":|-","",Sys.time()),".csv"))
# 
#future interventions
#extend travel ban, quarantine, hand washing, cocooning the elderly until 1st July
out <-multi_runs(Y, times, parameters, vectors, iterations, noise, confidence)
simul_interventions <- process_ode_outcome(out,iterations)
# write.csv(simul_interventions, paste0(hilo,"_futureIntv_",gsub(":|-","",Sys.time()),".csv"))

pop1<-out$mean[,(Sindex+1)]+out$mean[,(Eindex+1)]+out$mean[,(Iindex+1)]+out$mean[,(CLindex+1)]+out$mean[,(Rindex+1)]+out$mean[,(Xindex+1)]+out$mean[,(Vindex+1)]+
  out$mean[,(Zindex+1)]+out$mean[,(QSindex+1)]+out$mean[,(QEindex+1)]+out$mean[,(QIindex+1)]+out$mean[,(QCindex+1)]+out$mean[,(QRindex+1)]+
  out$mean[,(Hindex+1)]+out$mean[,(HCindex+1)]+out$mean[,(ICUindex+1)]+out$mean[,(ICUCindex+1)]+out$mean[,(ICUCVindex+1)]+out$mean[,(Ventindex+1)]+out$mean[,(VentCindex+1)]
tpop1<-rowSums(pop1)

#############    PLOTTING
# Fitting tab
# fitting the intervention lines to the data to account for any historical interventions
time<-as.Date(out0$mean[,1]+startdate)
par(mfrow=c(1,2))
# set up the axis limits
xmin<-min(as.Date(cases_rv[,1]))
xmax<-max(as.Date(cases_rv[,1]))
ymax<-max(cases_rv[,2],na.rm = T)
xtick<-seq(xmin, xmax, by=7)
plot(time,simul_interventions$daily_incidence,type='l',lwd=3,
     main="New Reported Cases", xlab="Date", ylab="Cases per day",
     xlim=c(xmin,xmax),  ylim=c(0,ymax), col='blue',xaxt="n")
axis(side=1, labels = FALSE)
text(x=xtick,  y=-250, labels = format(xtick,"%b-%d"), srt = 0, xpd = TRUE)
points(as.Date(cases_rv[,1]),cases_rv[,2],pch=19,col='red')

# reset the maximum to the cumulative mortality
ymax<-max(cases_rv[,3],na.rm = T)
plot(time,simul_interventions$cum_mortality,type='l',lwd=3,
     main="Cumulative Mortality", xlab="Date", ylab="Total deaths",
     xlim=c(xmin,xmax), ylim=c(0,ymax), col='blue',xaxt="n")
text(x=xtick,  y=-100, labels = format(xtick,"%b-%d"), srt = 0, xpd = TRUE)
points(as.Date(cases_rv[,1]),cases_rv[,3],pch=19,col='red')


### Predictions tab
par(mfrow=c(1,2))
### Cases at baseline and intervention
ymax<-max(c(cases_rv[,2],simul_baseline$daily_incidence,simul_interventions$daily_incidence),na.rm=T)
plot(time,simul_baseline$daily_incidence,type='l',lwd=3,col='blue',
     main="Baseline", xlab="Date", ylab="New cases per day",ylim=c(0,ymax))
points(as.Date(cases_rv[,1]),cases_rv[,2],pch=19,col='red')
plot(time,simul_interventions$daily_incidence,type='l',lwd=3,col='blue',
     main="Intervention", xlab="Date", ylab="New cases per day",ylim=c(0,ymax))
points(as.Date(cases_rv[,1]),cases_rv[,2],pch=19,col='red')



# # # Hospital prevalences stratified by H,ICU and Vent
ymax<-max(c((simul_baseline$hospital_surge_beds+simul_baseline$icu_beds+simul_baseline$ventilators),(simul_interventions$hospital_surge_beds+simul_interventions$icu_beds+simul_interventions$ventilators)))
time<-as.Date(out0$mean[,1]+startdate)
coul=c("#047883", "#24A9E2","#051A46")
DM<-as.data.frame(cbind(time,simul_baseline$hospital_surge_beds,simul_baseline$icu_beds,simul_baseline$ventilators))
colnames(DM)<-c("Time","Hospital surge beds","ICU beds","Ventilators")
DM$Time<-as.Date(DM$Time,origin = "1970-01-01")
DMF<-melt(DM, id.vars="Time",measure.vars = c("Hospital surge beds","ICU beds","Ventilators"))
d0<-ggplot(DMF, aes(x = Time, y = value,fill=variable)) +
  geom_area()+
  scale_fill_manual(values=coul)

DM<-as.data.frame(cbind(time,simul_interventions$hospital_surge_beds,simul_interventions$icu_beds,simul_interventions$ventilators))
colnames(DM)<-c("Time","Hospital surge beds","ICU beds","Ventilators")
DM$Time<-as.Date(DM$Time, origin = "1970-01-01")
DMF<-melt(DM, id.vars="Time",measure.vars = c("Hospital surge beds","ICU beds","Ventilators"))
d1<-ggplot(DMF, aes(x = Time, y = value,fill=variable)) +
  geom_area()+
  scale_fill_manual(values=coul)

grid.arrange(d0+ylab("Number of Patients")+
               ggtitle("Baseline")+
               ylim(0, ymax)+
               geom_hline(yintercept=(parameters["beds_available"]+parameters["icu_beds_available"]+parameters["ventilators_available"]), linetype="dashed", color = "#047883")+
               geom_hline(yintercept=(parameters["icu_beds_available"]+parameters["ventilators_available"]), linetype="dashed", color = "#24A9E2")+
               geom_hline(yintercept=parameters["ventilators_available"], linetype="dashed", color = "#051A46")+
               theme_bw(),
             d1+ylab("Number of Patients")+
               ggtitle("Intervention")+
               ylim(0, ymax)+
               geom_hline(yintercept=(parameters["beds_available"]+parameters["icu_beds_available"]+parameters["ventilators_available"]), linetype="dashed", color = "#047883")+
               geom_hline(yintercept=(parameters["icu_beds_available"]+parameters["ventilators_available"]), linetype="dashed", color = "#24A9E2")+
               geom_hline(yintercept=parameters["ventilators_available"], linetype="dashed", color = "#051A46")+
               theme_bw(),
             nrow = 1)


# # # Cumulative mortality at baseline and intervention stratified by hospital status
ymax<-max(c((simul_baseline$total_deaths),(simul_interventions$total_deaths)))
time<-as.Date(out$mean[,1]+startdate)
coul=c("#047883", "#24A9E2","#051A46","#E68029", "#D63304","#D1D604")
DM0<-as.data.frame(cbind(time,simul_baseline$base_mort_I+simul_baseline$base_mort_QI,
                         simul_baseline$base_mort_CL+simul_baseline$base_mort_QC,
                         simul_baseline$base_mort_X,
                         simul_baseline$base_mort_S+simul_baseline$base_mort_QS,
                         simul_baseline$base_mort_E+simul_baseline$base_mort_QE,
                         simul_baseline$base_mort_QR+simul_baseline$base_mort_R,
                         simul_baseline$death_treated_hospital,
                         simul_baseline$death_treated_icu,
                         simul_baseline$death_treated_ventilator,
                         simul_baseline$death_untreated_hospital,
                         simul_baseline$death_untreated_icu,
                         simul_baseline$death_untreated_ventilator))
colnames(DM0)<-c("Time","Asymptomatic","Clnical","Self-Isolating","Susceptible","Exposed","Recovered",
                 "Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator")
DM0$Time<-as.Date(DM0$Time, origin = "1970-01-01")
DMF0<-melt(DM0, id.vars="Time",measure.vars = c("Asymptomatic","Clnical","Self-Isolating","Susceptible","Exposed","Recovered",
                                                "Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator"))
m0<-ggplot(DMF0, aes(x = Time, y = value,fill=variable)) +
  geom_area()

DM<-as.data.frame(cbind(time,simul_interventions$base_mort_I+simul_interventions$base_mort_QI,simul_interventions$base_mort_CL+simul_interventions$base_mort_QC,simul_interventions$base_mort_X,simul_interventions$base_mort_S+
                          simul_interventions$base_mort_QS,simul_interventions$base_mort_E+simul_interventions$base_mort_QE,simul_interventions$base_mort_QR+simul_interventions$base_mort_R,
                        simul_interventions$death_treated_hospital,simul_interventions$death_treated_icu,simul_interventions$death_treated_ventilator,simul_interventions$death_untreated_hospital,simul_interventions$death_untreated_icu,simul_interventions$death_untreated_ventilator))
colnames(DM)<-c("Time","Asymptomatic","Clnical","Self-Isolating","Susceptible","Exposed","Recovered",
                "Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator")
DM$Time<-as.Date(DM$Time, origin = "1970-01-01")
DMF<-melt(DM, id.vars="Time",measure.vars = c("Asymptomatic","Clnical","Self-Isolating","Susceptible","Exposed","Recovered",
                                              "Treated: Hospital","Treated: ICU","Treated: Ventilator","Untreated: Hospital","Untreated: ICU","Untreated: Ventilator"))
m1<-ggplot(DMF, aes(x = Time, y = value,fill=variable)) +
  geom_area()
grid.arrange(m0+ylab("Cumulatice mortality")+
               ggtitle("Baseline")+
               ylim(0, ymax),
             m1+ylab("Cumulatice mortality")+
               ggtitle("Intervention")+
               ylim(0, ymax),
             nrow = 1)



# Estimated basic reproduction number, R_t
par(mfrow=c(1,2))
ymax<-max(c(simul_baseline$Rt[!is.na(simul_baseline$Rt)],simul_interventions$Rt[!is.na(simul_interventions$Rt)]))
plot(time,simul_baseline$Rt,type='l',lwd=3,col='black',
     main="Baseline", xlab="Date", ylab="Reproduction number",ylim=c(0,ymax))
lines(time,simul_baseline$Rt/simul_baseline$Rt,lwd=2,col='grey')
plot(time,simul_interventions$Rt,type='l',lwd=3,col='black',
     main="Intervention", xlab="Date", ylab="Reproduction number",ylim=c(0,ymax))
lines(time,simul_interventions$Rt/simul_interventions$Rt,lwd=2,col='grey')


# ## Predicted ifr
# ymax=max(c(simul_baseline$MORT1$value,simul_interventions$MORT1$value))
# gm<-ggplot(data=simul_interventions$MORT1,aes(x=Age,y=value,fill=variable))+
#   geom_line(data=simul_interventions$MORT1,aes(x=Age,y=value,colour=variable),lwd=1.5)+ylim(0,ymax)+ylab("Mortality")
# gm0<-ggplot(data=simul_baseline$MORT1,aes(x=Age,y=value,fill=variable))+
#   geom_line(data=simul_baseline$MORT1,aes(x=Age,y=value,colour=variable),lwd=1.5)+ylim(0,ymax)+ylab("Mortality")
# 
# grid.arrange(gm+theme_classic(),
#              gm0+theme_classic(),
#              nrow=1)


# ## AGE DEPENDENT MORTALITY
# cinc_mort_H1 <- parameters["nus"]*parameters["pdeath_h"]*(out$mean[,(Hindex+1)])
# cinc_mort_HC1 <- parameters["nusc"]*parameters["pdeath_hc"]*(out$mean[,(HCindex+1)])
# cinc_mort_ICU1 <- parameters["nu_icu"]*parameters["pdeath_icu"]*out$mean[,(ICUindex+1)]
# cinc_mort_ICUC1 <- parameters["nu_icuc"]*parameters["pdeath_icuc"]*out$mean[,(ICUCindex+1)]
# cinc_mort_Vent1 <- parameters["nu_vent"]*parameters["pdeath_vent"]*out$mean[,(Ventindex+1)]
# cinc_mort_VentC1 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out$mean[,(VentCindex+1)]
# totage1<-as.data.frame(cinc_mort_H1+cinc_mort_HC1+cinc_mort_ICU1+cinc_mort_ICUC1+cinc_mort_Vent1+cinc_mort_VentC1)
# basemort_H1<-(out$mean[,(Hindex+1)])
# basemort_HC1<-(out$mean[,(HCindex+1)])
# basemort_ICU1<-(out$mean[,(ICUindex+1)])
# basemort_ICUC1<-(out$mean[,(ICUCindex+1)])
# basemort_Vent1<-(out$mean[,(Ventindex+1)])
# basemort_VentC1<-(out$mean[,(VentCindex+1)])
# totbase1<-as.data.frame(basemort_H1+basemort_HC1+basemort_ICU1+basemort_ICUC1+basemort_Vent1+basemort_VentC1)
# tc<-c()
# ages<-seq(0,100,by=5)
# for (i in 1:dim(cinc_mort_H1)[1]) {
#   for (j in 1:dim(cinc_mort_H1)[2]) {
#     tc<-rbind(tc,c(i,ages[j],totage1[i,j]*ifr[j,2]+totbase1[i,j]*mort[j]))
#   }
# }
# tc<-as.data.frame(tc)
# colnames(tc)<-c("Day","Age","value")
# tc$Age<-as.factor(tc$Age)
# p6<-ggplot(data=tc, aes(x=Day,y=value,fill=Age))+
#   geom_bar(stat = "identity",position="fill", width=1)+
#   ylab("Proportion of deaths")
#
# inc_mort_H0  <- parameters["nus"]*parameters["pdeath_h"]*(out0$mean[,(Hindex+1)])
# inc_mort_HC0 <- parameters["nusc"]*parameters["pdeath_hc"]*(out0$mean[,(HCindex+1)])
# inc_mort_ICU0 <- parameters["nu_icu"]*parameters["pdeath_icu"]*out0$mean[,(ICUindex+1)]
# inc_mort_ICUC0 <- parameters["nu_icuc"]*parameters["pdeath_icuc"]*out0$mean[,(ICUCindex+1)]
# inc_mort_Vent0 <- parameters["nu_vent"]*parameters["pdeath_vent"]*out0$mean[,(Ventindex+1)]
# inc_mort_VentC0 <- parameters["nu_ventc"]*parameters["pdeath_ventc"]*out0$mean[,(VentCindex+1)]
# totage0<-as.data.frame(inc_mort_H0+inc_mort_HC0+inc_mort_ICU0+inc_mort_ICUC0+inc_mort_Vent0+inc_mort_VentC0)
# basemort_H0<-(out0$mean[,(Hindex+1)])
# basemort_HC0<-(out0$mean[,(HCindex+1)])
# basemort_ICU0<-(out0$mean[,(ICUindex+1)])
# basemort_ICUC0<-(out0$mean[,(ICUCindex+1)])
# basemort_Vent0<-(out0$mean[,(Ventindex+1)])
# basemort_VentC0<-(out0$mean[,(VentCindex+1)])
# totbase0<-as.data.frame(basemort_H0+basemort_HC0+basemort_ICU0+basemort_ICUC0+basemort_Vent0+basemort_VentC0)
# tc0<-c()
# for (i in 1:dim(cinc_mort_H1)[1]) {
#   for (j in 1:dim(cinc_mort_H1)[2]) {
#     tc0<-rbind(tc0,c(i,ages[j],totage0[i,j]*ifr[j,2]+totbase0[i,j]*mort[j]))
#   }
# }
# tc0<-as.data.frame(tc0)
# colnames(tc0)<-c("Day","Age","value")
# tc0$Age<-as.factor(tc0$Age)
# p16<-ggplot(data=tc0, aes(x=Day,y=value,fill=Age))+
#   geom_bar(stat = "identity",position="fill", width=1)+ylab("Proportion of deaths")

# grid.arrange(p16+theme_minimal(),
#              p6+theme_minimal(),
#              nrow=1)
#


#
#
#
# #########################################################################################################################
# #####   SUMMARY METRICS  ################################################################################################
# #######################################################################################################################
#
infected0<-tail((rowSums(out0$mean[,(Rindex+1)])),1)/sum(popstruc[,2])
infected0
infected1<-tail((rowSums(out$mean[,(Rindex+1)])),1)/sum(popstruc[,2])
infected1

# # #Population size checks
# # tpop1
# # tpop0
#
# # PCR
# time_of_measurement<-40:49
# # general population
# (rowSums(out$mean[time_of_measurement,Iindex+1])+rowSums(out$mean[time_of_measurement,CLindex+1])+rowSums(out$mean[time_of_measurement,QIindex+1])+
#     rowSums(out$mean[time_of_measurement,QCindex+1]))/sum(popstruc[,2])
# # every infection including hospital infections
# (rowSums(out$mean[time_of_measurement,Iindex+1])+rowSums(out$mean[time_of_measurement,CLindex+1])+rowSums(out$mean[time_of_measurement,Hindex+1])+
#     rowSums(out$mean[time_of_measurement,ICUindex+1])+rowSums(out$mean[time_of_measurement,Ventindex+1])+rowSums(out$mean[time_of_measurement,HCindex+1])+
#     rowSums(out$mean[time_of_measurement,ICUCindex+1])+rowSums(out$mean[time_of_measurement,VentCindex+1])+rowSums(out$mean[time_of_measurement,QIindex+1])+
#     rowSums(out$mean[time_of_measurement,QCindex+1]))/sum(popstruc[,2])
#
# SEROLOGY
tail((rowSums(out$mean[,(Rindex+1)])),1)/sum(popstruc[,2])

# IHR
sum(ihr$severe*popstruc[,2]/sum(popstruc[,2]))


# # PORPORTIONAL MORTALITY IN THE ELDEST
# m30<-out0$mean[30,CMindex+1]/(out0$mean[30,Cindex+1])
# m30[is.infinite(m30)]<-0
# m60<-out0$mean[60,CMindex+1]/out0$mean[60,Cindex+1]
# m60[is.infinite(m60)]<-0
# m90<-out0$mean[90,CMindex+1]/out0$mean[90,Cindex+1]
# m90[is.infinite(m90)]<-0
# m120<-out0$mean[120,CMindex+1]/out0$mean[120,Cindex+1]
# m120[is.infinite(m120)]<-0
# 
# ifr30<-sum(m30*popstruc[,2]/sum(popstruc[,2]),na.rm = T)
# ifr60<-sum(m60*popstruc[,2]/sum(popstruc[,2]),na.rm = T)
# ifr90<-sum(m90*popstruc[,2]/sum(popstruc[,2]),na.rm = T)
# ifr120<-sum(m120*popstruc[,2]/sum(popstruc[,2]),na.rm = T)
# cbind(ifr30,ifr60,ifr90,ifr120)*100
# 
# PMORTDF0<-as.data.frame(cbind(out0$mean[30,CMindex+1]/sum(out0$mean[30,CMindex+1]),out0$mean[60,CMindex+1]/sum(out0$mean[60,CMindex+1]),
#                               out0$mean[90,CMindex+1]/sum(out0$mean[90,CMindex+1]),out0$mean[120,CMindex+1]/sum(out0$mean[120,CMindex+1])))
# PMORTDF<-as.data.frame(cbind(out$mean[30,CMindex+1]/sum(out$mean[30,CMindex+1]),out$mean[60,CMindex+1]/sum(out$mean[60,CMindex+1]),
#                              out$mean[90,CMindex+1]/sum(out$mean[90,CMindex+1]),out$mean[120,CMindex+1]/sum(out$mean[120,CMindex+1])))
# sum(PMORTDF0$V2[15:21])
# sum(PMORTDF$V2[15:21])


# output doubling time over time first 7 days
# dd<-7
# doub0<-log(2)*dd/(log(dailyinc0[2+dd]/dailyinc0[2]))
# doub0
# 
# 
#


