// comomodel.net
// c++ version
// first written by sompob@tropmedre.ac
// modified by bo.gao@ndm.ox.ac.uk
//
// for using with deSolve package in R
#include <Rmath.h>
#include <RcppArmadillo.h>

#include <chrono>
// #include "gperftools/profiler.h"

#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

using namespace std::chrono; 

// arma::mat getVarmat(std::string m){
//   Environment glob = Environment::global_env();
//   arma::mat var = as<arma::mat>(glob[m]);
//   return(var);
// }

// double SplineFun(NumericVector& a, NumericVector& b, double c){
//     Function splinefun("splinefun");
//     Function fn = splinefun(a,b, Named("method")="hyman");
//     double out = as<double>(fn(c));
//     return(out);
// }

double MIN(const arma::vec& a, double m){
  arma::vec b = a;
  int sz = b.size();
  b.resize(sz + 1);
  b(sz) = m;
  return(min(b));
}


////////////////////////////////////////////////////
// Performance measures

std::chrono::duration<double> duration_a, duration_b, duration_c;

// [[Rcpp::export]]
void covidOdeCpp_print_timing() {
  std::cout << "duration_a=" << duration_a.count() << "\n"
            << "duration_b=" << duration_b.count() << "\n"
            << "duration_c=" << duration_c.count() << "\n";
}

bool ask_input = true;

////////////////////////////////////////////////////
// Splinefun

bool is_initialised_splinefun=false;
Environment pkg_stats = Environment::namespace_env("stats");
Function sf=pkg_stats["splinefun"];
// Function sf("splinefun");

NumericVector dummy_a = {1,2,3,4};
NumericVector dummy_b = {1,2,3,4};

Function splinefun_H = sf(dummy_a, dummy_b, Named("method")="hyman");
Function splinefun_ICU = sf(dummy_a, dummy_b, Named("method")="hyman");
Function splinefun_Vent = sf(dummy_a, dummy_b, Named("method")="hyman");

void init_splinefun(
      NumericVector& xH,
      NumericVector& xICU,
      NumericVector& xVent,
      NumericVector& f
    ) {
  splinefun_H = sf(xH, f, Named("method")="hyman");
  splinefun_ICU = sf(xICU, f, Named("method")="hyman");
  splinefun_Vent = sf(xVent, f, Named("method")="hyman");

  is_initialised_splinefun=true;
  std::cout << "covidOdeCpp: splinefuns updated\n";
  // std::cout << "sourceCpp version\n";
}


////////////////////////////////////////////////////
// Static Parameters

bool is_initialised_parameters=false;


// [[Rcpp::export]]
void covidOdeCpp_reset() {
  duration_a = std::chrono::duration<double>::zero();
  duration_b = std::chrono::duration<double>::zero();
  duration_c = std::chrono::duration<double>::zero();

  is_initialised_splinefun = false;
  is_initialised_parameters = false;

  ask_input = true;
}

// [[Rcpp::export]]
List covidOdeCpp(double t, const arma::vec& y, const List& parameters,
             const List& input, int A,
             const arma::mat& contact_home, const arma::mat& contact_school,
             const arma::mat& contact_work, const arma::mat& contact_other,
             const arma::vec& popbirth_col2, const arma::vec& popstruc_col2,
             const arma::mat& ageing,
             const arma::vec& ifr_col2, const arma::vec& ihr_col2, const arma::vec& mort_col ){
    //Rf_PrintValue(parameters);

// auto start_a = high_resolution_clock::now();

        ////////////////////////////////////////////////////
        // Parameters

        static double p = 0.0;
        static double rho = 0.0;
        static double omega = 0.0;
        static double gamma = 0.0;
        static double nui = 0.0;
        static double report = 0.0;
        static double reportc = 0.0;
        static double reporth = 0.0;
        static double beds_available = 0.0;
        static double icu_beds_available = 0.0;
        static double ventilators_available = 0.0;
        static double nus = 0.0;
        static double rhos = 0.0;
        static double amp = 0.0;
        static double phi = 0.0;
        static double ihr_scaling = 0.0;
        static double give = 0.0;
        static double pdeath_h = 0.0;
        static double pdeath_hc = 0.0;
        static double pdeath_icu = 0.0;
        static double pdeath_icuc = 0.0;
        static double pdeath_vent = 0.0;
        static double pdeath_ventc = 0.0;
        static double nusc = 0.0;
        static double nu_icu = 0.0;
        static double nu_icuc = 0.0;
        static double nu_vent = 0.0;
        static double nu_ventc = 0.0;
        static double pclin = 0.0;
        static double prob_icu = 0.0;
        static double prob_vent = 0.0;

        //interventions
        // self isolation
        static double selfis_eff = 0.0;
        // social distance
        static double dist_eff = 0.0;
        // hand washing
        static double hand_eff = 0.0;
        // working at home
        static double work_eff = 0.0;
        static double w2h = 0.0;
        // school closures
        static double school_eff = 0.0;
        static double s2h = 0.0;
        // cocooning the elderly
        static double cocoon_eff = 0.0;
        static double age_cocoon = 0.0;
        // vaccination
        static double vaccine_eff = 0.0;
        static double vac_campaign = 0.0;
        // imported cases
        static double mean_imports = 0.0;
        // screening 
        static double screen_test_sens = 0.0;
        static double screen_overdispersion = 0.0;
        // quarantine
        static double quarantine_days = 0.0;
        static double quarantine_effort = 0.0;
        static double quarantine_eff_home = 0.0;
        static double quarantine_eff_other = 0.0;
        // lockdown
        // mean household size
        static double household_size = 0.0;

    if (!is_initialised_parameters) {
        p = parameters["p"];
        rho = parameters["rho"];
        omega = parameters["omega"];
        gamma = parameters["gamma"];
        nui = parameters["nui"];
        report = parameters["report"];
        reportc = parameters["reportc"];
        reporth = parameters["reporth"];
        beds_available = parameters["beds_available"];
        icu_beds_available = parameters["icu_beds_available"];
        ventilators_available = parameters["ventilators_available"];
        nus = parameters["nus"];
        rhos = parameters["rhos"];
        amp = parameters["amp"];
        phi = parameters["phi"];
        ihr_scaling = parameters["ihr_scaling"];
        give = parameters["give"];
        pdeath_h = parameters["pdeath_h"];
        pdeath_hc = parameters["pdeath_hc"];
        pdeath_icu = parameters["pdeath_icu"];
        pdeath_icuc = parameters["pdeath_icuc"];
        pdeath_vent = parameters["pdeath_vent"];
        pdeath_ventc = parameters["pdeath_ventc"];
        nusc = parameters["nusc"];
        nu_icu = parameters["nu_icu"];
        nu_icuc = parameters["nu_icuc"];
        nu_vent = parameters["nu_vent"];
        nu_ventc = parameters["nu_ventc"];
        pclin = parameters["pclin"];
        prob_icu = parameters["prob_icu"];
        prob_vent = parameters["prob_vent"];
        
        //interventions
        // self isolation
        selfis_eff = parameters["selfis_eff"];
        // social distance
        dist_eff = parameters["dist_eff"];
        // hand washing
        hand_eff = parameters["hand_eff"];
        // working at home
        work_eff = parameters["work_eff"];
        w2h = parameters["w2h"];
        // school closures
        school_eff = parameters["school_eff"];
        s2h = parameters["s2h"];
        // cocooning the elderly
        cocoon_eff = parameters["cocoon_eff"];
        age_cocoon = parameters["age_cocoon"];
        // vaccination
        vaccine_eff = parameters["vaccine_eff"];
        vac_campaign = parameters["vac_campaign"];
        // imported cases
        mean_imports = parameters["mean_imports"];
        // screening 
        screen_test_sens = parameters["screen_test_sens"];
        screen_overdispersion = parameters["screen_overdispersion"];
        // quarantine
        quarantine_days = parameters["quarantine_days"];
        quarantine_effort = parameters["quarantine_effort"];
        quarantine_eff_home = parameters["quarantine_eff_home"];
        quarantine_eff_other = parameters["quarantine_eff_other"];
        // lockdown
        // mean household size
        household_size = parameters["household_size"];

        is_initialised_parameters=true;
    }
    
  // int A = 21; 

// duration_a += high_resolution_clock::now()-start_a;
// auto start_b = high_resolution_clock::now();

  arma::vec S = y.subvec(0,A-1);
  arma::vec E = y.subvec(A,2*A-1);
  arma::vec I = y.subvec(2*A,3*A-1);
  arma::vec R = y.subvec(3*A,4*A-1);
  arma::vec X = y.subvec(4*A,5*A-1);
  arma::vec H = y.subvec(5*A,6*A-1);
  arma::vec HC = y.subvec(6*A,7*A-1);
  arma::vec C = y.subvec(7*A,8*A-1);
  arma::vec CM = y.subvec(8*A,9*A-1);
  arma::vec V = y.subvec(9*A,10*A-1);
  arma::vec QS = y.subvec(10*A,11*A-1);
  arma::vec QE = y.subvec(11*A,12*A-1);
  arma::vec QI = y.subvec(12*A,13*A-1);
  arma::vec QR = y.subvec(13*A,14*A-1);
  arma::vec CL = y.subvec(14*A,15*A-1);
  arma::vec QC = y.subvec(15*A,16*A-1);
  arma::vec ICU = y.subvec(16*A,17*A-1);
  arma::vec ICUC = y.subvec(17*A,18*A-1);
  arma::vec ICUCV = y.subvec(18*A,19*A-1);
  arma::vec Vent = y.subvec(19*A,20*A-1);
  arma::vec VentC = y.subvec(20*A,21*A-1);
  arma::vec CMC = y.subvec(21*A,22*A-1);

  arma::vec P = (S+E+I+R+X+V+H+HC+QS+QE+QI+QR+CL+QC+ICU+ICUC+ICUCV+Vent+VentC);
  double Q = double((sum(QS)+sum(QE)+sum(QI)+sum(QC)+sum(QR)))/double(sum(P));

// Environment base = Environment("package:base");
// Function readline = base["readline"];
// Function as_numeric = base["as.numeric"];

// if (ask_input) {
//   int drink = as<int>(as_numeric(readline("input: ")));
//   if (drink == 4) {
//     ask_input=false;
//   }
// }


// duration_b += high_resolution_clock::now()-start_b;
// auto start_c = high_resolution_clock::now();
      

// health system performance
   NumericVector f = {1.0,(1.0+give)/2.0,(1.0-give)/2.0,0.0};
   double KH  = beds_available;
   double KICU = icu_beds_available + ventilators_available;
   double Kvent = ventilators_available;
   NumericVector  xH = {0.0,(1.0+give)*KH/2.0,(3.0-give)*KH/2.0,2.0*KH};
   NumericVector  xICU = {0.0,(1.0+give)*KICU/2.0,(3.0-give)*KICU/2.0,2.0*KICU};
   NumericVector  xVent = {0.0,(1.0+give)*Kvent/2.0,(3.0-give)*Kvent/2.0,2.0*Kvent};

   
   // double critH = std::min(1.0 - SplineFun(xH, f,sum(H)+sum(ICUC)+sum(ICUCV)),1.0);
   // double crit = std::min(1.0-SplineFun(xICU, f,sum(ICU)+sum(Vent)+sum(VentC)),1.0);
   // double critV = std::min(1.0-SplineFun(xVent, f,sum(Vent)),1.0);

   double sum_H=sum(H);
   double sum_ICUC=sum(ICUC);
   double sum_ICUCV=sum(ICUCV);
   double sum_ICU=sum(ICU);
   double sum_Vent=sum(Vent);
   double sum_VentC=sum(VentC);

    
    if (!is_initialised_splinefun) {
      init_splinefun(xH, xICU, xVent, f);
    }


   // double critH = std::min(1.0 - SplineFun(xH, f,sum_H+sum_ICUC+sum_ICUCV),1.0);
   // double crit = std::min(1.0-SplineFun(xICU, f,sum_ICU+sum_Vent+sum_VentC),1.0);
   // double critV = std::min(1.0-SplineFun(xVent, f,sum_Vent),1.0);

   double critH = std::min(1.0 - as<double>(splinefun_H(sum_H+sum_ICUC+sum_ICUCV)), 1.0);
   double crit = std::min(1.0 - as<double>(splinefun_ICU(sum_ICU+sum_Vent+sum_VentC)), 1.0);
   double critV = std::min(1.0 - as<double>(splinefun_Vent(sum_Vent)),1.0);
     
// interventions
    // int isolation = (t>=selfis_on)*(t<=selfis_on+selfis_dur);
    // int distancing = (t>=dist_on)*(t<=(dist_on+dist_dur));
    // int handwash = (t>=hand_on)*(t<=(hand_on+hand_dur));
    // int workhome = (t>=work_on)*(t<=(work_on+work_dur));
    // int schoolclose = (t>=school_on)*(t<=(school_on+school_dur));
    // int cocoon = (t>=cocoon_on)*(t<=(cocoon_on+cocoon_dur))*cocoon_cov;
    // int vaccine = (t>=(vaccine_on))*(t<=vaccine_on+vac_campaign);
    // int travelban = (t>=travelban_on)*(t<=(travelban_on+travelban_dur));
    // int screen = (t>=screen_on)*(t<=(screen_on+screen_dur));
    // int quarantine = (t>=quarantine_on)*(t<=(quarantine_on+quarantine_dur));
    // int lockdown_low = (t>=lockdown_low_on)*(t<=(lockdown_low_on+lockdown_low_dur));
    // int lockdown_mid = (t>=lockdown_mid_on)*(t<=(lockdown_mid_on+lockdown_mid_dur));
    // int lockdown_high = (t>=lockdown_high_on)*(t<=(lockdown_high_on+lockdown_high_dur));

    int my_t = t*20;

    int isolation  = as<NumericVector>(input["isolation"])[my_t];
    int distancing = as<NumericVector>(input["distancing"])[my_t];
    int handwash   = as<NumericVector>(input["handwash"])[my_t];
    int workhome   = as<NumericVector>(input["workhome"])[my_t];
    int schoolclose = as<NumericVector>(input["schoolclose"])[my_t];
    // int cocoon     = as<NumericVector>(input["cocoon"])[my_t];
    int vaccine    = as<NumericVector>(input["vaccine"])[my_t];
    int travelban  = as<NumericVector>(input["travelban"])[my_t];
    int screen     = as<NumericVector>(input["screen"])[my_t];
    int quarantine = as<NumericVector>(input["quarantine"])[my_t];
    // int lockdown_low  = as<NumericVector>(input["lockdown_low"])[my_t];
    // int lockdown_mid  = as<NumericVector>(input["lockdown_mid"])[my_t];
    // int lockdown_high = as<NumericVector>(input["lockdown_high"])[my_t];


    double screen_eff = 0.0;
    double selfis = 0.0;
    double school = 1.0;
    double dist = 1.0;
    double hand = 0.0;
    double vaccinate = 0.0;
    double trvban_eff = 0.0;
    double quarantine_rate = 0.0;
    double rate_q = 0.0;

    double work;
    double vac_rate;

    double selfis_cov =      (as<NumericVector>(input["si_vector"])[my_t])/100.0;
    double screen_contacts = (as<NumericVector>(input["scr_vector"])[my_t])/10.0;
    school_eff =      (as<NumericVector>(input["sc_vector"])[my_t])/100.0;
    double dist_cov =        (as<NumericVector>(input["sd_vector"])[my_t])/100.0;
    double hand_cov =        (as<NumericVector>(input["hw_vector"])[my_t])/100.0;
    double cocoon =          (as<NumericVector>(input["cte_vector"])[my_t])/100.0;
    double work_cov =        (as<NumericVector>(input["wah_vector"])[my_t])/100.0;
    double travelban_eff =   (as<NumericVector>(input["tb_vector"])[my_t])/100.0;
    double vaccine_cov =     (as<NumericVector>(input["vc_vector"])[my_t])/100.0;
    double quarantine_cov =  (as<NumericVector>(input["q_vector"])[my_t])/100.0;


// if (lockdown_low || lockdown_mid || lockdown_high){
//            if(lockdown_low){
//              selfis = 0.5;
//              dist = 0.25;
//              school = 0.0;
//              trvban_eff = 0.0;
//              quarantine_rate = 0.0;
//              work = 0.0;
//              cocoon = 0.95;
//              hand = 0.05;
//              vaccinate = 0.0;
//            }
//            if(lockdown_mid){
//              selfis = 0.5;
//              dist = 0.35;
//              school = 0.85;
//              trvban_eff = 0.0;
//              quarantine_rate = 0.05;
//              work = 0.5;
//              cocoon= 0.95;
//              hand = 0.05;
//              vaccinate = 0.0;
//            }
//            if(lockdown_high){
//              selfis = 0.95;
//              dist = 0.95;
//              school = 0.85;
//              trvban_eff = 0.95;
//              quarantine_rate = 0.9;
//              work = 0.75;
//              cocoon = 0.95;
//              hand = 0.075;
//              vaccinate= 0.0;
//            }
//          }
//          else{
           if (workhome){
             work = work_cov*work_eff;
           }else{work = 1.0;}
           if (isolation){
             selfis = selfis_cov;
             if(screen){
               // screen_eff = MIN((report*I + reportc*CL + H+ICU+Vent+ reporth*HC + ICUC+VentC)  *screen_contacts*(screen_overdispersion*I/P)*screen_cov/P,1.0); 
               screen_eff = MIN(sum(report*I + reportc*CL + H+ICU+Vent+ reporth*(HC+ICUC+ICUCV+VentC)) *screen_contacts*(screen_overdispersion*I/P)*screen_test_sens/P,1.0); 
             }
           }
           if (schoolclose){
             school = school_eff;
           }
           if(distancing){
             dist = dist_cov*dist_eff;
           }
           if(handwash){
             hand = hand_eff*hand_cov;
           }
           if(vaccine){
             vac_rate = -log(1.0-vaccine_cov)/vac_campaign;
             vaccinate = vac_rate;
           }
           if(travelban){
             trvban_eff = travelban_eff;
           }
           if(quarantine){
             rate_q = std::min(std::min(sum((I+CL+H+ICU+Vent+HC+ICUC+ICUCV+VentC))*(household_size-1.0)/sum(P),1.0) * quarantine_effort, quarantine_cov/2.0);
             quarantine_rate = rate_q/(1.0+exp(-10.0*(quarantine_cov/2.0-Q)));
           }
         // }


        // cocooning the elderly
        // arma::mat cocoon_mat(21,21);
        arma::mat cocoon_mat(A,A);
        cocoon_mat.fill((1.0-cocoon_eff));
        arma::mat cc_sub(age_cocoon-1,age_cocoon-1);
        cc_sub.fill(1.0);
        cocoon_mat.submat(0,0,cc_sub.n_rows-1,cc_sub.n_rows-1) = cc_sub;

        // arma::mat contact_home = getVarmat("contact_home");
        // arma::mat contact_other = getVarmat("contact_other");
        // arma::mat contact_school = getVarmat("contact_school");
        // arma::mat contact_work = getVarmat("contact_work");
        
        // contact matrices
        arma::mat cts = (contact_home+distancing*(1.0-dist)*contact_other+(1.0-distancing)*contact_other
               +(1.0-schoolclose)*contact_school //# school on
               +schoolclose*(1.0-school)*contact_school //# school close
               +schoolclose*contact_home*school*s2h //# inflating contacts at home when school closes
               +(1.0-workhome)*contact_work  //# normal work
               +workhome*(1.0-work)*contact_work //# people not working from home when homework is active
               +contact_home*workhome*work*w2h //# inflating contacts at home when working from home
         );


        // Final transmission related parameters
        arma::mat contacts = (1.0-cocoon)*cts+cocoon*cts%cocoon_mat+cocoon*(1.0+schoolclose*(1.0-school_eff)+workhome*(1.0-work_eff))*contact_home%(1.0-cocoon_mat);
        double seas = 1.0+amp*cos(2.0*3.14*(t-(phi*365.25/12.0))/365.25);
        double importation = mean_imports*(1.0-trvban_eff);
        
        arma::vec HH = H+ICU+Vent+ICUC+ICUCV+VentC;
        arma::vec HHC = HC;

        arma::vec lam = (1.0-hand)*p*seas*(contacts*((rho*E+(I+CL+importation)+(1.0-selfis_eff)*(X+HHC)+rhos*(HH))/P))
                        + (1.0-hand)*p*seas*(1.0-quarantine*quarantine_eff_other)*(contact_other*((rho*QE+QI+QC)/P));

                // # contacts under home quarantine
        arma::vec lamq = (1.0-hand)*p*seas*((1.0-quarantine_eff_home)*contact_home*(((1.0-selfis_eff)*(X+HHC+rho*QE+QI+QC))/P))
                        + (1.0-hand)*p*seas*(1.0-quarantine_eff_other)*(contact_other*((rho*E+(I+CL+importation)+(1.0-selfis_eff)*(X+HHC+rho*QE+QI+QC)+rhos*(HH))/P));
         
        // # birth/death
        double b1 = sum(popbirth_col2 % popstruc_col2);
        arma::vec birth = 0.0*popbirth_col2;
        birth(0) = b1;



        
      // arma::mat ageing = getVarmat("ageing");
      
      //# ODE system
      arma::vec dSdt = -S%lam-S*vaccinate+omega*R+ageing*S-mort_col%S+birth-quarantine_rate*S +(1.0/quarantine_days)*QS;
      arma::vec dEdt = S%lam-gamma*E+ageing*E-mort_col%E + (1.0-vaccine_eff)*(lam%V)-quarantine_rate*E+(1.0/quarantine_days)*QE;
      arma::vec dIdt = gamma*(1.0-pclin)*(1.0-screen_eff)*(1.0-ihr_col2)%E-nui*I+ageing*I-mort_col%I + (1.0/quarantine_days)*QI - quarantine_rate*I;
      arma::vec dCLdt = gamma*pclin*(1.0-selfis)*(1.0-ihr_col2)*(1.0-quarantine_rate)%E-nui*CL+ageing*CL-mort_col%CL + (1.0/quarantine_days)*QC;

      arma::vec dRdt = nui*I-omega*R+nui*X+nui*CL+ageing*R-mort_col%R + (1.0/quarantine_days)*QR + nus*(1.0-pdeath_h*ifr_col2)%H
        + ((1.0-pdeath_icu*ifr_col2)*nu_icu)%ICU + ((1.0-pdeath_icuc*ifr_col2)*nu_icuc)%ICUC + ((1.0-pdeath_hc*ifr_col2)*nu_ventc)%ICUCV
        + ((1.0-pdeath_hc*ifr_col2)*nusc)%HC + ((1.0-pdeath_vent*ifr_col2)*nu_vent)%Vent
        + ((1.0-pdeath_ventc*ifr_col2)*nu_ventc)%VentC;

      arma::vec dXdt = (gamma*selfis*pclin*(1.0-ihr_col2))%E+(gamma*(1.0-pclin)*screen_eff*(1.0-ihr_col2))%E-nui*X+ageing*X-mort_col%X; 
      arma::vec dVdt = vaccinate*S -((1.0-vaccine_eff)*lam)%V +ageing*V - mort_col%V;
         
      arma::vec dQSdt = quarantine_rate*S+ ageing*QS-mort_col%QS - (1.0/quarantine_days)*QS - lamq%QS;
      arma::vec dQEdt = quarantine_rate*E - gamma*QE + ageing*QE-mort_col%QE - (1.0/quarantine_days)*QE + lamq%QS; 
      arma::vec dQIdt = quarantine_rate*I + (gamma*(1.0-ihr_col2)*(1.0-pclin))%QE-nui*QI+ageing*QI-mort_col%QI - (1.0/quarantine_days)*QI;
      arma::vec dQCdt = (gamma*pclin*(1.0-selfis)*(1.0-ihr_col2))*quarantine_rate%E + (gamma*(1.0-ihr_col2))%(pclin*QE)-nui*QC+ageing*QC-mort_col%QC - (1.0/quarantine_days)*QC;
      arma::vec dQRdt = nui*QI+nui*QC+ageing*QR-mort_col%QR - (1.0/quarantine_days)*QR;
         

      arma::vec dHdt = gamma*ihr_col2%((1.0-prob_icu)*(1.0-critH)*reporth*E) + gamma*ihr_col2%((1.0-prob_icu)*(1.0-critH)*QE) - nus*H + ageing*H-mort_col%H; //  # all pdeath have to be lower than
      arma::vec dHCdt = gamma*ihr_col2%((1.0-prob_icu)*(1.0-critH)*(1.0-reporth)*E) + gamma*ihr_col2%((1.0-prob_icu)*critH*E) + gamma*ihr_col2%((1.0-prob_icu)*critH*QE) - nusc*HC + ageing*HC-mort_col%HC;
      arma::vec dICUdt = gamma*ihr_col2%(prob_icu*(1.0-crit)*(1.0-prob_vent)*E) + gamma*ihr_col2%(prob_icu*(1.0-crit)*(1.0-prob_vent)*QE) - nu_icu*ICU +ageing*ICU - mort_col%ICU +(1.0-crit)*ICUC*0.5;
      arma::vec dICUCdt = gamma*ihr_col2%(prob_icu*crit*(1.0-prob_vent)*E) + gamma*ihr_col2%(prob_icu*crit*(1.0-prob_vent)*QE)
                          - nu_icuc*ICUC - (1.0-crit)*ICUC*0.5 +ageing*ICUC - mort_col%ICUC;
      arma::vec dICUCVdt = gamma*ihr_col2%(prob_icu*prob_vent*crit*E) +gamma*ihr_col2%(prob_icu*prob_vent*crit*QE) -nu_ventc*ICUCV +ageing*ICUCV - mort_col%ICUCV - (1.0-critV)*ICUCV*0.5;
      arma::vec dVentdt = gamma*ihr_col2%(prob_icu*(1.0-crit)*(1.0-critV)*prob_vent*E) + gamma*ihr_col2%(prob_icu*(1.0-crit)*(1.0-critV)*prob_vent*QE) + (1.0-critV)*VentC*1.0/2.0 
                          + (1.0-critV)*ICUCV*1.0/2.0 - nu_vent*Vent +ageing*Vent - mort_col%Vent ;
      arma::vec dVentCdt = gamma*ihr_col2%(prob_icu*prob_vent*(1.0-crit)*critV*E) +gamma*ihr_col2%(prob_icu*prob_vent*(1.0-crit)*critV*QE) 
                          - (1.0-critV)*VentC*1.0/2.0-nu_ventc*VentC +ageing*VentC - mort_col%VentC ;
      arma::vec dCdt = report*gamma*(1.0-pclin)*(1.0-ihr_col2)%(E+QE)+reportc*gamma*pclin*(1.0-ihr_col2)%(E+QE)
                          + gamma*ihr_col2%((1.0-critH)*(1.0-prob_icu)*(E+QE))+gamma*ihr_col2%(critH*reporth*(1.0-prob_icu)*(E+QE))
                          + gamma*ihr_col2%(prob_icu*(E+QE));
      arma::vec dCMdt = nus*pdeath_h*ifr_col2%H + nusc*pdeath_hc*ifr_col2%HC + nu_icu*pdeath_icu*ifr_col2%ICU + nu_icuc*pdeath_icuc*ifr_col2%ICUC +  nu_vent*pdeath_vent*ifr_col2%Vent + nu_ventc*pdeath_ventc*ifr_col2%VentC
                        + nu_ventc*pdeath_ventc*ifr_col2%ICUCV
                        + mort_col%H + mort_col%HC + mort_col%ICU + mort_col%ICUC + mort_col%Vent + mort_col%VentC; 
      arma::vec dCMCdt = nusc*pdeath_hc*ifr_col2%HC + nu_icuc*pdeath_icuc*ifr_col2%ICUC + nu_ventc*pdeath_ventc*ifr_col2%VentC + nu_ventc*pdeath_ventc*ifr_col2%ICUCV
                          + mort_col%HC + mort_col%ICUC + mort_col%VentC + mort_col%ICUCV;


  arma::vec outvec = dSdt;
  outvec = arma::join_cols(outvec,dEdt);
  outvec = arma::join_cols(outvec,dIdt);
  outvec = arma::join_cols(outvec,dRdt);
  outvec = arma::join_cols(outvec,dXdt);
  outvec = arma::join_cols(outvec,dHdt);
  outvec = arma::join_cols(outvec,dHCdt);
  outvec = arma::join_cols(outvec,dCdt);
  outvec = arma::join_cols(outvec,dCMdt);
  outvec = arma::join_cols(outvec,dVdt);
  outvec = arma::join_cols(outvec,dQSdt);
  outvec = arma::join_cols(outvec,dQEdt);
  outvec = arma::join_cols(outvec,dQIdt);
  outvec = arma::join_cols(outvec,dQRdt);
  outvec = arma::join_cols(outvec,dCLdt);
  outvec = arma::join_cols(outvec,dQCdt);
  outvec = arma::join_cols(outvec,dICUdt);
  outvec = arma::join_cols(outvec,dICUCdt);
  outvec = arma::join_cols(outvec,dICUCVdt);
  outvec = arma::join_cols(outvec,dVentdt);
  outvec = arma::join_cols(outvec,dVentCdt);
  outvec = arma::join_cols(outvec,dCMCdt);


  List output(1);
  output[0] = NumericVector(outvec.begin(),outvec.end());

// duration_c += high_resolution_clock::now()-start_c;

  return output;
}
