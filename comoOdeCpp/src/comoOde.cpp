// comomodel.net
// c++ version
// first written by sompob@tropmedre.ac
// modified (from v13.8.1) by bo.gao@ndm.ox.ac.uk
//
// for using with deSolve package in R
#include <Rmath.h>
#include <RcppArmadillo.h>

#include <chrono>
// #include "gperftools/profiler.h"

// #include <iostream>

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
  Rcout << "duration_a=" << duration_a.count() << "\n"
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
  Rcout << "covidOdeCpp: splinefuns updated\n";
  // Rcout << "sourceCpp version\n";
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

        // Ones selected for fitting
        double p = parameters["p"];
        double reporth = parameters["reporth"];
        double amp = parameters["amp"];

        // 

        // static double p = 0.0;
        static double rho = 0.0;
        static double omega = 0.0;
        static double gamma = 0.0;
        static double nui = 0.0;
        static double report = 0.0;
        static double reportc = 0.0;
        static double report_v = 0.0;
        static double report_cv = 0.0;
        static double report_vr = 0.0;
        static double report_cvr = 0.0;
        static double report_r = 0.0;
        static double report_cr = 0.0;

        // static double reporth = 0.0;


        static double beds_available = 0.0;
        static double icu_beds_available = 0.0;
        static double ventilators_available = 0.0;
        static double nus = 0.0;
        static double rhos = 0.0;
        // static double amp = 0.0;
        static double phi = 0.0;
        // static double ihr_scaling = 0.0;
        static double give = 0.0;
        static double pdeath_h = 0.0;
        static double pdeath_ho = 0.0;
        static double pdeath_hc = 0.0;
        static double pdeath_hco = 0.0;
        static double pdeath_icu = 0.0;
        static double pdeath_icuo = 0.0;
        static double pdeath_icuc = 0.0;
        static double pdeath_icuco = 0.0;
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
        // mask wearing
        static double mask_eff = 0.0;
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
        static double vaccine_eff_r = 0.0;
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

        static double mass_test_sens = 0.0;
        static double isolation_days = 0.0;

        static double propo2 = 0.0;
        static double pa_dexo2 = 0.0;
        static double pa_dexo2c = 0.0;
        static double pa_dexv = 0.0;
        static double pa_dexvc = 0.0;
        static double vent_dex = 0.0;

        static double prob_icu_v = 0.0;
        static double prob_icu_vr = 0.0;
        static double prob_icu_r = 0.0;
        static double prob_v_v = 0.0;
        static double prob_v_r = 0.0;
        static double prob_v_vr = 0.0;
        static double pclin_v = 0.0;
        static double pclin_vr = 0.0;
        static double pclin_r = 0.0;
        static double sigmaEV = 0.0;
        static double sigmaER = 0.0;
        static double sigmaEVR = 0.0;
        static double sigmaR = 0.0;
        static double vac_dur = 0.0;
        static double vac_dur_r = 0.0;
        static double report_natdeathI = 0.0;
        static double report_natdeathCL = 0.0;
        static double report_death_HC = 0.0;
        static double reporth_ICU = 0.0;
        static double pre = 0.0;
        static double pdeath_vent_hc = 0.0;
        static double pdeath_icu_hc = 0.0;
        static double pdeath_icu_hco = 0.0;
        static double reporth_g = 0.0;
        static double seroneg = 0.0;

    if (!is_initialised_parameters) {
        // p = parameters["p"];
        rho = parameters["rho"];
        omega = parameters["omega"];
        gamma = parameters["gamma"];
        nui = parameters["nui"];
        report = parameters["report"];
        reportc = parameters["reportc"];
        report_v = parameters["report_v"];
        report_cv = parameters["report_cv"];
        report_vr = parameters["report_vr"];
        report_cvr = parameters["report_cvr"];
        report_r = parameters["report_r"];
        report_cr = parameters["report_cr"];
        // reporth = parameters["reporth"];
        beds_available = parameters["beds_available"];
        icu_beds_available = parameters["icu_beds_available"];
        ventilators_available = parameters["ventilators_available"];
        nus = parameters["nus"];
        rhos = parameters["rhos"];
        // amp = parameters["amp"];
        phi = parameters["phi"];
        // ihr_scaling = parameters["ihr_scaling"];
        give = parameters["give"];
        pdeath_h = parameters["pdeath_h"];
        pdeath_ho = parameters["pdeath_ho"];
        pdeath_hc = parameters["pdeath_hc"];
        pdeath_hco = parameters["pdeath_hco"];
        pdeath_icu = parameters["pdeath_icu"];
        pdeath_icuo = parameters["pdeath_icuo"];
        pdeath_icuc = parameters["pdeath_icuc"];
        pdeath_icuco = parameters["pdeath_icuco"];
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
        // mask wearing
        mask_eff = parameters["mask_eff"];
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
        vaccine_eff_r = parameters["vaccine_eff_r"];
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

        mass_test_sens = parameters["mass_test_sens"];
        isolation_days = parameters["isolation_days"];

        propo2 = parameters["propo2"];
        pa_dexo2 = parameters["dexo2"];
        pa_dexo2c = parameters["dexo2c"];
        pa_dexv = parameters["dexv"];
        pa_dexvc = parameters["dexvc"];
        vent_dex = parameters["vent_dex"];

        prob_icu_v = parameters["prob_icu_v"];
        prob_icu_vr = parameters["prob_icu_vr"];
        prob_icu_r = parameters["prob_icu_r"];
        prob_v_v = parameters["prob_v_v"];
        prob_v_r = parameters["prob_v_r"];
        prob_v_vr = parameters["prob_v_vr"];
        pclin_v = parameters["pclin_v"];
        pclin_vr = parameters["pclin_vr"];
        pclin_r = parameters["pclin_r"];
        sigmaEV = parameters["sigmaEV"];
        sigmaER = parameters["sigmaER"];
        sigmaEVR = parameters["sigmaEVR"];
        sigmaR = parameters["sigmaR"];
        vac_dur = parameters["vac_dur"];
        vac_dur_r = parameters["vac_dur_r"];
        report_natdeathI = parameters["report_natdeathI"];
        report_natdeathCL = parameters["report_natdeathCL"];
        report_death_HC = parameters["report_death_HC"];
        reporth_ICU = parameters["reporth_ICU"];
        pre = parameters["pre"];
        pdeath_vent_hc = parameters["pdeath_vent_hc"];
        pdeath_icu_hc = parameters["pdeath_icu_hc"];
        pdeath_icu_hco = parameters["pdeath_icu_hco"];
        reporth_g = parameters["reporth_g"];
        seroneg = parameters["seroneg"];

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
  arma::vec Z = y.subvec(22*A,23*A-1);

  arma::vec EV  = y.subvec(23*A,24*A-1);
  arma::vec ER  = y.subvec(24*A,25*A-1);
  arma::vec EVR = y.subvec(25*A,26*A-1);
  arma::vec VR  = y.subvec(26*A,27*A-1);
  arma::vec QV  = y.subvec(27*A,28*A-1);
  arma::vec QEV = y.subvec(28*A,29*A-1);
  arma::vec QEVR= y.subvec(29*A,30*A-1);
  arma::vec QER = y.subvec(30*A,31*A-1);
  arma::vec QVR = y.subvec(31*A,32*A-1);
  arma::vec HCICU = y.subvec(32*A,33*A-1);
  arma::vec HCV = y.subvec(33*A,34*A-1);
  arma::vec Ab = y.subvec(34*A,35*A-1);
  
  // Zindex<-(22*A+1):(23*A)

  arma::vec P = (S+E+I+R+X+Z+V+H+HC+ICU+ICUC+ICUCV+Vent+VentC+EV+ER+EVR+VR+HCICU+HCV+
                  QS+QE+QI+QR+CL+QC+QEV+QV+QER+QEVR+QVR);

  double Q = double((sum(QS)+sum(QE)+sum(QI)+sum(QC)+sum(QR)+sum(QV)+sum(QER)+sum(QEVR)+sum(QEV)+sum(QVR)))/double(sum(P));
// Rcout << "Q=" << Q << "\n";
  
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
    if (!is_initialised_splinefun) {
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

      init_splinefun(xH, xICU, xVent, f);
    }

   double sum_H=sum(H);
   double sum_ICUC=sum(ICUC);
   double sum_ICUCV=sum(ICUCV);
   double sum_ICU=sum(ICU);
   double sum_Vent=sum(Vent);
   double sum_VentC=sum(VentC);

    
   // double critH = std::min(1.0 - SplineFun(xH, f,sum_H+sum_ICUC+sum_ICUCV),1.0);
   // double crit = std::min(1.0-SplineFun(xICU, f,sum_ICU+sum_Vent+sum_VentC),1.0);
   // double critV = std::min(1.0-SplineFun(xVent, f,sum_Vent),1.0);

   double critH = std::min(1.0 - as<double>(splinefun_H(sum_H+sum_ICUC+sum_ICUCV)), 1.0);
   double crit = std::min(1.0 - as<double>(splinefun_ICU(sum_ICU+sum_Vent+sum_VentC)), 1.0);
   double critV = std::min(1.0 - as<double>(splinefun_Vent(sum_Vent)),1.0);


   int to_keep = 10000000;
   critH = round(critH * to_keep) / to_keep;
   crit  = round(crit  * to_keep) / to_keep;
   critV = round(critV * to_keep) / to_keep;
     
// if (t == (int)t) {
//   Rcout << "t=" << t
//         << std::setprecision(15)
//         << ", " << critH
//         << ", " << crit
//         << ", "  << critV << "\n";

// }

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
    int masking   = as<NumericVector>(input["masking"])[my_t];
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
    // int masstesting = as<NumericVector>(input["masstesting"])[my_t];
    int dexamethasone = as<NumericVector>(input["dex"])[my_t];


    double screen_eff = 0.0;
    double selfis = 0.0;
    double school = 1.0;
    double dist = 1.0;
    double hand = 0.0;
    double mask = 0.0;
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
    double mask_cov =        (as<NumericVector>(input["msk_vector"])[my_t])/100.0;
    double cocoon =          (as<NumericVector>(input["cte_vector"])[my_t])/100.0;
    double work_cov =        (as<NumericVector>(input["wah_vector"])[my_t])/100.0;
    double travelban_eff =   (as<NumericVector>(input["tb_vector"])[my_t])/100.0;
    double vaccine_cov =     (as<NumericVector>(input["vc_vector"])[my_t])/100.0;
    double quarantine_cov =  (as<NumericVector>(input["q_vector"])[my_t])/100.0;
    double tests_per_day =   (as<NumericVector>(input["mt_vector"])[my_t]);
    double min_age_testing = floor((as<NumericVector>(input["minas_vector"])[my_t])/5.0 + 1.0);
    double max_age_testing = floor((as<NumericVector>(input["maxas_vector"])[my_t])/5.0 + 1.0);
    double min_age_vaccine = floor((as<NumericVector>(input["minav_vector"])[my_t])/5.0 + 1.0);
    double max_age_vaccine = floor((as<NumericVector>(input["maxav_vector"])[my_t])/5.0 + 1.0);

    arma::vec age_testing_vector =  arma::join_cols(
                                          arma::vec(min_age_testing-1, arma::fill::zeros),
                                          arma::vec(max_age_testing-min_age_testing+1, arma::fill::ones),
                                          arma::vec(A-max_age_testing, arma::fill::zeros)
                                      );
    
    arma::vec age_vaccine_vector =  arma::vec(A, arma::fill::zeros);
    if(vaccine){
      age_vaccine_vector =  arma::join_cols(
                                  arma::vec(min_age_vaccine-1, arma::fill::zeros),
                                  arma::vec(max_age_vaccine-min_age_vaccine+1, arma::fill::ones),
                                  arma::vec(A-max_age_vaccine, arma::fill::zeros)
                              );
             vac_rate = -log(1.0-vaccine_cov)/vac_campaign;
             vaccinate = vac_rate;
      // if (t < 0.1){
      //   Rcout << "age_vaccine_vector\n" << age_vaccine_vector << "\n";
      //   Rcout << "t\n" << t << "\n";

      // }
    }


           if (workhome){
             work = work_cov*work_eff;
           }else{work = 1.0;}
           if (isolation){
             selfis = selfis_cov;
             if(screen){
               // screen_eff = MIN((report*I + reportc*CL + H+ICU+Vent+ reporth*HC + ICUC+VentC)  *screen_contacts*(screen_overdispersion*I/P)*screen_cov/P,1.0); 
               screen_eff = MIN(sum(report*I + reportc*CL + H+ICU+Vent+ reporth*(HC+ICUC+ICUCV+VentC+HCICU+HCV)) *screen_contacts*(screen_overdispersion*I/P)*screen_test_sens/P,1.0); 
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
           if (masking) {
              mask = mask_eff*mask_cov;
           }
           if(travelban){
             trvban_eff = travelban_eff;
           }
           if(quarantine){
             rate_q = std::min(std::min(sum((I+CL+H+ICU+Vent+HC+ICUC+ICUCV+VentC+HCV+HCICU))*(household_size-1.0)/sum(P),1.0) * quarantine_effort, quarantine_cov/2.0);
             quarantine_rate = rate_q/(1.0+exp(-10.0*(quarantine_cov/2.0-Q)));
           }


           double prob_v;
           double dexo2;
           double dexo2c;
           double dexv;
           double dexvc;

           if (dexamethasone) {
              prob_v = prob_vent*vent_dex;
              dexo2 = pa_dexo2;
              dexo2c = pa_dexo2c;
              dexv = pa_dexv;
              dexvc = pa_dexvc;
           } else {
              prob_v = prob_vent;
              dexo2 = 1.0;
              dexo2c = 1.0;
              dexv = 1.0;
              dexvc = 1.0;
           }


        // testing rates
        double sum_I = sum(I);
        double sum_P = sum(P);
        double sum_CL = sum(CL);
        double sum_E = sum(E);

        double sum_EV  = sum(EV);
        double sum_ER  = sum(ER);
        double sum_EVR = sum(EVR);
        double sum_HC  = sum(HC);
        double sum_HCICU  = sum(HCICU);
        double sum_HCV  = sum(HCV);

        double propI = sum_I/sum_P;
        double propC = sum_CL/sum_P;
        double propE = sum_E/sum_P;

        double propEV  = sum_EV/sum_P;
        double propER  = sum_ER/sum_P;
        double propEVR = sum_EVR/sum_P;
        double propHC  = sum_HC/sum_P;
        double propHCICU = sum_HCICU/sum_P;
        double propHCV = sum_HCV/sum_P;

        double testE = tests_per_day * propE;
        double testEV  = tests_per_day * propEV;
        double testER  = tests_per_day * propER;
        double testEVR = tests_per_day * propEVR;
        double testI = tests_per_day * propI;
        double testC = tests_per_day * propC;

        double testHC = tests_per_day * propHC;
        double testHCICU = tests_per_day * propHCICU;
        double testHCV = tests_per_day * propHCV;

        double ratetestI = 0.0;
        double ratetestC = 0.0;
        double ratetestE = 0.0;

        double ratetestEV = 0.0;
        double ratetestER = 0.0;
        double ratetestEVR = 0.0;
        double ratetestHC = 0.0;
        double ratetestHCICU = 0.0;
        double ratetestHCV = 0.0;


        if(sum_I > 1){
          ratetestI = mass_test_sens * testI / sum_I;
        }
        if(sum_CL > 1){
          ratetestC = mass_test_sens * testC / sum_CL;
        }
        if(sum_E > 1){
          ratetestE = mass_test_sens * testE / sum_E;
        }

        if(sum_EV > 1){
          ratetestEV = mass_test_sens * testEV / sum_EV;
        }
        if(sum_ER > 1){
          ratetestER = mass_test_sens * testER / sum_ER;
        }
        if(sum_EVR > 1){
          ratetestEVR = mass_test_sens * testEVR / sum_EVR;
        }
        if(sum_HC > 1){
          ratetestHC = mass_test_sens * testHC / sum_HC;
        }
        if(sum_HCICU > 1){
          ratetestHCICU = mass_test_sens * testHCICU / sum_HCICU;
        }
        if(sum_HCV > 1){
          ratetestHCV = mass_test_sens * testHCV / sum_HCV;
        }

        
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
        arma::vec HHC = HC+HCICU+HCV;

        // std::cout << "std::max(hand,mask)=" << std::max(hand,mask) << "\n";

        arma::vec lam = (1.0-std::max(hand,mask))*p*seas*(contacts*((rho*E+(I+CL+importation)+(1.0-selfis_eff)*(X+HHC)+rhos*(HH))/P))
                      + (1.0-std::max(hand,mask))*p*seas*(1.0-quarantine*quarantine_eff_other)*(contact_other*((rho*QE+QI+QC+QEV+QEVR+QER)/P));

                // # contacts under home quarantine
        arma::vec lamq = (1.0-std::max(hand,mask))*p*seas*((1.0-quarantine_eff_home)*contact_home*(((1.0-selfis_eff)*(X+HHC+rho*QE+QI+QC+QEV+QEVR+QER))/P))
                       + (1.0-std::max(hand,mask))*p*seas*(1.0-quarantine_eff_other)*(contact_other*((rho*E+(I+CL+importation)+(1.0-selfis_eff)*(X+HHC+rho*QE+QI+QC+QEV+QEVR+QER)+rhos*(HH))/P));
        
        // if (t < 0.4){
        // // Rcout << std::setprecision(15) << "lam:" << lam << "lam[0]:" << lam[0] << "\n";
        // // Rcout << std::setprecision(15) << "lamq:" << lamq << "lamq[0]:" << lamq[0] << "\n";
        // Rcout << std::setprecision(15) << "vaccinate:" << vaccinate << "\n";

        // }

        // # birth/death
        double b1 = sum(popbirth_col2 % popstruc_col2);
        arma::vec birth = 0.0*popbirth_col2;
        birth(0) = b1;

      //# ODE system
      arma::vec dSdt = -S%lam
                      - vaccinate*age_vaccine_vector%S
                      + omega*R
                      + vac_dur*V
                      + ageing*S
                      - mort_col%S
                      + birth
                      - quarantine_rate*S
                      + (1.0/quarantine_days)*QS
                      ;

      arma::vec dEdt = S%lam
                      - gamma*E
                      + ageing*E
                      - vaccinate*age_vaccine_vector%E
                      - mort_col%E
                      - quarantine_rate*E
                      + (1.0/quarantine_days)*QE
                      ;

      arma::vec dIdt =  gamma*(1.0-pclin   )*(1.0-screen_eff)*(1.0-ihr_col2)%(1.0-age_testing_vector*ratetestE)%E
                      + gamma*(1.0-pclin_v )*(1.0-screen_eff)*(1.0-sigmaEV *ihr_col2)%(1.0-age_testing_vector*ratetestEV )%EV
                      + gamma*(1.0-pclin_vr)*(1.0-screen_eff)*(1.0-sigmaEVR*ihr_col2)%(1.0-age_testing_vector*ratetestEVR)%EVR
                      + gamma*(1.0-pclin_r )*(1.0-screen_eff)*(1.0-sigmaER *ihr_col2)%(1.0-age_testing_vector*ratetestER )%ER
                      - vaccinate*age_vaccine_vector%I
                      - nui*I
                      + ageing*I
                      - mort_col%I
                      + (1.0/quarantine_days)*QI
                      - quarantine_rate*I
                      - ratetestI*age_testing_vector%I
                      ;

      arma::vec dCLdt = gamma*pclin*(1.0-age_testing_vector*ratetestE)*(1.0-selfis)%(1.0-ihr_col2)*(1.0-quarantine_rate)%E
                        + gamma*pclin_v *(1.0-age_testing_vector*ratetestEV )*(1.0-selfis)%(1.0-sigmaEV* ihr_col2)*(1.0-quarantine_rate)%EV
                        + gamma*pclin_vr*(1.0-age_testing_vector*ratetestEVR)*(1.0-selfis)%(1.0-sigmaEVR*ihr_col2)*(1.0-quarantine_rate)%EVR
                        + gamma*pclin_r *(1.0-age_testing_vector*ratetestER )*(1.0-selfis)%(1.0-sigmaER* ihr_col2)*(1.0-quarantine_rate)%ER
                        - nui*CL+ageing*CL
                        - mort_col%CL
                        + (1.0/quarantine_days)*QC
                        - ratetestC*age_testing_vector%CL
                        ;

      arma::vec dRdt = nui*I
                        - omega*R
                        + nui*X
                        + nui*CL
                        + ageing*R
                        - mort_col%R
                        + (1.0/isolation_days)*Z
                        + (1.0/quarantine_days)*QR
                        + nus*propo2*(1.0-dexo2*pdeath_ho)*ifr_col2%H
                        + nus*(1.0-propo2)*(1.0-pdeath_h)*ifr_col2%H
                        + nusc*propo2*(1.0-pdeath_hco)*ifr_col2%HC
                        + nusc*(1.0-propo2)*(1.0-pdeath_hc)*ifr_col2%HC
                        + nu_icu*propo2*(1.0-dexo2*pdeath_icuo)*ifr_col2%ICU
                        + nu_icu*(1.0-propo2)*(1.0-pdeath_icu)*ifr_col2%ICU
                        + nu_icuc*propo2*(1.0-dexo2c*pdeath_icuco)*ifr_col2%ICUC
                        + nu_icuc*(1.0-propo2)*(1.0-pdeath_icuc)*ifr_col2%ICUC
                        + nu_vent*(1.0-dexv*pdeath_vent)*ifr_col2%Vent
                        + nu_ventc*(1.0-dexvc*pdeath_ventc)*ifr_col2%VentC
                        + nu_ventc*(1.0-dexvc*pdeath_ventc)*ifr_col2%ICUCV
                        + vac_dur_r*VR
                        - vaccinate*age_vaccine_vector%R
                        - lam*sigmaR%R
                        - quarantine_rate*R
                        + nusc*propo2*(1.0-pdeath_icu_hco)*ifr_col2%HCICU
                        + nusc*(1.0-propo2)*(1.0-pdeath_icu_hc)*ifr_col2%HCICU
                        + nu_ventc*(1.0-pdeath_vent_hc)*ifr_col2%HCV
                        ;

      arma::vec dXdt = (gamma*selfis*(1.0-age_testing_vector*ratetestE)*pclin%(1.0-ihr_col2))%E
                        + (gamma*(1.0-pclin)*(1.0-age_testing_vector*ratetestE)*screen_eff%(1.0-ihr_col2))%E
                        - nui*X
                        + ageing*X
                        - mort_col%X
                        + gamma*selfis        *(1.0-age_testing_vector*ratetestEV) *pclin_v   %(1.0-sigmaEV*ihr_col2 )%EV
                        + gamma*(1.0-pclin_v) *(1.0-age_testing_vector*ratetestEV) *screen_eff%(1.0-sigmaEV*ihr_col2 )%EV
                        + gamma*selfis        *(1.0-age_testing_vector*ratetestEVR)*pclin_v   %(1.0-sigmaEVR*ihr_col2)%EVR
                        + gamma*(1.0-pclin_vr)*(1.0-age_testing_vector*ratetestEVR)*screen_eff%(1.0-sigmaEVR*ihr_col2)%EVR
                        + gamma*selfis        *(1.0-age_testing_vector*ratetestER) *pclin_r   %(1.0-sigmaER*ihr_col2 )%ER
                        + gamma*(1.0-pclin_r) *(1.0-age_testing_vector*ratetestER) *screen_eff%(1.0-sigmaER*ihr_col2 )%ER
                        ;

      arma::vec dVdt = vaccinate*age_vaccine_vector%S
                        - ((1.0-vaccine_eff)*lam)%V
                        + ageing*V
                        - mort_col%V
                        + omega*VR
                        - vac_dur*V
                        - quarantine_rate*V
                        ;
      arma::vec dEVdt = (1.0-vaccine_eff)*lam%V
                        - gamma*EV
                        + ageing*EV
                        - mort_col%EV
                        - quarantine_rate*EV
                        + (1.0/quarantine_days)*QEV
                        ;
      arma::vec dERdt = lam*sigmaR%R
                        - gamma*ER
                        + ageing*ER
                        - mort_col%ER
                        - quarantine_rate*ER
                        + (1.0/quarantine_days)*QER
                        ;
      arma::vec dVRdt = vaccinate*age_vaccine_vector%E
                        + vaccinate*age_vaccine_vector%I
                        + vaccinate*age_vaccine_vector%R
                        - (1.0-vaccine_eff_r)*lam%VR
                        - vac_dur_r*VR
                        + ageing*VR
                        - mort_col%VR
                        - omega*VR
                        - quarantine_rate*VR
                        + (1.0/quarantine_days)*QVR
                        ;
      arma::vec dEVRdt= (1.0-vaccine_eff_r)*lam%VR
                        - gamma*EVR
                        + ageing*EVR
                        - mort_col%EVR
                        - quarantine_rate*EVR
                        + (1.0/quarantine_days)*QEVR
                        ;

      arma::vec dQSdt = quarantine_rate*S+ ageing*QS-mort_col%QS - (1.0/quarantine_days)*QS - lamq%QS;

      arma::vec dQEdt = quarantine_rate*E - gamma*QE + ageing*QE-mort_col%QE - (1.0/quarantine_days)*QE + lamq%QS; 

      arma::vec dQIdt = quarantine_rate*I
                        + (gamma*(1.0-ihr_col2)*(1.0-pclin))%QE
                        - nui*QI
                        + ageing*QI
                        - mort_col%QI
                        - (1.0/quarantine_days)*QI
                        + gamma*(1.0-sigmaEV*ihr_col2 )*(1.0-pclin_v )%QEV
                        + gamma*(1.0-sigmaER*ihr_col2 )*(1.0-pclin_r )%QER     
                        + gamma*(1.0-sigmaEVR*ihr_col2)*(1.0-pclin_vr)%QEVR
                        ;

      arma::vec dQCdt = (gamma*pclin*(1.0-selfis)*(1.0-age_testing_vector*ratetestE)%(1.0-ihr_col2))*quarantine_rate%E
                        + (gamma*(1.0-ihr_col2))%(pclin*QE)
                        - nui*QC
                        + ageing*QC-mort_col%QC
                        - (1.0/quarantine_days)*QC
                        + gamma*pclin_v *(1.0-age_testing_vector*ratetestEV) *(1.0-selfis)%(1.0-sigmaEV *ihr_col2)*quarantine_rate%EV
                        + gamma*pclin_vr*(1.0-age_testing_vector*ratetestEVR)*(1.0-selfis)%(1.0-sigmaEVR*ihr_col2)*quarantine_rate%EVR
                        + gamma*pclin_r *(1.0-age_testing_vector*ratetestER) *(1.0-selfis)%(1.0-sigmaER *ihr_col2)*quarantine_rate%ER
                        + gamma*(1.0-sigmaEV* ihr_col2)*pclin_v %QEV
                        + gamma*(1.0-sigmaER* ihr_col2)*pclin_r %QER
                        + gamma*(1.0-sigmaEVR*ihr_col2)*pclin_vr%QEVR
                        ;

      arma::vec dQRdt = nui*QI
                      + nui*QC
                      + ageing*QR
                      - mort_col%QR
                      - (1.0/quarantine_days)*QR
                      + quarantine_rate*R
                      + vac_dur_r*QVR
                      ;

      arma::vec dQVdt = quarantine_rate*V
                      + ageing*QV
                      - mort_col%QV
                      - (1.0/quarantine_days)*QV
                      - (1.0-vaccine_eff)*lamq%QV
                      + omega*QVR
                      ;

      arma::vec dQEVdt = quarantine_rate*EV
                        - gamma*QEV
                        + ageing*QEV
                        - mort_col%QEV
                        - (1.0/quarantine_days)*QEV
                        + (1.0-vaccine_eff)*lamq%QV
                        ;

      arma::vec dQERdt = quarantine_rate*ER
                        - gamma*QER
                        + ageing*QER
                        - mort_col%QER
                        - (1.0/quarantine_days)*QER
                        + sigmaR*lamq%QR 
                        ;

      arma::vec dQVRdt = quarantine_rate*VR
                        - (1.0-vaccine_eff_r)*lam%QVR
                        - vac_dur_r*QVR
                        - omega*QVR
                        + ageing*QVR
                        - mort_col%QVR 
                        ;

      arma::vec dQEVRdt = quarantine_rate*EVR
                        - gamma*QEVR
                        + ageing*QEVR
                        - mort_col%QEVR
                        - (1.0/quarantine_days)*QEVR
                        + (1.0-vaccine_eff_r)*lamq%QVR 
                        ;

      arma::vec dHdt = gamma*          ihr_col2*(1.0-prob_icu)*(1.0-critH)*reporth%E
                      - nus*H
                      + ageing*H
                      - mort_col%H
                      + gamma*         ihr_col2*(1.0-prob_icu)*(1.0-critH)*reporth%QE
                      + gamma*sigmaEV *ihr_col2*(1.0-prob_icu_v) *(1.0-critH)*reporth%EV
                      + gamma*sigmaEVR*ihr_col2*(1.0-prob_icu_vr)*(1.0-critH)*reporth%EVR
                      + gamma*sigmaER *ihr_col2*(1.0-prob_icu_r) *(1.0-critH)*reporth%ER
                      + gamma*sigmaEV *ihr_col2*(1.0-prob_icu_v) *(1.0-critH)*reporth%QEV
                      + gamma*sigmaEVR*ihr_col2*(1.0-prob_icu_vr)*(1.0-critH)*reporth%QEVR
                      + gamma*sigmaER *ihr_col2*(1.0-prob_icu_r) *(1.0-critH)*reporth%QER
                      ; //  # all pdeath have to be lower than


      arma::vec dHCdt = gamma*ihr_col2*(1.0-prob_icu)*(1.0-reporth)%E
                      + gamma*ihr_col2*(1.0-prob_icu)*critH*reporth%E
                      + gamma*ihr_col2*(1.0-prob_icu)*(1.0-reporth)%QE
                      + gamma*ihr_col2*(1.0-prob_icu)*critH*reporth%QE
                      - nusc*HC
                      + ageing*HC
                      - mort_col%HC
                      + gamma*sigmaEV *ihr_col2*(1.0-prob_icu_v )*(1.0-reporth)%EV
                      + gamma*sigmaEV *ihr_col2*(1.0-prob_icu_v )*critH*reporth%EV
                      + gamma*sigmaEVR*ihr_col2*(1.0-prob_icu_vr)*(1.0-reporth)%EVR
                      + gamma*sigmaEVR*ihr_col2*(1.0-prob_icu_vr)*critH*reporth%EVR
                      + gamma*sigmaER *ihr_col2*(1.0-prob_icu_r )*(1.0-reporth)%ER
                      + gamma*sigmaER *ihr_col2*(1.0-prob_icu_r )*critH*reporth%ER 
                      + gamma*sigmaEV *ihr_col2*(1.0-prob_icu_v )*(1.0-reporth)%QEV
                      + gamma*sigmaEV *ihr_col2*(1.0-prob_icu_v )*critH*reporth%QEV
                      + gamma*sigmaEVR*ihr_col2*(1.0-prob_icu_vr)*(1.0-reporth)%QEVR
                      + gamma*sigmaEVR*ihr_col2*(1.0-prob_icu_vr)*critH*reporth%QEVR
                      + gamma*sigmaER *ihr_col2*(1.0-prob_icu_r )*(1.0-reporth)%QER
                      + gamma*sigmaER *ihr_col2*(1.0-prob_icu_r )*critH*reporth%QER
                      - ratetestHC*age_testing_vector%HC
                      ;

      arma::vec dHCICUdt =  gamma*(1.0-reporth_ICU)         *ihr_col2*prob_icu   *(1.0-prob_v)   %E
                          + gamma*(1.0-reporth_ICU)*sigmaEV *ihr_col2*prob_icu_v *(1.0-prob_v_v) %EV
                          + gamma*(1.0-reporth_ICU)*sigmaEVR*ihr_col2*prob_icu_vr*(1.0-prob_v_vr)%EVR
                          + gamma*(1.0-reporth_ICU)*sigmaER *ihr_col2*prob_icu_r *(1.0-prob_v_r) %ER
                          + gamma*(1.0-reporth_ICU)         *ihr_col2*prob_icu   *(1.0-prob_v)   %QE
                          + gamma*(1.0-reporth_ICU)*sigmaEV *ihr_col2*prob_icu_v *(1.0-prob_v_v) %QEV
                          + gamma*(1.0-reporth_ICU)*sigmaEVR*ihr_col2*prob_icu_vr*(1.0-prob_v_vr)%QEVR
                          + gamma*(1.0-reporth_ICU)*sigmaER *ihr_col2*prob_icu_r *(1.0-prob_v_r) %QER
                          - nusc*HCICU
                          + ageing*HCICU
                          - mort_col%HCICU
                          - ratetestHCICU*age_testing_vector%HCICU
                          ;

      arma::vec dHCVdt =  gamma*(1.0-reporth_ICU)         *ihr_col2*prob_icu   *prob_v   %E
                        + gamma*(1.0-reporth_ICU)*sigmaEV *ihr_col2*prob_icu_v *prob_v_v %EV
                        + gamma*(1.0-reporth_ICU)*sigmaEVR*ihr_col2*prob_icu_vr*prob_v_vr%EVR
                        + gamma*(1.0-reporth_ICU)*sigmaER *ihr_col2*prob_icu_r *prob_v_r %ER
                        + gamma*(1.0-reporth_ICU)         *ihr_col2*prob_icu   *prob_v   %QE
                        + gamma*(1.0-reporth_ICU)*sigmaEV *ihr_col2*prob_icu_v *prob_v_v %QEV
                        + gamma*(1.0-reporth_ICU)*sigmaEVR*ihr_col2*prob_icu_vr*prob_v_vr%QEVR
                        + gamma*(1.0-reporth_ICU)*sigmaER *ihr_col2*prob_icu_r *prob_v_r %QER
                        - nu_ventc*HCV
                        + ageing*HCV
                        - mort_col%HCV
                        - ratetestHCV*age_testing_vector%HCV
                        ;

      arma::vec dICUdt =  gamma*reporth_ICU*ihr_col2*prob_icu*(1.0-crit)*(1.0-prob_v)%E
                        + gamma*reporth_ICU*ihr_col2*prob_icu*(1.0-crit)*(1.0-prob_v)%QE
                        + gamma*reporth_ICU*sigmaEV* ihr_col2*prob_icu_v *(1.0-crit)*(1.0-prob_v_v) %EV
                        + gamma*reporth_ICU*sigmaEVR*ihr_col2*prob_icu_vr*(1.0-crit)*(1.0-prob_v_vr)%EVR
                        + gamma*reporth_ICU*sigmaER* ihr_col2*prob_icu_r *(1.0-crit)*(1.0-prob_v_r) %ER
                        + gamma*reporth_ICU*sigmaEV* ihr_col2*prob_icu_v *(1.0-crit)*(1.0-prob_v_v) %QEV
                        + gamma*reporth_ICU*sigmaEVR*ihr_col2*prob_icu_vr*(1.0-crit)*(1.0-prob_v_vr)%QEVR
                        + gamma*reporth_ICU*sigmaER* ihr_col2*prob_icu_r *(1.0-crit)*(1.0-prob_v_r) %QER
                        - nu_icu*ICU
                        + ageing*ICU
                        - mort_col%ICU
                        + (1.0-crit)*ICUC*1.0/2.0
                        ;

      arma::vec dICUCdt =   gamma*reporth_ICU*ihr_col2*prob_icu*crit*(1.0-prob_v)%E
                          + gamma*reporth_ICU*ihr_col2*prob_icu*crit*(1.0-prob_v)%QE
                          + gamma*reporth_ICU*sigmaEV *ihr_col2*prob_icu_v *crit*(1.0-prob_v_v) %EV
                          + gamma*reporth_ICU*sigmaEVR*ihr_col2*prob_icu_vr*crit*(1.0-prob_v_vr)%EVR
                          + gamma*reporth_ICU*sigmaER *ihr_col2*prob_icu_r *crit*(1.0-prob_v_r) %ER
                          + gamma*reporth_ICU*sigmaEV *ihr_col2*prob_icu_v *crit*(1.0-prob_v_v) %QEV
                          + gamma*reporth_ICU*sigmaEVR*ihr_col2*prob_icu_vr*crit*(1.0-prob_v_vr)%QEVR
                          + gamma*reporth_ICU*sigmaER *ihr_col2*prob_icu_r *crit*(1.0-prob_v_r) %QER
                          - nu_icuc*ICUC
                          - (1.0-crit)*ICUC*1.0/2.0
                          + ageing*ICUC
                          - mort_col%ICUC
                          ;

      arma::vec dICUCVdt = gamma*reporth_ICU*ihr_col2*prob_icu*prob_v*crit%E
                          + gamma*reporth_ICU*ihr_col2*prob_icu*prob_v*crit%QE
                          - nu_ventc*ICUCV
                          + ageing*ICUCV
                          - mort_col%ICUCV
                          - (1.0-critV)*ICUCV*1.0/2.0
                          + gamma*reporth_ICU*sigmaEV *ihr_col2*prob_icu_v *prob_v_v *crit%EV
                          + gamma*reporth_ICU*sigmaEVR*ihr_col2*prob_icu_vr*prob_v_vr*crit%EVR
                          + gamma*reporth_ICU*sigmaER *ihr_col2*prob_icu_r *prob_v_r *crit%ER
                          + gamma*reporth_ICU*sigmaEV *ihr_col2*prob_icu_v *prob_v_v *crit%QEV
                          + gamma*reporth_ICU*sigmaEVR*ihr_col2*prob_icu_vr*prob_v_vr*crit%QEVR
                          + gamma*reporth_ICU*sigmaER *ihr_col2*prob_icu_r *prob_v_r *crit%QER
                          ;

      arma::vec dVentdt =   gamma*reporth_ICU*ihr_col2*prob_icu*(1.0-crit)*(1.0-critV)*prob_v%E
                          + gamma*reporth_ICU*ihr_col2*prob_icu*(1.0-crit)*(1.0-critV)*prob_v%QE
                          + (1.0-critV)*VentC*1.0/2.0 
                          + (1.0-critV)*ICUCV*1.0/2.0
                          - nu_vent*Vent
                          + ageing*Vent
                          - mort_col%Vent
                          + gamma*reporth_ICU*sigmaEV *ihr_col2*prob_icu_v *(1.0-crit)*(1.0-critV)*prob_v_v %EV
                          + gamma*reporth_ICU*sigmaEVR*ihr_col2*prob_icu_vr*(1.0-crit)*(1.0-critV)*prob_v_vr%EVR
                          + gamma*reporth_ICU*sigmaER *ihr_col2*prob_icu_r *(1.0-crit)*(1.0-critV)*prob_v_r %ER
                          + gamma*reporth_ICU*sigmaEV *ihr_col2*prob_icu_v *(1.0-crit)*(1.0-critV)*prob_v_v %QEV
                          + gamma*reporth_ICU*sigmaEVR*ihr_col2*prob_icu_vr*(1.0-crit)*(1.0-critV)*prob_v_vr%QEVR
                          + gamma*reporth_ICU*sigmaER *ihr_col2*prob_icu_r *(1.0-crit)*(1.0-critV)*prob_v_r %QER 
                          ;

      arma::vec dVentCdt =  gamma*reporth_ICU*ihr_col2*prob_icu*prob_v*(1.0-crit)*critV%E
                          + gamma*reporth_ICU*ihr_col2*prob_icu*prob_v*(1.0-crit)*critV%QE
                          - (1.0-critV)*VentC*1.0/2.0
                          - nu_ventc*VentC
                          + ageing*VentC
                          - mort_col%VentC
                          + gamma*reporth_ICU*sigmaEV *ihr_col2*prob_icu_v *prob_v_v* (1.0-crit)*critV%EV
                          + gamma*reporth_ICU*sigmaEVR*ihr_col2*prob_icu_vr*prob_v_vr*(1.0-crit)*critV%EVR
                          + gamma*reporth_ICU*sigmaER *ihr_col2*prob_icu_r *prob_v_r* (1.0-crit)*critV%ER
                          + gamma*reporth_ICU*sigmaEV *ihr_col2*prob_icu_v *prob_v_v* (1.0-crit)*critV%QEV
                          + gamma*reporth_ICU*sigmaEVR*ihr_col2*prob_icu_vr*prob_v_vr*(1.0-crit)*critV%QEVR
                          + gamma*reporth_ICU*sigmaER *ihr_col2*prob_icu_r *prob_v_r* (1.0-crit)*critV%QER
                          ;



      arma::vec dCdt =  report *gamma      *(1.0-age_testing_vector*ratetestE)*(1.0-pclin)%(1.0-ihr_col2)%(E+QE)
                      + reportc*gamma*pclin*(1.0-age_testing_vector*ratetestE)            %(1.0-ihr_col2)%(E+QE)
                      + gamma*ihr_col2*(1.0-critH)  *(1.0-prob_icu)%(E+QE)
                      + gamma*ihr_col2*critH*reporth*(1.0-prob_icu)%(E+QE)
                      + gamma*ihr_col2*prob_icu%(E+QE)
                      + ratetestI*age_testing_vector%I
                      + ratetestC*age_testing_vector%CL
                      + gamma*age_testing_vector*ratetestE%(1.0-ihr_col2)%E
                      ;
      
      arma::vec dCMdt = 
                        nus*propo2*dexo2*pdeath_ho*ifr_col2%H
                        + nus*(1.0-propo2)*pdeath_h*ifr_col2%H
                        + nusc*report_death_HC     *propo2 *pdeath_hco*ifr_col2%HC
                        + nusc*report_death_HC*(1.0-propo2)*pdeath_hc *ifr_col2%HC
                        + nu_icu*propo2*dexo2*pdeath_icuo*ifr_col2%ICU
                        + nu_icu*(1.0-propo2)*pdeath_icu*ifr_col2%ICU
                        + nu_icuc*propo2*dexo2c*pdeath_icuco*ifr_col2%ICUC
                        + nu_icuc*(1.0-propo2)*pdeath_icuc*ifr_col2%ICUC
                        + nu_vent*dexv*pdeath_vent*ifr_col2%Vent
                        + nu_ventc*dexvc*pdeath_ventc*ifr_col2%VentC
                        + nu_ventc*dexvc*pdeath_ventc*ifr_col2%ICUCV
                        
                        + nu_ventc           *pdeath_vent_hc*ifr_col2%HCV
                        + nu_icuc *propo2    *pdeath_icu_hco*ifr_col2%HCICU
                        + nu_icuc *(1.0-propo2)*pdeath_icu_hc *ifr_col2%HCICU // all above are attributable deaths

                        + mort_col%H
                        + mort_col%ICU
                        + mort_col%ICUC
                        + mort_col%ICUCV
                        + mort_col%Vent
                        + mort_col%VentC
                        + mort_col%Z // mass tested infected
                        + report_death_HC  *mort_col%HC
                        + report_death_HC  *mort_col%HCICU
                        + report_death_HC  *mort_col%HCV
                        + report_natdeathI *mort_col%I
                        + report_natdeathI *mort_col%QI
                        + report_natdeathI *mort_col%E
                        + report_natdeathI *mort_col%QE
                        + report_natdeathI *mort_col%EV
                        + report_natdeathI *mort_col%EVR
                        + report_natdeathI *mort_col%ER
                        + report_natdeathI *mort_col%QEV
                        + report_natdeathI *mort_col%QEVR
                        + report_natdeathI *mort_col%QER
                        + report_natdeathCL*mort_col%CL
                        + report_natdeathCL*mort_col%QC
                        + report_natdeathCL*mort_col%X  
                        ;


// reportable = attributable + mort_col%(!not reportable)

//attributable = coloured
//mort_col%(!not reportable) lighter grey

//mort_col%(not reportable) darker grey, S, E, QS, QE, R, QR, V


      arma::vec dCMCdt = nusc*propo2*pdeath_hco*ifr_col2%HC
                          + nusc*(1.0-propo2)*pdeath_hc*ifr_col2%HC
                          + nu_icuc*propo2*dexo2c*pdeath_icuco*ifr_col2%ICUC
                          + nu_icuc*(1.0-propo2)*pdeath_icuc*ifr_col2%ICUC
                          + nu_ventc*dexvc*pdeath_ventc*ifr_col2%VentC
                          + nu_ventc*dexvc*pdeath_ventc*ifr_col2%ICUCV
                          + mort_col%HC + mort_col%ICUC + mort_col%VentC + mort_col%ICUCV
                          ;

      arma::vec dZdt = gamma*ratetestE*age_testing_vector%(1.0-ihr_col2)%E
                        + ratetestI*age_testing_vector%I
                        + ratetestC*age_testing_vector%CL
                        - (1.0/isolation_days)*Z
                        - mort_col%Z
                        + gamma*(1.0-ihr_col2)*ratetestEV %age_testing_vector%EV
                        + gamma*(1.0-ihr_col2)*ratetestEVR%age_testing_vector%EVR
                        + gamma*(1.0-ihr_col2)*ratetestER %age_testing_vector%ER
                        + ratetestHC   *age_testing_vector%HC
                        + ratetestHCICU*age_testing_vector%HCICU
                        + ratetestHCV  *age_testing_vector%HCV
                        ;
//--------------------------

      arma::vec dAbdt = nui*I
                      + nui*X
                      + nui*CL 
                      + nus*      propo2 *(1.0-dexo2*pdeath_ho)*ifr_col2%H
                      + nus*(1.0- propo2)*(1.0-pdeath_h)       *ifr_col2%H
                      + nusc*     propo2 *(1.0-pdeath_hco)    *ifr_col2%HC
                      + nusc*(1.0-propo2)*(1.0-pdeath_hc )    *ifr_col2%HC  
                      + nusc*     propo2 *(1.0-pdeath_icu_hco)*ifr_col2%HCICU
                      + nusc*(1.0-propo2)*(1.0-pdeath_icu_hc )*ifr_col2%HCICU
                      + nu_ventc         *(1.0-pdeath_vent_hc)*ifr_col2%HCV
                      + nu_icu*   propo2 *(1.0-dexo2*pdeath_icuo)   *ifr_col2%ICU
                      + nu_icu*(1.0-propo2)*(1.0-pdeath_icu)        *ifr_col2%ICU
                      + nu_icuc*   propo2 *(1.0-dexo2c*pdeath_icuco)*ifr_col2%ICUC
                      + nu_icuc*(1.0-propo2)*(1.0-pdeath_icuc)      *ifr_col2%ICUC
                      + nu_vent* (1.0-dexv *pdeath_vent) *ifr_col2%Vent
                      + nu_ventc*(1.0-dexvc*pdeath_ventc)*ifr_col2%VentC
                      + nu_ventc*(1.0-dexvc*pdeath_ventc)*ifr_col2%ICUCV
                      - seroneg*Ab
                      - mort_col%Ab
                      + ageing*Ab
                      ;

//--------------------------

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
  outvec = arma::join_cols(outvec,dZdt);

  outvec = arma::join_cols(outvec,dEVdt);
  outvec = arma::join_cols(outvec,dERdt);
  outvec = arma::join_cols(outvec,dEVRdt);
  outvec = arma::join_cols(outvec,dVRdt);

  outvec = arma::join_cols(outvec,dQVdt);
  outvec = arma::join_cols(outvec,dQEVdt);
  outvec = arma::join_cols(outvec,dQEVRdt);
  outvec = arma::join_cols(outvec,dQERdt);
  outvec = arma::join_cols(outvec,dQVRdt);
  outvec = arma::join_cols(outvec,dHCICUdt);
  outvec = arma::join_cols(outvec,dHCVdt);
  outvec = arma::join_cols(outvec,dAbdt);


  List output(1);
  output[0] = NumericVector(outvec.begin(),outvec.end());

// duration_c += high_resolution_clock::now()-start_c;

  return output;
}
