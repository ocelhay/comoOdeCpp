// comomodel.net
// c++ version by sompob@tropmedre.ac
//
// for using with deSolve package in R
#include <Rmath.h>
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

arma::mat getVarmat(std::string m){
  Environment glob = Environment::global_env();
  arma::mat var = as<arma::mat>(glob[m]);
  return(var);
}


double SplineFun(NumericVector a, NumericVector b, double c){
    Function splinefun("splinefun");
    Function fn = splinefun(a,b, Named("method")="hyman");
    double out = as<double>(fn(c));
    return(out);
}


double MIN(const arma::vec& a, double m){
  arma::vec b = a;
  int sz = b.size();
  b.resize(sz + 1);
  b(sz) = m;
  return(min(b));
}

arma::vec THpopstruct = {3596052.0, 3843780.0, 4113805.0, 4378506.0, 4807904.0, 
4822404.0, 4466694.0, 4763033.0, 5308840.0, 5605417.0, 5598953.0, 5082441.0, 4367653.0,
 3256703.0, 2282338.0, 1584230.0, 1070912.0, 563683.0, 225198.0, 52666.0, 8766.0};

arma::vec THpopbirth = {
0.000000e+00, 0.000000e+00, 0.000000e+00, 6.303758e-05, 1.025998e-04, 1.056259e-04, 
9.602491e-05, 4.421848e-05, 1.079289e-05, 1.483263e-06, 0.000000e+00, 0.000000e+00,
0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00, 
0.000000e+00, 0.000000e+00, 0.000000e+00};


arma::vec ihr = {0.0000, 0.0000, 0.0004, 0.0004, 0.0110, 0.0110, 0.0340, 0.0340, 0.0430,
                 0.0430, 0.0820, 0.0820, 0.1180, 0.1180, 0.1660, 0.1660, 0.1840, 0.1840, 
                 0.1840, 0.1840, 0.1840};

arma::vec ifr = {0.0002051282, 0.0002051282, 0.0008974359, 0.0008974359, 0.0039743590, 0.0039743590, 0.0107692308, 0.0107692308, 0.0205128205, 0.0205128205,
                 0.0769230769, 0.0769230769, 0.2435897436, 0.2435897436, 0.5512820513, 0.5512820513, 1.0000000000, 1.0000000000, 1.0000000000, 1.0000000000,
                 1.0000000000};

arma::vec mort = {
  5.005109e-06, 7.520261e-07, 1.168267e-06, 3.099954e-06, 2.834712e-06, 2.994124e-06, 4.586444e-06, 7.165038e-06, 9.579105e-06, 1.221015e-05,
  1.548603e-05, 2.040141e-05, 2.819007e-05, 3.913962e-05, 6.372658e-05, 1.085628e-04, 1.678622e-04, 2.619392e-04, 3.626299e-04, 6.443249e-04,
  3.871095e-03};  
  
// [[Rcpp::export]]
List covidTH(double t, arma::vec y, List parameters){
    
    //Rf_PrintValue(parameters);
  
    double p = parameters["p"];
    double rho = parameters["rho"];
    double omega = parameters["omega"];
    double gamma = parameters["gamma"];
    double nui = parameters["nui"];
    double report = parameters["report"];
    double reportc = parameters["reportc"];
    double reporth = parameters["reporth"];
    double beds_available = parameters["beds_available"];
    double icu_beds_available = parameters["icu_beds_available"];
    double ventilators_available = parameters["ventilators_available"];
    double nus = parameters["nus"];
    double rhos = parameters["rhos"];
    double amp = parameters["amp"];
    double phi = parameters["phi"];
    double ihr_scaling = parameters["ihr_scaling"];
    double give = parameters["give"];
    double pdeath_h = parameters["pdeath_h"];
    double pdeath_hc = parameters["pdeath_hc"];
    double pdeath_icu = parameters["pdeath_icu"];
    double pdeath_icuc = parameters["pdeath_icuc"];
    double pdeath_vent = parameters["pdeath_vent"];
    double pdeath_ventc = parameters["pdeath_ventc"];
    double nusc = parameters["nusc"];
    double nu_icu = parameters["nu_icu"];
    double nu_icuc = parameters["nu_icuc"];
    double nu_vent = parameters["nu_vent"];
    double nu_ventc = parameters["nu_ventc"];
    double pclin = parameters["pclin"];
    double prob_icu = parameters["prob_icu"];
    double prob_vent = parameters["prob_vent"];
    
    //interventions
    // self isolation
    double selfis_on = parameters["selfis_on"];
    double selfis_dur = parameters["selfis_dur"];
    double selfis_cov = parameters["selfis_cov"];
    double selfis_eff = parameters["selfis_eff"];
    // social distance
    double dist_on = parameters["dist_on"];
    double dist_dur = parameters["dist_dur"];
    double dist_cov = parameters["dist_cov"];
    double dist_eff = parameters["dist_eff"];
    // hand washing
    double hand_on = parameters["hand_on"];
    double hand_dur = parameters["hand_dur"];
    double hand_eff = parameters["hand_eff"];
    // working at home
    double work_on = parameters["work_on"];
    double work_dur = parameters["work_dur"];
    double work_cov = parameters["work_cov"];
    double work_eff = parameters["work_eff"];
    double w2h = parameters["w2h"];
    // school closures
    double school_on = parameters["school_on"];
    double school_dur = parameters["school_dur"];
    double schoolcov = parameters["schoolcov"];
    double school_eff = parameters["school_eff"];
    double s2h = parameters["s2h"];
    // cocooning the elderly
    double cocoon_on = parameters["cocoon_on"];
    double cocoon_dur = parameters["cocoon_dur"];
    double cocoon_cov = parameters["cocoon_cov"];
    double cocoon_eff = parameters["cocoon_eff"];
    double age_cocoon = parameters["age_cocoon"];
    // vaccination
    double vaccine_on = parameters["vaccine_on"];
    double vaccine_eff = parameters["vaccine_eff"];
    double vaccine_cov = parameters["vaccine_cov"];
    double vac_campaign = parameters["vac_campaign"];
    // imported cases
    double mean_imports = parameters["mean_imports"];
    double travelban_on = parameters["travelban_on"];
    double travelban_eff = parameters["travelban_eff"];
    double travelban_dur = parameters["travelban_dur"];
    // screening 
    double screen_on = parameters["screen_on"];
    double screen_dur = parameters["screen_dur"];
    double screen_cov = parameters["screen_cov"];
    double screen_overdispersion = parameters["screen_overdispersion"];
    double screen_contacts = parameters["screen_contacts"];
    // quarantine
    double quarantine_on = parameters["quarantine_on"];
    double quarantine_cov = parameters["quarantine_cov"];
    double quarantine_dur = parameters["quarantine_dur"];
    double quarantine_days = parameters["quarantine_days"];
    double quarantine_effort = parameters["quarantine_effort"];
    double quarantine_eff_home = parameters["quarantine_eff_home"];
    double quarantine_eff_other = parameters["quarantine_eff_other"];
    // lockdown
    double lockdown_low_on = parameters["lockdown_low_on"];
    double lockdown_low_dur = parameters["lockdown_low_dur"];
    double lockdown_mid_on = parameters["lockdown_mid_on"];
    double lockdown_mid_dur = parameters["lockdown_mid_dur"];
    double lockdown_high_on = parameters["lockdown_high_on"];
    double lockdown_high_dur = parameters["lockdown_high_dur"];
    // mean household size
    double household_size = parameters["household_size"];
    
      
  int A = 21; 

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
  arma::vec Vent = y.subvec(18*A,19*A-1);
  arma::vec VentC = y.subvec(19*A,20*A-1);
  arma::vec CMC = y.subvec(20*A,21*A-1);

  arma::vec P = (S+E+I+R+X+V+H+HC+QS+QE+QI+QR+CL+QC+ICU+ICUC+Vent+VentC);
         

// health system performance
   NumericVector f = {1,(1+give)/2,(1-give)/2,0};
   double KH  = beds_available;
   double KICU = icu_beds_available;
   double Kvent = ventilators_available;
   NumericVector  xH = {0,(1+give)*KH/2,(3-give)*KH/2,2*KH};
   NumericVector  xICU = {0,(1+give)*KICU/2,(3-give)*KICU/2,2*KICU};
   NumericVector  xVent = {0,(1+give)*Kvent/2,(3-give)*Kvent/2,2*Kvent};
   
   double critH = std::min(1.0 - SplineFun(xH, f,sum(H)+sum(ICUC)) + (1.0 - reporth),1.0);
   double crit = std::min(1.0-SplineFun(xICU, f,sum(ICU)+sum(Vent)+sum(VentC)),1.0);
   double critV = std::min(1.0-SplineFun(xVent, f,sum(Vent)),1.0);

     
// interventions
    int isolation = (t>=selfis_on)*(t<=selfis_on+selfis_dur);
    int distancing = (t>=dist_on)*(t<=(dist_on+dist_dur));
    int handwash = (t>=hand_on)*(t<=(hand_on+hand_dur));
    int workhome = (t>=work_on)*(t<=(work_on+work_dur));
    int schoolclose = (t>=school_on)*(t<=(school_on+school_dur));
    int cocoon = (t>=cocoon_on)*(t<=(cocoon_on+cocoon_dur))*cocoon_cov;
    int vaccine = (t>=(vaccine_on))*(t<=vaccine_on+vac_campaign);
    int travelban = (t>=travelban_on)*(t<=(travelban_on+travelban_dur));
    int screen = (t>=screen_on)*(t<=(screen_on+screen_dur));
    int quarantine = (t>=quarantine_on)*(t<=(quarantine_on+quarantine_dur));
    int lockdown_low = (t>=lockdown_low_on)*(t<=(lockdown_low_on+lockdown_low_dur));
    int lockdown_mid = (t>=lockdown_mid_on)*(t<=(lockdown_mid_on+lockdown_mid_dur));
    int lockdown_high = (t>=lockdown_high_on)*(t<=(lockdown_high_on+lockdown_high_dur));

    double screen_eff = 0.0;
    double selfis = 0.0;
    double school = 1.0;
    double dist = 1.0;
    double hand = 0.0;
    double vaccinate = 0.0;
    double trvban_eff = 0.0;
    double quarantine_rate = 0.0;
    double work;
    double vac_rate;


if (lockdown_low || lockdown_mid || lockdown_high){
           if(lockdown_low){
             selfis = 0.5;
             dist = 0.25;
             school = 0.0;
             trvban_eff = 0.0;
             quarantine_rate = 0.0;
             work = 0.0;
             cocoon = 0.95;
             hand = 0.05;
             vaccinate = 0.0;
           }
           if(lockdown_mid){
             selfis = 0.5;
             dist = 0.35;
             school = 0.85;
             trvban_eff = 0.0;
             quarantine_rate = 0.05;
             work = 0.5;
             cocoon= 0.95;
             hand = 0.05;
             vaccinate = 0.0;
           }
           if(lockdown_high){
             selfis = 0.95;
             dist = 0.95;
             school = 0.85;
             trvban_eff = 0.95;
             quarantine_rate = 0.9;
             work = 0.75;
             cocoon = 0.95;
             hand = 0.075;
             vaccinate= 0.0;
           }
         }
         else{
           if (workhome){
             work = work_cov*work_eff;
           }else{work = 1.0;}
           if (isolation){
             selfis = selfis_cov;
             if(screen){
               screen_eff = MIN((report*I + reportc*CL + H+ICU+Vent+ reporth*HC + ICUC+VentC)  *screen_contacts*(screen_overdispersion*I/P)*screen_cov/P,1.0); 
             }
           }
           if (schoolclose){
             school = school_eff;
           }
           if(distancing){
             dist = dist_cov*dist_eff;
           }
           if(handwash){
             hand = hand_eff;
           }
           if(vaccine){
             vac_rate = -log(1.0-vaccine_cov)/vac_campaign;
             vaccinate = vac_rate;
           }
           if(travelban){
             trvban_eff = travelban_eff;
           }
           if(quarantine){
             quarantine_rate = MIN(((I+CL+H+ICU+Vent+HC+ICUC+VentC)*(household_size-1.0)/P),1.0)*quarantine_cov*quarantine_effort;
           }
         }


        // cocooning the elderly
        arma::mat cocoon_mat(21,21);
        cocoon_mat.fill((1.0-cocoon_eff));
        arma::mat cc_sub(age_cocoon-1,age_cocoon-1);
        cc_sub.fill(1.0);
        cocoon_mat.submat(0,0,cc_sub.n_rows-1,cc_sub.n_rows-1) = cc_sub;

        arma::mat contact_home = getVarmat("contact_home");
        arma::mat contact_other = getVarmat("contact_other");
        arma::mat contact_school = getVarmat("contact_school");
        arma::mat contact_work = getVarmat("contact_work");
        
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
        arma::mat contacts = (1.0-cocoon)*cts+cocoon*cts*cocoon_mat+cocoon*(1.0+school*(1.0-school_eff)+work*(1.0-work_eff))*contact_home*(1.0-cocoon_mat);
        double seas = 1.0+amp*cos(2*3.14*(t-(phi*365.25/12.))/365.25);
        double importation = mean_imports*(1.0-trvban_eff);
        
        arma::vec HH = H+ICU+Vent;
        arma::vec HHC = HC+ICUC+VentC;

        arma::vec lam = (1.0-hand)*p*seas*(contacts*((rho*E+(I+CL+importation)+(1.0-selfis_eff)*(X+HHC)+rhos*(HH))/P));
        
                // # contacts under home quarantine
        arma::vec lamq = (1.0-hand)*p*seas*((1.0-quarantine_eff_home)*contact_home*(((1.0-selfis_eff)*(X+HHC))/P))+(1.0-hand)*p*seas*(1.0-quarantine_eff_other)*(contact_other*((rho*E+(I+CL+importation)+(1.0-selfis_eff)*(X+HHC)+rhos*(HH))/P));
         
        // # birth/death
        double b1 = sum(THpopbirth % THpopstruct);
        arma::vec birth = 0.0*THpopbirth;
        birth(0) = b1;

        
        
      arma::mat ageing = getVarmat("ageing");
      
      //# ODE system
      arma::vec dSdt = -S%lam-S*vaccinate+omega*R+ageing*S-mort%S+birth-quarantine_rate*S +(1.0/quarantine_days)*QS;
      arma::vec dEdt = S%lam-gamma*E+ageing*E-mort%E + (1.0-vaccine_eff)*(lam%V)-quarantine_rate*E+(1.0/quarantine_days)*QE;
      arma::vec dIdt = gamma*(1.0-pclin)*(1.0-screen_eff)*(1.0-ihr)%E-nui*I+ageing*I-mort%I + (1.0/quarantine_days)*QI - quarantine_rate*I;
      arma::vec dCLdt = gamma*pclin*(1.0-selfis)*(1.0-ihr)%E-nui*CL+ageing*CL-mort%CL + (1.0/quarantine_days)*QC;
      arma::vec dRdt = nui*I-omega*R+nui*X+nui*CL+ageing*R-mort%R + (1.0/quarantine_days)*QR + nus*(1.0-pdeath_h*ifr)%H + 
        ((1.0-pdeath_icu*ifr)*nu_icu)%ICU + ((1.0-pdeath_icuc*ifr)*nu_icuc)%ICUC + ((1.0-pdeath_hc*ifr)*nusc)%HC + ((1.0-pdeath_vent*ifr)*nu_vent)%Vent+ 
        ((1.0-pdeath_ventc*ifr)*nu_ventc)%VentC;
      arma::vec dXdt = (gamma*selfis*pclin*(1.0-ihr))%E+(gamma*(1.0-pclin)*screen_eff*(1.0-ihr))%E-nui*X+ageing*X-mort%X; 
      arma::vec dVdt = vaccinate*S -((1.0-vaccine_eff)*lam)%V +ageing*V - mort%V;
         
      arma::vec dQSdt = quarantine_rate*S+ ageing*QS-mort%QS - (1.0/quarantine_days)*QS - lamq%QS;
      arma::vec dQEdt = quarantine_rate*E - gamma*QE + ageing*QE-mort%QE - (1.0/quarantine_days)*QE + lamq%QS; 
      arma::vec dQIdt = quarantine_rate*I + (gamma*(1.0-ihr)*(1.0-pclin))%QE-nui*QI+ageing*QI-mort%QI - (1.0/quarantine_days)*QI;
      arma::vec dQCdt = (gamma*(1.0-ihr))%(pclin*QE)-nui*QC+ageing*QC-mort%QC - (1.0/quarantine_days)*QC;
      arma::vec dQRdt = nui*QI+nui*QC+ageing*QR-mort%QR - (1.0/quarantine_days)*QR;
         
      arma::vec dHdt = gamma*ihr%((1.0-prob_icu)*(1.0-critH)*E) + gamma*ihr%((1.0-prob_icu)*(1.0-critH)*QE) - nus*H + ageing*H-mort%H; //  # all pdeath have to be lower than
      arma::vec dHCdt = gamma*ihr%((1.0-prob_icu)*critH*E) + gamma*ihr%((1.0-prob_icu)*critH*QE) - nusc*HC + ageing*HC-mort%HC;
      arma::vec dICUdt = gamma*ihr%(prob_icu*(1.0-crit)*(1.0-prob_vent)*E) + gamma*ihr%(prob_icu*(1.0-crit)*(1.0-prob_vent)*QE) - nu_icu*ICU +ageing*ICU - mort%ICU;
      arma::vec dICUCdt = gamma*ihr%(prob_icu*crit*(1.0-prob_vent)*E) + gamma*ihr%(prob_icu*crit*(1.0-prob_vent)*QE) - nu_icuc*ICUC +ageing*ICUC - mort%ICUC;
      arma::vec dVentdt = gamma*ihr%(prob_icu*(1.0-crit)*(1.0-critV)*prob_vent*E) + gamma*ihr%(prob_icu*(1.0-crit)*(1.0-critV)*prob_vent*QE) + (1.0-critV)*VentC*1.0/2.0 
                            - nu_vent*Vent +ageing*Vent - mort%Vent ;
      arma::vec dVentCdt = gamma*ihr%(prob_icu*prob_vent*(1.0-crit)*critV*E) +gamma*ihr%(prob_icu*prob_vent*crit*E)+
           gamma*ihr%(prob_icu*prob_vent*(1.0-crit)*critV*QE) + gamma*ihr%(prob_icu*prob_vent*crit*QE) - 
           (1.0-critV)*VentC*1.0/2.0-nu_ventc*VentC +ageing*VentC - mort%VentC ;
         
      arma::vec dCdt = report*gamma*(1.0-pclin)*(1.0-ihr)%(E+QE)+reportc*gamma*pclin*(1.0-ihr)%(E+QE)+ 
           gamma*ihr%((1.0-critH)*(1.0-prob_icu)*(E+QE))+gamma*ihr%(critH*reporth*(1.0-prob_icu)*(E+QE))+
           gamma*ihr%(prob_icu*(E+QE));
      arma::vec dCMdt = nus*pdeath_h*ifr%H + nusc*pdeath_hc*ifr%HC + nu_icu*pdeath_icu*ifr%ICU + nu_icuc*pdeath_icuc*ifr%ICUC +  nu_vent*pdeath_vent*ifr%Vent + nu_ventc*pdeath_ventc*ifr%VentC + 
           mort%H + mort%HC + mort%ICU + mort%ICUC + mort%Vent + mort%VentC; 
      arma::vec dCMCdt = nusc*pdeath_hc*ifr%HC+nu_icuc*pdeath_icuc*ifr%ICUC + nu_ventc*pdeath_ventc*ifr%VentC + mort%HC + mort%ICUC + mort%VentC;
   

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
  outvec = arma::join_cols(outvec,dVentdt);
  outvec = arma::join_cols(outvec,dVentCdt);
  outvec = arma::join_cols(outvec,dCMCdt);


  List output(1);
  output[0] = NumericVector(outvec.begin(),outvec.end());

  return output;

}
