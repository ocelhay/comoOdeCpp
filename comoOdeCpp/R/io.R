#' Read the "Interventions" tab of the template into a list of time-series vectors
#' 
#' This function is to provide the same functionality of the inputs function that's part of the como App
#' which is to translate the information from the Excel spreadsheet to a list of time-series vectors controlling
#' the actions of different interventions during the course of the simulation
#' @return A list of vectors including those listed as `val_vec` and `bol_vec` in `intv_profile_list`
#' @export
read_intervention_schedule <- function(
        inp, # read_excel(file_path, sheet = "Interventions")
        run, # 'Baseline (Calibration)' or 'Hypothetical Scenario'
        time_max, # tail(times,1)
        steps_per_time, # 20 <- 1/hini; hini = 0.05
        startdate,  # Date, Simulation Start, "Parameters" tab
        stopdate,   # Date, Simulation End, "Parameters" tab
        age_testing_min,    # "Interventions Param" tab
        age_testing_max,    # "Interventions Param" tab
        age_vaccine_min,    # "Interventions Param" tab
        age_vaccine_max     # "Interventions Param" tab
    ) {

    inp[["Date Start"]] = pmax(startdate, as.Date(inp[["Date Start"]]))
    inp[["Date End"]] = pmax(startdate, as.Date(inp[["Date End"]]))

    # cap intervention end dates with simulation end date
    inp[["Date Start"]] = pmin(stopdate, as.Date(inp[["Date Start"]]))
    inp[["Date End"]] = pmin(stopdate, as.Date(inp[["Date End"]]))

    v<-(format(as.POSIXct(inp[["Date Start"]],format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
    v2<-as.Date(v,format="%d/%m/%y")
    inp[["Date Start"]]<-v2

    v<-(format(as.POSIXct(inp[["Date End"]],format='%Y/%m/%d %H:%M:%S'),format="%d/%m/%y"))
    v2<-as.Date(v,format="%d/%m/%y")
    inp[["Date End"]]<-v2

    inp <- arrange(inp, `Date Start`)

    tv<-which(inp[["Apply to"]]==run)

    intv_profile_list <- list(
        list(text="Self-isolation if Symptomatic"     , val_vec="si_vector"     , val_default=0,     bol_vec="isolation"   ),
        list(text="Screening (when S.I.)"             , val_vec="scr_vector"    , val_default=0,     bol_vec="screen"      ),
        list(text="Social Distancing"                 , val_vec="sd_vector"     , val_default=0,     bol_vec="distancing"  ),
        list(text="Handwashing"                       , val_vec="hw_vector"     , val_default=0,     bol_vec="handwash"    ),
        list(text="Masking"                           , val_vec="msk_vector"    , val_default=0,     bol_vec="masking"     ),
        list(text="Working at Home"                   , val_vec="wah_vector"    , val_default=0,     bol_vec="workhome"    ),
        list(text="School Closures"                   , val_vec="sc_vector"     , val_default=0,     bol_vec="schoolclose" ),
        list(text="Shielding the Elderly"             , val_vec="cte_vector"    , val_default=0,     bol_vec="cocoon"      ),
        list(text="Household Isolation (when S.I.)"   , val_vec="q_vector"      , val_default=0,     bol_vec="quarantine"  ),
        list(text="International Travel Ban"          , val_vec="tb_vector"     , val_default=0,     bol_vec="travelban"   ),
        list(text="Vaccination"                       , val_vec="vc_vector"     , val_default=0,     bol_vec="vaccine"     ),
        list(text="Mass Testing"                      , val_vec="mt_vector"     , val_default=0,     bol_vec="masstesting" ),
        list(text="Age Testing Minimum"               , val_vec="minas_vector"  , val_default=age_testing_min              ),
        list(text="Age Testing Maximum"               , val_vec="maxas_vector"  , val_default=age_testing_max              ),
        list(text="Age Vaccine Minimum"               , val_vec="minav_vector"  , val_default=age_vaccine_min              ),
        list(text="Age Vaccine Maximum"               , val_vec="maxav_vector"  , val_default=age_vaccine_max              ),
        list(text="Dexamethasone"                     ,                                              bol_vec="dex"         )
    )

    intv_vectors <- list()

    for (intv in intv_profile_list) {
        # print(intv[["text"]])

        # default vectors
        ii_val_vec <- rep(0, time_max * steps_per_time)
        if(!is.null( intv[["val_default"]] )){
            ii_val_vec <- rep(intv[["val_default"]], time_max * steps_per_time)
        }
        ii_bol_vec <- rep(0, time_max * steps_per_time)

        ii_rows <- intersect(which(inp[["Intervention"]]==intv[["text"]]),tv)

        if (length(ii_rows) >= 1) {
            for(rr in ii_rows) {

                t1 <- inp[["Date Start"]][rr]-startdate
                t2 <- inp[["Date End"]][rr]-startdate

                stopifnot(t1 >= 0)
                stopifnot(t2 >= 0)

                if (t1 < t2) {
                    idx1 = t1*steps_per_time+1
                    idx2 = t2*steps_per_time
                    ii_val_vec[idx1:idx2] = inp[["Value"]][rr]
                    ii_bol_vec[idx1:idx2] = 1
                }

            }
        }

        if (!is.null( intv[["val_vec"]] )) {
            intv_vectors[[ intv[["val_vec"]] ]] <- ii_val_vec
        }
        if (!is.null( intv[["bol_vec"]] )) {
            intv_vectors[[ intv[["bol_vec"]] ]] <- ii_bol_vec
        }

    }

    return(intv_vectors)
}