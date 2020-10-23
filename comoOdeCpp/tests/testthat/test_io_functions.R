test_that("compare read_intervention_schedule of comoOdeCpp and inputs of como App", {
    # skip("temp skip")
    rm(list = ls())
    source(paste0(getwd(), "/common.R"), local = environment())
    init(e = environment())

    file_path <- paste0(getwd(), "/data/templates_v16.2/Template_CoMoCOVID-19App_io_intv_sched.xlsx")

    if (!exists("inputs", mode = "function")) {
    source(paste0(getwd(), CORE_FILE), local = environment())
    }

    intv_schd_base <- read_intervention_schedule(
        inp = read_excel(file_path, sheet = "Interventions"),
        run = "Baseline (Calibration)",
        time_max = tail(times, 1),
        steps_per_time = 20,
        startdate = startdate,
        stopdate = stopdate,
        age_testing_min = parameters["age_testing_min"],
        age_testing_max = parameters["age_testing_max"],
        age_vaccine_min = parameters["age_vaccine_min"],
        age_vaccine_max = parameters["age_vaccine_max"]
    )

    intv_schd_hype <- read_intervention_schedule(
        inp = read_excel(file_path, sheet = "Interventions"),
        run = "Hypothetical Scenario",
        time_max = tail(times, 1),
        steps_per_time = 20,
        startdate = startdate,
        stopdate = stopdate,
        age_testing_min = parameters["age_testing_min"],
        age_testing_max = parameters["age_testing_max"],
        age_vaccine_min = parameters["age_vaccine_min"],
        age_vaccine_max = parameters["age_vaccine_max"]
    )

    for (vv in names(vectors0)) {
        if (!all(vectors0[[vv]] == intv_schd_base[[vv]])) {
            print(vv)
            print("vectors0[[vv]]")
            print(vectors0[[vv]])
            print("intv_schd_base[[vv]]")
            print(intv_schd_base[[vv]])
        }
        expect_true(is.numeric((intv_schd_base[[vv]])))
        expect_true(all(vectors0[[vv]] == intv_schd_base[[vv]]))
    }
    for (vv in names(vectors)) {
        if (!all(vectors[[vv]] == intv_schd_hype[[vv]])) {
            print(vv)
            print("vectors[[vv]]")
            print(vectors[[vv]])
            print("intv_schd_hype[[vv]]")
            print(intv_schd_hype[[vv]])
        }
        expect_true(is.numeric((intv_schd_hype[[vv]])))
        expect_true(all(vectors[[vv]] == intv_schd_hype[[vv]]))
    }


    check_parameters_list_for_na(parameters)

    parameters["p"] <- 0.05

    input_list <- list(
        vectors0,
        intv_schd_base,
        vectors,
        intv_schd_hype
    )

    output_list <- list()
    output_processed_list <- list()

    cc <- 0
    for (ii in input_list) {


        covidOdeCpp_reset()
        output_message <- capture_output(
            output <- ode(
                            y = Y, times = times, method = "euler", hini = 0.05,
                            func = covidOdeCpp, parms = parameters,
                            input = ii, A = A,
                            contact_home = contact_home,
                            contact_school = contact_school,
                            contact_work = contact_work,
                            contact_other = contact_other,
                            popbirth_col2 = popbirth[, 2],
                            popstruc_col2 = popstruc[, 2],
                            ageing = ageing,
                            ifr_col2 = ifr[, 2],
                            ihr_col2 = ihr[, 2],
                            mort_col = mort
                        )
        )
        output_processed <- process_ode_outcome_mortality(output, ii, parameters)

        cc <- cc + 1
        output_list[[cc]] <- output
        output_processed_list[[cc]] <- output_processed

    }

    for (ii in c(1, 3)) {

        match_processed_outputs(
            outputA = output_processed_list[[ii]],
            outputB = output_processed_list[[ii + 1]],
            tlr = 0.0001
        )

        match_outputs(
            outputA = output_list[[ii]],
            outputB = output_list[[ii + 1]],
            tlr = 0.0001,
            smp = 100
        )

    }

})