test_that("Sensitivity", {
    # skip("temp skip")
    rm(list = ls())
    source(paste0(getwd(), "/common.R"), local = environment())
    init(e=environment())


    file_path <- paste0(getwd(), "/data/Template_CoMoCOVID-19App_new_16.1_sa.xlsx")

    if (!exists("inputs", mode = "function")) {
        source(paste0(getwd(), CORE_FILE), local = environment())
    }

    check_parameters_list_for_na(parameters)

    parameters["p"] <- 0.05

    covidOdeCpp_reset()
    output_message <- capture_output(
        out_hype <- ode(
                        y = Y, times = times, method = "euler", hini = 0.05,
                        func = covidOdeCpp, parms = parameters,
                        input = vectors, A = A,
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
    processed_hype_results <- process_ode_outcome_mortality(out_hype, vectors, parameters)

    sa_parameters <- c(
        "selfis_eff",
        "dist_eff",
        "hand_eff",
        "mask_eff",
        "work_eff",

        # "school_eff",
        "cocoon_eff",
    	# "travelban_eff",
        "vaccine_eff"
    )
    sa_multipliers <- c(
        0.1,
        0.1,
        0.1,
        0.1,
        0.1,

        # 0.1,
        0.1,
        # 0.1,
        0.1
    )
    sa_expect_mort_inc <- c(
        TRUE,
        TRUE,
        TRUE,
        TRUE,
        TRUE,

        # TRUE,
        TRUE,
        # TRUE,
        TRUE
    )

    for (pp_name in sa_parameters) {

        parameters_mod <- parameters
        parameters_mod[pp_name] = parameters_mod[pp_name] * sa_multipliers[match(pp_name, sa_parameters)]

        covidOdeCpp_reset()
        output_message <- capture_output(
            out_hype_mod <- ode(
                            y = Y, times = times, method = "euler", hini = 0.05,
                            func = covidOdeCpp, parms = parameters_mod,
                            input = vectors, A = A,
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

        processed_hype_mod_results <- process_ode_outcome_mortality(out_hype_mod, vectors, parameters_mod)

        if (sa_expect_mort_inc[match(pp_name, sa_parameters)]) {
            expect_gt(
                processed_hype_mod_results$total_cm_deaths_end,
                processed_hype_results$total_cm_deaths_end
            )
        } else {
            expect_gt(
                processed_hype_mod_results$total_cm_deaths_end,
                processed_hype_results$total_cm_deaths_end
            )
        }

    }


 })