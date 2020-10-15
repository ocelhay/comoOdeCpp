
CORE_FILE <- "/v16.2.core.R"

check_libraries <- function() {
  library_list <- list(
    "deSolve",
    "dplyr",
    "readxl"
  )
  for (ll in library_list) {
    if (!requireNamespace(ll, quietly = TRUE)) {
      testthat::skip(paste(ll, "needed but not available"))
    }
  }
}

load_libraries <- function() {
  check_libraries()
  library("deSolve")
  library("dplyr")
  library("readxl")
  library("comoOdeCpp")
}

init <- function(e) {
  load_libraries()
  load("data/data_CoMo.RData", envir = e)
}

check_parameters_list_for_na <- function(parameters_list) {
  for (pp_name in names(parameters_list)) {
    if (is.na(parameters_list[[pp_name]])) {
      print(paste0("parameters_list[\"",pp_name, "\"] = ", parameters_list[[pp_name]]), quote = FALSE)
      expect_equal(is.na(parameters_list[[pp_name]]), FALSE)
      stop()
    }
  }
}

test_that("Splitting intervention", {
  # skip("temp skip")
  rm(list = ls())
  init(e=environment())

  file_path <- paste0(getwd(), "/data/Template_CoMoCOVID-19App_new_16.1_intv_split.xlsx")

  # if (!exists("inputs", mode = "function")) {
    source(paste0(getwd(), CORE_FILE), local = environment())
  # }

  check_parameters_list_for_na(parameters)
  covidOdeCpp_reset()
  output_message <- capture_output(
    out_base <- ode(
                y = Y, times = times, method = "euler", hini = 0.05,
                func = covidOdeCpp, parms = parameters,
                input = vectors0, A = A,
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
  processed_base_results <- process_ode_outcome_mortality(out_base, vectors, parameters)


  covidOdeCpp_reset()
  output_message <- capture_output(
    out_hype <- ode(
                y = Y, times = times, method = "euler", hini = 0.05,
                func = covidOdeCpp, parms = parameters,
                input = vectors , A = A,
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
  processed_hype_results <- process_ode_outcome_mortality(out_hype, vectors0, parameters)

  expect_equal(
    processed_base_results$total_cm_deaths_end,
    processed_hype_results$total_cm_deaths_end
  )


})

test_that("Matching Rcpp and R version at p={0.00,0.01, ... 0.1}", {
  # skip("temp skip")

  rm(list = ls())
  init(e=environment())
  
  file_path <- paste0(getwd(), "/data/Template_CoMoCOVID-19App_new_16.1.xlsx")

  if (!exists("inputs", mode = "function")) {
    source(paste0(getwd(), CORE_FILE), local = environment())
  }

  for (pp_name in names(parameters)) {
    if (is.na(parameters[[pp_name]])) {
      print(paste0("parameters[\"",pp_name, "\"] = ", parameters[[pp_name]]), quote = FALSE)
      expect_equal(is.na(parameters[[pp_name]]), FALSE)
      stop()
    }
  }

  # environment(check_mortality_count) <- environment()

  p_value_list = seq(0.0, 0.1, by = 0.02)
  # p_value_list = seq(0.1, 0.1)

  scenario_list <- list(
    vectors0, # Baseline
    vectors   # Hypothetical
  )

  # sss = 0

  for (ss in scenario_list) {
    for (pp in p_value_list) {
      parameters["p"] <- pp

      param_vector <- parameters
      param_vector[parameters_noise] <- parameters[parameters_noise]
        + rnorm(
            length(parameters_noise),
            mean = 0,
            sd = noise*abs(parameters[parameters_noise])
          )

      covidOdeCpp_reset()
      output_message <- capture_output(
        out_cpp <- ode(
                    y = Y, times = times, method = "euler", hini = 0.05,
                    func = covidOdeCpp, parms = param_vector,
                    input = ss, A = A,
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
      expect_equal(output_message, "covidOdeCpp: splinefuns updated")

      processed_cpp_results <- process_ode_outcome_mortality(out_cpp, ss, param_vector)

      expect_equal(
        processed_cpp_results$total_reportable_deaths_end,
        processed_cpp_results$total_cm_deaths_end,
        tolerance = 0.01,
        scale = processed_cpp_results$total_cm_deaths_end
      )

      # stop()

      expect_silent(
        out_r <- ode(
                  y = Y, times = times, method = "euler", hini = 0.05,
                  func = covid, parms = param_vector, input = ss
                  )
      )

      processed_r_results <- process_ode_outcome_mortality(out_r, ss, param_vector)

      expect_equal(
        processed_r_results$total_reportable_deaths_end,
        processed_r_results$total_cm_deaths_end,
        tolerance = 0.01,
        scale = processed_r_results$total_cm_deaths_end
      )

      expect_equal(
        processed_cpp_results$total_cm_deaths_end,
        processed_r_results$total_cm_deaths_end,
        tolerance = 0.0001,
        scale = processed_r_results$total_cm_deaths_end
      )
      expect_equal(
        processed_cpp_results$total_reportable_deaths_end,
        processed_r_results$total_reportable_deaths_end,
        tolerance = 0.0001,
        scale = processed_r_results$total_reportable_deaths_end
      )

      # print(paste("processed_r$total_reported_deaths_end=", processed_r$total_reported_deaths_end))      
      # print(paste("processed_r$total_deaths_end=", processed_r$total_deaths_end))      

      # print(paste(
      #     "results$attributable_deaths_end:", processed_cpp$attributable_deaths_end, ",", processed_r$attributable_deaths_end,
      #     "results$reportable_death:", processed_cpp$reportable_death, ",", processed_r$reportable_death,
      #     "results$total_deaths_end:", processed_cpp$total_deaths_end, ",", processed_r$total_deaths_end
      #   ))

      # expect_equal(processed_cpp$attributable_deaths_end, processed_r$attributable_deaths_end)
      # expect_equal(processed_cpp$reportable_death, processed_r$reportable_death)
      # expect_equal(processed_cpp$total_deaths_end, processed_r$total_deaths_end)

      # sss = 1
      # write.csv(out_cpp, paste0("out_cpp_",sss,"_",parameters["p"],".csv"),row.names = FALSE)
      # write.csv(out_r, paste0("out_r_",sss,"_",parameters["p"],".csv"),row.names = FALSE)

      for (ii in 1:1000) {
        rr = sample(1:nrow(out_r),1)
        cc = sample(1:ncol(out_r),1)
        # print(paste("out_r[rr,cc] =", out_r[rr,cc]))
        # print(paste("out_cpp[rr,cc] =", out_cpp[rr,cc]))

        expect_gte(out_r[rr,cc], 0) # >=0
        expect_gte(out_cpp[rr,cc], 0) # >=0

        if (out_r[rr,cc] > 0) {

          res = expect_equal(
            out_cpp[rr,cc],
            out_r[rr,cc],
            tolerance = 0.0001,
            scale = out_r[rr,cc]
          )

          if(abs(out_cpp[rr,cc]-out_r[rr,cc])>out_r[rr,cc]*0.0001){
            print(paste(
              "not equal: rr=", rr,
              ", cc=", cc,
              ", pp=", pp,
              ", out_r[rr,cc]", out_r[rr,cc],
              ", out_cpp[rr,cc]", out_cpp[rr,cc]
            ))
          }

        }
      }

    }
  }

})

test_that("performance messages prints", {
  expect_equal(
    capture_output(covidOdeCpp_print_timing()),
    "duration_a=0\nduration_b=0\nduration_c=0"
  )
})
