
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

test_that("Matching Rcpp and R version at p={0.00,0.01, ... 0.1}", {
  check_libraries()
  rm(list = ls())

  library("deSolve")
  library("dplyr")
  library("readxl")
  library("comoOdeCpp")

  load("data/data_CoMo.RData")
  file_path <- paste0(getwd(), "/data/Template_CoMoCOVID-19App_v15.xlsx")

  if (!exists("inputs", mode = "function")) {
    source(paste0(getwd(), "/v15.2.core.R"), local = environment())
  }

  p_value_list = seq(0.0, 0.1, by = 0.01)

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

      out_r <- ode(
                y = Y, times = times, method = "euler", hini = 0.05,
                func = covid, parms = param_vector, input = ss
                )

      # sss = sss + 1
      # write.csv(out_cpp, paste0("out_cpp_",sss,"_",parameters["p"],".csv"),row.names = FALSE)
      # write.csv(out_r, paste0("out_r_",sss,"_",parameters["p"],".csv"),row.names = FALSE)

      for (ii in 1:1000) {
        rr = sample(1:nrow(out_r),1)
        cc = sample(1:ncol(out_r),1)
        expect_equal(
          out_cpp[rr,cc],
          out_r[rr,cc],
          tolerance = 0.03,
          scale = out_r[rr,cc]
        )
      }

      # expect_equal(out_cpp, out_r)
    }
  }

})

test_that("performance messages prints", {
  expect_equal(
    capture_output(covidOdeCpp_print_timing()),
    "duration_a=0\nduration_b=0\nduration_c=0"
  )
})
