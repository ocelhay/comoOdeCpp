
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

check_mortality_count <- function(out_mat) {

  dexo2_hist <- rep(0,length(times))
  dexo2c_hist <- rep(0,length(times))
  dexv_hist <- rep(0,length(times))
  dexvc_hist <- rep(0,length(times))

  for (tt in times) {
    if((tt)<max(times)){
      if(ss$dex[tt*20+1]) {
        dexo2_hist[tt+1] <- parameters["dexo2"]
        dexo2c_hist[tt+1] <- parameters["dexo2c"]
        dexv_hist[tt+1] <- parameters["dexv"]
        dexvc_hist[tt+1] <- parameters["dexvc"]
      } else {
        dexo2_hist[tt+1] <- 1
        dexo2c_hist[tt+1] <- 1
        dexv_hist[tt+1] <- 1
        dexvc_hist[tt+1] <- 1
      }
    } else {
      dexo2_hist[tt+1] <- dexo2_hist[tt]
      dexo2c_hist[tt+1] <- dexo2c_hist[tt]
      dexv_hist[tt+1] <- dexv_hist[tt]
      dexvc_hist[tt+1] <- dexvc_hist[tt]
    }
  }

  # dexo2_hist_mat = matrix(dexo2_hist, nrow=length(dexo2_hist), ncol = A, byrow=FALSE)

  # Secion A (Attributable)

  cinc_mort_1 <- cumsum(rowSums(parameters["nus"]*parameters["propo2"]*parameters["pdeath_ho"]*dexo2_hist*(out_mat[,(Hindex+1)]%*%ifr[,2])))
  cinc_mort_2 <- cumsum(rowSums(parameters["nus"]*(1-parameters["propo2"])*parameters["pdeath_h"]*(out_mat[,(Hindex+1)]%*%ifr[,2])))
  cinc_mort_3 <- cumsum(rowSums(parameters["nusc"]*parameters["propo2"]*parameters["pdeath_hco"]*(out_mat[,(HCindex+1)]%*%ifr[,2])))
  cinc_mort_4 <- cumsum(rowSums(parameters["nusc"]*(1-parameters["propo2"])*parameters["pdeath_hc"]*(out_mat[,(HCindex+1)]%*%ifr[,2])))
  cinc_mort_5 <- cumsum(rowSums(parameters["nu_icu"]*parameters["propo2"]*parameters["pdeath_icuo"]*dexo2_hist*(out_mat[,(ICUindex+1)]%*%ifr[,2])))
  cinc_mort_6 <- cumsum(rowSums(parameters["nu_icu"]*(1-parameters["propo2"])*parameters["pdeath_icu"]*(out_mat[,(ICUindex+1)]%*%ifr[,2])))
  cinc_mort_7 <- cumsum(rowSums(parameters["nu_icuc"]*parameters["propo2"]*parameters["pdeath_icuco"]*dexo2c_hist*(out_mat[,(ICUCindex+1)]%*%ifr[,2])))
  cinc_mort_8 <- cumsum(rowSums(parameters["nu_icuc"]*(1-parameters["propo2"])*parameters["pdeath_icuc"]*(out_mat[,(ICUCindex+1)]%*%ifr[,2])))
  cinc_mort_9 <- cumsum(rowSums(parameters["nu_vent"]*parameters["pdeath_vent"]*dexv_hist*(out_mat[,(Ventindex+1)]%*%ifr[,2])))
  cinc_mort_10 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mat[,(VentCindex+1)]%*%ifr[,2])))
  cinc_mort_11 <- cumsum(rowSums(parameters["nu_ventc"]*parameters["pdeath_ventc"]*dexvc_hist*(out_mat[,(ICUCVindex+1)]%*%ifr[,2])))

  cinc_mort_H1 <- cinc_mort_1 + cinc_mort_2
  cinc_mort_HC1 <- cinc_mort_3 + cinc_mort_4
  cinc_mort_ICU1 <- cinc_mort_5 + cinc_mort_6
  cinc_mort_ICUC1 <- cinc_mort_7 + cinc_mort_8
  cinc_mort_Vent1 <- cinc_mort_9
  cinc_mort_VentC1 <- cinc_mort_10
  cinc_mort_ICUCV1 <- cinc_mort_11

  cinc_mort_sum <- cinc_mort_1 + 
                  cinc_mort_2 +
                  cinc_mort_3 +
                  cinc_mort_4 +
                  cinc_mort_5 +
                  cinc_mort_6 +
                  cinc_mort_7 +
                  cinc_mort_8 +
                  cinc_mort_9 +
                  cinc_mort_10 +
                  cinc_mort_11

  # Section B

  base_mort_H1 <- cumsum(rowSums(out_mat[,(Hindex+1)]%*%mort))
  base_mort_HC1 <- cumsum(rowSums(out_mat[,(HCindex+1)]%*%mort))
  base_mort_ICU1 <- cumsum(rowSums(out_mat[,(ICUindex+1)]%*%mort))
  base_mort_ICUC1 <- cumsum(rowSums(out_mat[,(ICUCindex+1)]%*%mort))
  base_mort_ICUCV1 <- cumsum(rowSums(out_mat[,(ICUCVindex+1)]%*%mort))
  base_mort_Vent1 <- cumsum(rowSums(out_mat[,(Ventindex+1)]%*%mort))
  base_mort_VentC1 <- cumsum(rowSums(out_mat[,(VentCindex+1)]%*%mort))
  base_mort_Z1 <- cumsum(rowSums(out_mat[,(Zindex+1)]%*%mort))
  base_mort_V1 <- cumsum(rowSums(out_mat[,(Vindex+1)]%*%mort))

  base_mort_sum <- base_mort_H1 +
                  base_mort_HC1 +
                  base_mort_ICU1 +
                  base_mort_ICUC1 +
                  base_mort_ICUCV1 +
                  base_mort_Vent1 +
                  base_mort_VentC1 +
                  base_mort_Z1 +
                  base_mort_V1  

  # Section C

  base_mort_S1 <- cumsum(rowSums(out_mat[,(Sindex+1)]%*%mort)) # not reportable
  base_mort_E1 <- cumsum(rowSums(out_mat[,(Eindex+1)]%*%mort)) # not reportable # reportable
  base_mort_I1 <- cumsum(rowSums(out_mat[,(Iindex+1)]%*%mort))
  base_mort_CL1 <- cumsum(rowSums(out_mat[,(CLindex+1)]%*%mort))
  base_mort_X1 <- cumsum(rowSums(out_mat[,(Xindex+1)]%*%mort))
  base_mort_QS1 <- cumsum(rowSums(out_mat[,(QSindex+1)]%*%mort)) # not reportable
  base_mort_QE1 <- cumsum(rowSums(out_mat[,(QEindex+1)]%*%mort)) # not reportable # reportable
  base_mort_QI1 <- cumsum(rowSums(out_mat[,(QIindex+1)]%*%mort))
  base_mort_QC1 <- cumsum(rowSums(out_mat[,(QCindex+1)]%*%mort))
  base_mort_QR1 <- cumsum(rowSums(out_mat[,(QRindex+1)]%*%mort)) # not reportable # reportable
  base_mort_R1 <- cumsum(rowSums(out_mat[,(Rindex+1)]%*%mort)) # not reportable  # reportable
  # base_mort_V1 <- cumsum(rowSums(out_mat[,(Vindex+1)]%*%mort)) # not reportable

  base_mort_sum <- base_mort_sum +
                    base_mort_S1 +  #*
                    base_mort_E1 +
                    base_mort_I1 +
                    base_mort_CL1 +
                    base_mort_X1 +
                    base_mort_QS1 +
                    base_mort_QE1 +
                    base_mort_QI1 +
                    base_mort_QC1 +
                    base_mort_QR1 +
                    base_mort_R1    #*

  ## total_deaths
  sum_deaths_1 <- last(cinc_mort_sum+base_mort_sum)
  # print(paste("sum_deaths_1:",sum_deaths_1))


  ## cum_mortality (total_reported_deaths_end)
  sum_deaths_2 <- last(rowSums(out_mat[,(CMindex+1)]))   
  # print(paste("sum_deaths_2:",sum_deaths_2))

  # expect_lt(
  #   abs(sum_deaths_2-sum_deaths_1),
  #   1
  # )
  expect_equal(
    sum_deaths_1,
    sum_deaths_2,
    tolerance = 0.01,
    scale = sum_deaths_2
  )

}

test_that("Matching Rcpp and R version at p={0.00,0.01, ... 0.1}", {
  check_libraries()
  rm(list = ls())

  library("deSolve")
  library("dplyr")
  library("readxl")
  library("comoOdeCpp")

  load("data/data_CoMo.RData")
  
  file_path <- paste0(getwd(), "/data/Template_CoMoCOVID-19App_new_16.1.xlsx")

  if (!exists("inputs", mode = "function")) {
    source(paste0(getwd(), "/v16.1.core.R"), local = environment())
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

      processed_cpp <- process_ode_outcome_mortality(out_cpp)

      # stop()

      expect_silent(
        out_r <- ode(
                  y = Y, times = times, method = "euler", hini = 0.05,
                  func = covid, parms = param_vector, input = ss
                  )
      )

      processed_r <- process_ode_outcome_mortality(out_r)

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
