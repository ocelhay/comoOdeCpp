
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

test_that("scenario01 baseline", {
  check_libraries()
  rm(list = ls())

  library("deSolve")
  library("dplyr")
  library("readxl")
  library("comoOdeCpp")

  load("data/data_CoMo.RData")
  file_path <- paste0(getwd(), "/data/Template_CoMoCOVID-19App_new_1.xlsx")

  if (!exists("inputs", mode = "function")) {
    source(paste0(getwd(), "/v13.13.core.R"), local = environment())
  }

  p_value_list <- list(
    0.00,
    0.01,
    0.02,
    0.03,
    0.035
  )

  for (pp in p_value_list) {
    parameters["p"] <- pp

    covidOdeCpp_reset()
    out_cpp <- ode(y = Y, times = times, method = "euler", hini = 0.05,
                func = covidOdeCpp, parms = parameters,
                input = vectors0, A = A,
                contact_home = contact_home, contact_school = contact_school,
                contact_work = contact_work, contact_other = contact_other,
                popbirth_col2 = popbirth[, 2], popstruc_col2 = popstruc[, 2],
                ageing = ageing,
                ifr_col2 = ifr[, 2], ihr_col2 = ihr[, 2], mort_col = mort
                )

    out_r <- ode(y = Y, times = times, method = "euler", hini = 0.05,
                func = covid, parms = parameters, input = vectors0
                )

    expect_equal(out_cpp, out_r)
  }
})


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
