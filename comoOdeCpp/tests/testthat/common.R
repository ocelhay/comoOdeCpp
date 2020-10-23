
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

match_outputs <-function(
    outputA,      # output matrix #1
    outputB,      # output matrix #2
    tlr = 0.0001, # tolerance
    smp = 1000    # num samples to take
  ) {

  for (ii in 1:smp) {
    rr = sample(1:nrow(outputA),1)
    cc = sample(1:ncol(outputA),1)
    # print(paste("outputA[rr,cc] =", outputA[rr,cc]))
    # print(paste("outputB[rr,cc] =", outputB[rr,cc]))

    outA = outputA[rr,cc]
    outB = outputB[rr,cc]

    expect_true(is.numeric(outA))
    expect_true(is.numeric(outB))

    expect_gte(outA, 0) # >=0
    expect_gte(outB, 0) # >=0

    if (outA > 0) {

      res = expect_equal(
        outB,
        outA,
        tolerance = tlr,
        scale = outA
      )

      if(abs(outB-outA)>outA*tlr){
        print(paste(
          "not equal: rr=", rr,
          ", cc=", cc,
          ", pp=", pp,
          ", outputA[rr,cc]", outA,
          ", outputB[rr,cc]", outB
        ))
      }

    }
  }

}

match_processed_outputs <- function(
    outputA,    # processed output matrix
    outputB,    # processed output matrix
    tlr = 0.0001 # tolerance
  ) {

  expect_true(is.numeric(outputA$total_cm_deaths_end))
  expect_true(is.numeric(outputA$total_reportable_deaths_end))

  expect_equal(
      outputA$total_cm_deaths_end,
      outputB$total_cm_deaths_end,
      tolerance = tlr,
      scale = outputB$total_cm_deaths_end
  )

  expect_equal(
      outputA$total_reportable_deaths_end,
      outputB$total_reportable_deaths_end,
      tolerance = tlr,
      scale = outputB$total_reportable_deaths_end
  )
}
