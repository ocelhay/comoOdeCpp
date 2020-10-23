CORE_FILE <- "/v16.2.core.input_mod.R"

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
      print(paste0("parameters_list[\"", pp_name, "\"] = ", parameters_list[[pp_name]]), quote = FALSE)
      testthat::expect_equal(is.na(parameters_list[[pp_name]]), FALSE)
      stop()
    }
  }
}

match_outputs <- function(
    output_a,      # output matrix #1
    output_b,      # output matrix #2
    tlr = 0.0001, # tolerance
    smp = 1000    # num samples to take
  ) {

  # for (ii in 1:smp) {
  for (ii in seq_len(smp)) {
    # rr <- sample(1:nrow(output_a), 1)
    # cc <- sample(1:ncol(output_a), 1)
    rr <- sample(seq_len(nrow(output_a)), 1)
    cc <- sample(seq_len(ncol(output_a)), 1)
    # print(paste("output_a[rr,cc] =", output_a[rr,cc]))
    # print(paste("output_b[rr,cc] =", output_b[rr,cc]))

    out_a <- output_a[rr, cc]
    out_b <- output_b[rr, cc]

    testthat::expect_true(is.numeric(out_a))
    testthat::expect_true(is.numeric(out_b))

    testthat::expect_gte(out_a, 0) # >=0
    testthat::expect_gte(out_b, 0) # >=0

    if (out_a > 0) {

      res <- expect_equal(
        out_b,
        out_a,
        tolerance = tlr,
        scale = out_a
      )

      if (abs(out_b - out_a) > out_a * tlr) {
        print(paste(
          "not equal: rr=", rr,
          ", cc=", cc,
          ", pp=", pp,
          ", output_a[rr,cc]", out_a,
          ", output_b[rr,cc]", out_b
        ))
      }

    }
  }

}

match_processed_outputs <- function(
    output_a,    # processed output matrix
    output_b,    # processed output matrix
    tlr = 0.0001 # tolerance
  ) {

  testthat::expect_true(is.numeric(output_a$total_cm_deaths_end))
  testthat::expect_true(is.numeric(output_a$total_reportable_deaths_end))

  testthat::expect_equal(
      output_a$total_cm_deaths_end,
      output_b$total_cm_deaths_end,
      tolerance = tlr,
      scale = output_b$total_cm_deaths_end
  )

  testthat::expect_equal(
      output_a$total_reportable_deaths_end,
      output_b$total_reportable_deaths_end,
      tolerance = tlr,
      scale = output_b$total_reportable_deaths_end
  )
}
