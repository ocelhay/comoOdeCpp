
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
