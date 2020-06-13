\name{RcppArmadillo-Functions}
\alias{covidOdeCpp}
\alias{covidOdeCpp_reset}
\alias{covidOdeCpp_print_timing}
\title{Set of functions in comoOdeCpp package}
\description{
  This package was created when
  \code{RcppArmadillo.package.skeleton()} is invoked to create a
  skeleton packages.
}
\usage{
#library("comoOdeCpp")
covidOdeCpp_reset()
# out0 <- ode(y = Y, times = times, func = covidOdeCpp, parms = parameters,
#             input=vectors0, A=A,
#             contact_home=contact_home, contact_school=contact_school,
#             contact_work=contact_work, contact_other=contact_other,
#             popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
#             ageing=ageing,
#             ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)
covidOdeCpp_reset()
covidOdeCpp_print_timing()
}
\arguments{
  \item{Y}{a numeric vector}
}
\value{
  \code{covidOdeCpp()} returns a list.

  \code{covidOdeCpp_reset()} resets the initialised status to false
  resets any accumulated timing information to zeros.

  \code{covidOdeCpp_print_timing()} prints performance information.

}
\details{

}
\references{
  See the documentation for Armadillo, and RcppArmadillo, for more details.
}
\examples{
#library("comoOdeCpp")
covidOdeCpp_reset()
# out0 <- ode(y = Y, times = times, func = covidOdeCpp, parms = parameters,
#             input=vectors0, A=A,
#             contact_home=contact_home, contact_school=contact_school,
#             contact_work=contact_work, contact_other=contact_other,
#             popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
#             ageing=ageing,
#             ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)
}
\author{Bo Gao}

