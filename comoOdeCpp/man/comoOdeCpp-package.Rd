\name{comoOdeCpp-package}
\alias{comoOdeCpp-package}
\alias{comoOdeCpp}
\docType{package}
\title{
\packageTitle{comoOdeCpp}
}
\description{
\packageDescription{comoOdeCpp}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{comoOdeCpp}
\packageIndices{comoOdeCpp}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
\packageAuthor{comoOdeCpp}
Ricardo Aguas, Sompob Saralamba, Bo Gao

Maintainer: Bo Gao \packageMaintainer{comoOdeCpp}
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# library("comoOdeCpp")
covidOdeCpp_reset()
# out0 <- ode(y = Y, times = times, func = covidOdeCpp, parms = parameters,
#             input=vectors0, A=A,
#             contact_home=contact_home, contact_school=contact_school,
#             contact_work=contact_work, contact_other=contact_other,
#             popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
#             ageing=ageing,
#             ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)
}
