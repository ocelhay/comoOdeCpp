# comoOdeCpp

RCpp version of CoMo Consortium's COVID-19 transmission model


by Ricardo Aguas, Sompob Saralamba, Bo Gao


## Install

- GUI option:

RStudio >> Tools >> Install Packages >> Install from: Package Archive File (.tar.gz) >> Package archive: Browse to select `comoOdeCpp_13.8.1.tar.gz`


- Commandline option:

`install.packages("path_to_download/comoOdeCpp_13.8.1.tar.gz", repos = NULL, type="source")`


- Using `devtools`

```r

install.packages("devtools")
library("devtools")
install_github("ATOME-MORU/comoOdeCpp", subdir="comoOdeCpp")

```


## Usage:

`library("comoOdeCpp")`


```r

covidOdeCpp_reset()

out0 <- ode(y = Y, times = times, func = covidOdeCpp, parms = parameters,
            input=vectors0, A=A,
            contact_home=contact_home, contact_school=contact_school,
            contact_work=contact_work, contact_other=contact_other,
            popbirth_col2=popbirth[,2], popstruc_col2=popstruc[,2],
            ageing=ageing,
            ifr_col2=ifr[,2], ihr_col2=ihr[,2], mort_col=mort)


```

