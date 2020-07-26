# *iMutSig*: a web application to identify the most similar mutational signature using shiny

![](https://img.shields.io/badge/release%20version-1.0-green.svg)
[![](https://img.shields.io/badge/doi-10.12688/f1000research.24435-yellow.svg)](https://doi.org/10.12688/f1000research.24435.1)


## Paper
You can download the paper at [*F1000Research*](https://doi.org/10.12688/f1000research.24435.1) and cite the paper:

Yang Z, Pandey P, Marjoram P and Siegmund KD. iMutSig: a web application to identify the most similar mutational signature using shiny [version 1; peer review: 2 approved with reservations]. *F1000Research* 2020, 9:586 (https://doi.org/10.12688/f1000research.24435.1)

## How to use the Shiny app
### Website
This Shiny app is hosted at Shinyapp.io where you can access using the link zhiyang.shinyapps.io/imutsig/. 

## Local installation
If you'd like to use this Shiny app locally, please type the following command in your RStudio. 

```
git clone https://github.com/USCbiostats/iMutSig.git
```

To run the Shiny app, you need to install the following packages. 

```
packages <- c("shinyjs", "shinydashboard", "shiny", "dplyr", "DT", "corrplot", "stringr", "devtools")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

devtools::install_github("friend1ws/pmsignature", ref = "devel")
```

By click the `Run App` button in either `ui.R` or `server.R`, you can start using the Shiny app locally. 