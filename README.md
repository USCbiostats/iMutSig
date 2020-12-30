# *iMutSig*: a web application to identify the most similar mutational signature using shiny

![](https://img.shields.io/badge/release%20version-1.0-green.svg)
[![](https://img.shields.io/badge/doi-10.12688/f1000research.24435.2-yellow.svg)](https://doi.org/10.12688/f1000research.24435.2)


![](https://f1000researchdata.s3.amazonaws.com/manuscripts/30641/76f8ffbc-ffb7-4956-a271-534cd8db6ee9_figure2.gif)

## Paper
You can download the paper at [*F1000Research*](https://doi.org/10.12688/f1000research.24435.2) and cite the paper:

- **Yang Z**, Pandey P, Marjoram P and Siegmund KD. iMutSig: a web application to identify the most similar mutational signature using shiny [version 2; peer review: 2 approved]. F1000Research 2020, 9:586 (https://doi.org/10.12688/f1000research.24435.2)

## How to use the Shiny app
### Website
This Shiny app is hosted at shinyapps.io where you can access using the link https://zhiyang.shinyapps.io/imutsig/. 

### Local installation
If you'd like to use this Shiny app locally, please type the following command in your RStudio. 

```
git clone https://github.com/USCbiostats/iMutSig.git
```

To run the Shiny app, you need to install the following packages. If you run into any issues while installing `pmsignature`, please refer to its GitHub page for more details https://github.com/friend1ws/pmsignature. 

```
packages <- c("shinyjs", "shinydashboard", "shiny", "dplyr", 
              "DT", "corrplot", "stringr", "devtools")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

if (!("d3heatmap" %in% rownames(installed.packages()))){
  devtools::install_github("rstudio/d3heatmap")
}

if (!("pmsignature" %in% rownames(installed.packages()))){
  devtools::install_github("friend1ws/pmsignature", ref = "devel")
}

if (!("decompTumor2Sig" %in% rownames(installed.packages()))){
  devtools::install_github("zhiiiyang/decompTumor2Sig")
}


```

By clicking the `Run App` button in either `ui.R` or `server.R` script, a Shiny app will run locally. Or you can simply enter `runApp()` in the console. 

## Report issues

Please open an issue at https://github.com/USCbiostats/iMutSig/issues if you run into any issues or would like to add a new feature. Thank you!

## Funding
This work was supported by NCI grant numbers 5P30 CA014089 and P01 CA196569.

