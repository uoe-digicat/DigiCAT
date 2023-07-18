# Digital Counterfactual Analysis Tool: DigiCAT

![](inst/DigiCAT/www/logos/DigiCAT/digicat6b.png)

### **Counterfactual Analysis:**

The aim of counterfactual analysis is to estimate the causal effects of interventions or treatments, by comparing what actually happened (observed outcomes) with what would have happened if a different action had been taken (counterfactual outcomes). In observational settings, where random allocation into different treatments is not possible, researchers often employ methods involving 'propensity scores' (the estimated probability of receiving the treatment/intervention, based on a set of observed covariates). These propensity scores can then be used in an analysis to balance the characteristics of treatment vs non-treated groups, reducing bias and enabling a more accurate estimation of the causal effect of receiving the treatment.

### Our App:

With the DigiCAT app, you can upload your own data and leverage propensity score methods to conduct counterfactual analyses, gaining insights into the causal effects of specific interventions or treatments. The primary objective of the DigiCAT app is to provide researchers, regardless of their statistical background, with a user-friendly platform that removes barriers and enables them to utilize these methods effectively.

## Getting Started:

Currently, DigiCAT is available as an Shiny app, both online and as a downloadable R package.

### Online Tool

Our [online tool](https://digicatapp.shinyapps.io/DigiCAT/) allows users to use our example data to carry out counterfactual analysis. Please only use synthetic data if you wish to upload your own data to the online tool.

### Local Tool

DigiCAT is available as a downloadable R package that enables users to run the tool locally and use real data. R (\>= 2.10) required.

#### 1. Install DigiCAT

``` r
install.packages("remotes")
remotes::install_github("josiahpjking/DigiCAT")
library(DigiCAT)
```

#### 2. Run DigiCAT locally

Once DigiCAT has been installed, the DigiCAT tool can be launched in RStudio or a browser.

``` r
DigiCAT::run_DigiCAT()
```

## Contact:

Please get in touch with any questions/feedback you have

Email: [uoe_digicat-group\@uoe.onmicrosoft.com](mailto:uoe_digicat-group@uoe.onmicrosoft.com)
