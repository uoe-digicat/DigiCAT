# Digital Counterfactual Analysis Tool: DigiCAT

<p align="center">

<img src="inst/DigiCAT/www/logos/DigiCAT/digicat6b.png"/>

</p>

### **Counterfactual Analysis**

The aim of counterfactual analysis is to estimate the causal effects of interventions or treatments, by comparing what actually happened (observed outcomes) with what would have happened if a different action had been taken (counterfactual outcomes). In observational settings, where random allocation into different treatments is not possible, researchers often employ methods involving 'propensity scores' (the estimated probability of receiving the treatment/intervention, based on a set of observed covariates). These propensity scores can then be used in an analysis to balance the characteristics of treatment vs non-treated groups, reducing bias and enabling a more accurate estimation of the causal effect of receiving the treatment.

### Our App

With the DigiCAT app, you can upload your own data and leverage propensity score methods to conduct counterfactual analyses, gaining insights into the causal effects of specific interventions or treatments. The primary objective of the DigiCAT app is to provide researchers, regardless of their statistical background, with a user-friendly platform that removes barriers and enables them to utilize these methods effectively.

## Getting Started

Currently, DigiCAT is available as an Shiny app, both online and as a downloadable R package.

### Online Tool

Our [online tool](https://digicatapp.shinyapps.io/DigiCAT/) allows users to use our example data to carry out counterfactual analysis. Please only use synthetic data if you wish to upload your own data to the online tool.

### Local Tool

DigiCAT is available as a downloadable R package that enables users to run the tool locally and use real data. R (\>= 2.10) required.

#### 1. Install DigiCAT

``` r
install.packages("remotes")
remotes::install_github("uoe-digicat/DigiCAT")
library(DigiCAT)
```

#### 2. Run DigiCAT locally

Once DigiCAT has been installed, the DigiCAT tool can be launched in RStudio or a browser. Launching the tool locally with 'enableLocal = TRUE' enables the upload local data for counterfactual analysis.

``` r
DigiCAT::run_DigiCAT(enableLocal = TRUE)
```

### Container platform

DigiCAT is also available as a container, meaning all required software is combined into a monolithic application. This means DigiCAT can be easily installed on new systems and is robust to updates in dependencies.

#### 1. Install Docker

<https://docs.docker.com/get-docker/>

#### 2. Download DigiCAT

This can be done from the command line and requires approximately 3 gigabytes of storage.

```{bash}
docker pull digicatuoe/digicat_tool:latest
```

#### 3. Run DigiCAT

DigiCAT can now be run locally from the command line.

```{bash}
docker run -p 3838:3838 digicatuoe/digicat_tool
```

Once running, visit <http://0.0.0.0:3838/> in your web browser to use the DigiCAT tool.

## DigiCAT Tutorial

To learn more about counterfactual analysis and the DigiCAT tool, please visit [our tutorial](https://uoe-digicat.github.io). Here you will find a comprehensive guide to the counterfactual analysis provided in our digital tool.

## Contact

If you have any questions or feedback, please get in touch!

Email: [uoe_digicat-group\@uoe.onmicrosoft.com](mailto:uoe_digicat-group@uoe.onmicrosoft.com)

## Package Structure

### DESCRIPTION file

Contains overall metadata about the DigiCAT package, such as package dependencies.

### NAMESPACE file

Contains the names of functions exported by the DigiCAT package and imported packages.

### R/ directory

This folder contains all R scripts (.R files) that will be sourced when the DigiCAT package is installed.

#### **Main counterfactual analysis scripts**

-   **propensity_estimation_stage.R**: Contains the function 'estimation_stage()' which calculates the propensity score (or likelihood of belonging to the treatment group) for each individual in the sample data, based on the matching variables provided.

-   **balance_data.R**: Contains the function 'balance_data()' which balances datasets for counterfactual analysis in DigiCAT..

-   **outcome_analysis_stage.R**: Contains the function ''outcome_analysis_stage()' which runs the outcome model for counterfactual analysis in DigiCAT.

#### **mod\_\* files**

Modules for each analysis page: Get Started (mod_home.R), Data Upload (mod_data_upload.R), Approach (mod_counterfactual_approach.R), Balancing (mod_balancing.R) and Outcome (mod_outcome_model.R).

### data/ directory

This folder contains the sample dataset 'zp_eg'. Visit [our tutorial](https://uoe-digicat.github.io/02_howto_digicat.html#example-dataset) to find out more about this dataset.

### inst/DigiCAT directory

Contains

-   **ui.R file**: User interface of tool
-   **server.R file**: R session that accepts input from the UI, runs code and returns output to the UI for display
-   **global.R**: Contains global objects. This file is executed once when tool is initiated.
-   **desc_global.R file**: Descriptions of counterfactual analysis terms to populate tool
-   **www/ directory**: Contains images and style sheets rendered in the tool

### man/ directory

This folder contains documentation for the functions exported from the DigiCAT package.
