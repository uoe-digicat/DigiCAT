desc_global <<- list(
  
  cfapproach = div(h5("Description:"),p("The aim of counterfactual analysis is to estimate the causal effects of 'exposures' or 'treatments' by comparing what actually happened (observed outcomes) with what would have happened if a different action had been taken (counterfactual outcomes). Because we can never directly observe counterfactual outcomes, we compare groups who differ in their treatment. However, in observational settings where random allocation into different treatments is not possible, researchers may employ methods that such as 'matching' or 'weighting' of participants to ensure that the different treatment groups being compared are 'balanced' with respect to other characteristics."), br(), p("Please click on the approach you would like to take. We will give you more information about each approach as you select them.")), 
  
  
  prop_matching = p(h5("Propensity Matching:"),p("You've chosen propensity matching. This approach involves creating balanced comparison
groups by matching treated individuals with similar untreated individuals based on their propensity scores (probability of someone having a specific treatment based on observed characteristics). This aims to ensure that the groups are comparable in terms of potential confounding variables. The treatment effect is then estimated by comparing outcomes between the matched groups.")),
  
  iptw = p(h5("Inverse probability of treatment weighting (IPTW):"),p("You've choosen IPTW, this is why is may/may not be a good choice.")),

  
  
  missingness = p(h5("Missingness:"),p("Missing data can introduce bias, affecting the validity of analyses and the reliability of conclusions drawn from the data. In order to minimise the impact of missingness in our data, DigiCAT offers several approaches to handle missing values.")),
  
  mi = p(h5("Multiple Imputation"),p("The fundamental idea behind multiple imputation is to create several (M) completed datasets by predicting what the missing values would have been if we could observe them. These datasets are then analysed using software for completely observed datasets, and the results of each of these datasets are combined, or ‘pooled’, together. The variability across datasets allows the standard errors to take account of the uncertainty due to the fact that some of the data are predicted rather than observed."),br(),p("However,don’t forget that this statistical method relies on assumptions, which can be difficult to test. If these assumptions are violated, your inferences may be invalid. Please see our tutorial pages for a detailed discussion of which missingness handling method may be best for you. Note: if you set m = 1, you will be performing single imputation. The default in DigiCAT is m = 5. ")),
  
  weighting = p(h5("Weighting"),p("You've choosen weighting, this is why is may/may not be a good choice.")),
  
  completecase = p(h5("Complete Cases"),p("Complete case analysis (CCA) (also sometimes known as ‘listwise deletion’), will only analyse the completely observed cases. This analysis will allow valid inferences of your data if the missing data are missing completely at random (MCAR), because the observed values will be a random sample of the complete dataset. If the data are missing at random (MAR) or missing not at random (MNAR), the inferences may be invalid. Please see our tutorial pages for a detailed discussion of which missingness handling method may be best for you.")),
  
  fiml = p(h5("Full Information Maximum Likelihood (FIML)"),p("You've choosen FIML, this is why is may/may not be a good choice."))
  
  
  
  
  
  
  
  
  
  
)