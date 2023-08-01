desc_global <<- list(
  
  ## Counterfactual Approach
  cfapproach = div(h5("Description:"),p("The aim of counterfactual analysis is to estimate the causal effects of 'exposures' or 'treatments' by comparing what actually happened (observed outcomes) with what would have happened if a different action had been taken (counterfactual outcomes). Because we can never directly observe counterfactual outcomes, we compare groups who differ in their treatment. However, in observational settings where random allocation into different treatments is not possible, researchers may employ methods that such as 'matching' or 'weighting' of participants to ensure that the different treatment groups being compared are 'balanced' with respect to other characteristics."), br(), p("Please click on the approach you would like to take. We will give you more information about each approach as you select them.")), 
  
  prop_matching = p(h5("Propensity Matching:"),p("You've chosen propensity matching. This approach involves creating balanced comparison
groups by matching treated individuals with similar untreated individuals based on their propensity scores (probability of someone having a specific treatment based on observed characteristics). This aims to ensure that the groups are comparable in terms of potential confounding variables. The treatment effect is then estimated by comparing outcomes between the matched groups.")),
  
  iptw = p(h5("Inverse probability of treatment weighting (IPTW):"),p("You've choosen IPTW, this is why is may/may not be a good choice.")),
  
  NBP = p(h5("Non-Bipartite (NBP):"),p("You've choosen NBP, this is why is may/may not be a good choice.")),
  
  missingness = p(h5("Missingness:"),p("Missing data can introduce bias, affecting the validity of analyses and the reliability of conclusions drawn from the data. In order to minimise the impact of missingness in our data, DigiCAT offers several approaches to handle missing values.")),
  
  mi = p(h5("Multiple Imputation"),p("The fundamental idea behind multiple imputation is to create several (M) completed datasets by predicting what the missing values would have been if we could observe them. These datasets are then analysed using software for completely observed datasets, and the results of each of these datasets are combined, or ‘pooled’, together. The variability across datasets allows the standard errors to take account of the uncertainty due to the fact that some of the data are predicted rather than observed."),br(),p("However,don’t forget that this statistical method relies on assumptions, which can be difficult to test. If these assumptions are violated, your inferences may be invalid. Please see our tutorial pages for a detailed discussion of which missingness handling method may be best for you. Note: if you set m = 1, you will be performing single imputation. The default in DigiCAT is m = 5. ")),
  
  weighting = p(h5("Weighting"),p("You've choosen weighting, this is why is may/may not be a good choice.")),
  
  completecase = p(h5("Complete Cases"),p("Complete case analysis (CCA) (also sometimes known as ‘listwise deletion’), will only analyse the completely observed cases. This analysis will allow valid inferences of your data if the missing data are missing completely at random (MCAR), because the observed values will be a random sample of the complete dataset. If the data are missing at random (MAR) or missing not at random (MNAR), the inferences may be invalid. Please see our tutorial pages for a detailed discussion of which missingness handling method may be best for you.")),
  
  balancing_model = div(h5("Description:"), p("In order to balance our dataset, we must first calculate the probability each individual has of being in our treatment group, based on their observed characteristics, i.e., their propensity score. On this page, you will select a balancing model to train on your dataset to predict the likelihood of an individual being treated. Different modelling approaches can be used for this step and the selection of specific model depends on the characteristics of the data and the research question at hand. Please visit our tutorial if you would like more guidance on choosing a balancing model.")),
  
  glm = p(h5("Probit Regression"),p("Probit regression is a method of modelling binary outcomes (such as whether or not someone is in the treatment group). This assumes that the probability of the outcome follows a cumulative distribution function of a normal distribution. This 'probit' function links the linear combination of independent variables into the probability space between 0 and 1.")),
  
  gbm = p(h5("Gradient Boosting Machine (GBM)"), p("You've choosen GBM, this is why is may/may not be a good choice.")),
  
  rf = p(h5("Random Forest"), p("You've choosen random forest, this is why is may/may not be a good choice.")),
  
  ## Balancing
  
  balancing_method = p(h5("Matching Method:"), p("In order to balance covariates between treatment groups, propensity score matching involves matching individuals based on their propensity scores, which represent their likelihood of being treated based on observed characteristics. The goal is to create a pseudo-randomized comparison between the treatment and control groups by matching individuals who have similar or close propensity scores.")),
  
  balancing_ratio = p(h5("Matching Ratio:"), p("Matching ratios in propensity score matching refer to the number of control/untreated individuals that are matched to each treated individual. In DigiCAT, matching ratios can be specified to control the trade-off between achieving better balance between treatment groups and maintaining an adequate sample size.")),
  
  nearest = p(h5("Matching Method: Nearest Neighbour (NN)"), p("Nearest neighbour matching is used in counterfactual analysis as a method for pairing treated and control cases with similar propensity scores. In DigiCat, nearest neighbour greedy matching is implemented whereby the most similar treated and control cases are matched first. Then, from those left the remaining most similar treated and control cases are paired and so on and so forth until all viable matches have been made. See the nearest neighbour matching tutorial for more details.")),
  
  optimal = p(h5("Matching Method: Optimal"), p("You've choosen optimal matching, this is why is may/may not be a good choice.")),
  
  one_to_one = p(h5("Matching Ratio: 1:1"), p("1:1 is the most commonly used matching ratio, although this ratio can be changed, for example, if there are many more members belonging to the control group than members belonging to the treatment group. In 1:1 matching, we match one unit belonging to the intervention group with one unit belonging to the control group that has the closest propensity score. The default matching strategy in DigiCAT is a nearest neighbour (‘greedy’) matching algorithm, with a 1:1 matching ratio. As our default is matching without replacement, the selected units will be taken out of the donor pool once matched. If you think this may not be best for you, see our tutorial pages for more detailed guidance on choosing an appropriate matching ratio.")),
  
  one_to_K = p(h5("Matching ratio: 1:K"), p("You've choosen a 1:K matching ratio, this is why is may/may not be a good choice."))
  
)