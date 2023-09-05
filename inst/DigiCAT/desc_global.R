desc_global <<- list(

  ## Counterfactual Approach----
  cfapproach_binary = div(h5("Description:"),
  p("The aim of counterfactual analysis is to estimate the causal effects of 'exposures' or 'treatments' by comparing what actually happened 
  (observed outcomes) with what would have happened if a different action had been taken (counterfactual outcomes). Because we can never directly 
  observe counterfactual outcomes, we compare groups who differ in their treatment. However, in observational settings where random allocation into 
  different treatments is not possible, researchers may employ methods that such as 'matching' or 'weighting' of participants to ensure that the 
  different treatment groups being compared are 'balanced' with respect to other characteristics. For further guidance on choosing an approach see 
  the ", a(id = "link","‘choosing a counterfactual analysis approach’", href = "https://uoe-digicat.github.io/03_choosecf.html",  target="_blank")," section of the tutorials.")), 
  
  cfapproach_temp = p(p("Please click on the approach you would like to take. We will give you more information about each approach as you select them.")),

  prop_matching = p(h5("Propensity Matching:"),
  p("You've chosen propensity matching. This approach involves creating balanced comparison groups by matching treated individuals with similar untreated individuals 
    based on their propensity scores (probability of someone having a specific treatment based on a set of ‘matching variables’). This aims to ensure that the groups 
    are comparable in terms of potential confounding variables. The treatment effect is then estimated by comparing outcomes between the matched groups. See the ",
    a(id = "link","‘propensity matching’", href = "https://uoe-digicat.github.io/04_cfmethod.html#propensity-score-matching",  target="_blank"), "section of the 
    tutorials for more information.")),
  
  iptw = p(h5("Inverse probability of treatment weighting (IPTW):"),
  p("You’ve chosen IPTW. IPTW aims to rebalance treated and untreated individuals using a weighted regression approach. It first estimates propensity scores 
  (probability of someone having a specific treatment based on a set of ‘matching variables’. It then transforms these into weights that are used in a regression 
    model to up-weight (i.e., give more importance to) untreated and treated cases that are similar in terms of their propensity scores and down-weight cases that 
    are very different. This helps to rebalance the treated and untreated groups. See the ",a(id = "link","‘IPTW’", href = "https://uoe-digicat.github.io/04_cfmethod.html#iptw", 
    target="_blank"), "section of the tutorials for more information.")),
  
  cfapproach_ordinal = p(h5("Description:"), 
                         p("As you have an ordinal treatment variable (i.e., an ordered categorical treatment variable) an appropriate counterfactual analysis 
                           approach is non-bipartite propensity matching (NBP). NBP finds pairs of people who differ in their treatment dosage but who have 
                           similar treatment propensities based on their matching variables and compares their outcome variable levels. As the treatment is ordinal, 
                           this might involve matching and comparing people with for example, low with medium treatment doses and medium with high doses. Please 
                           see the ", a(id = "link","'NBP'", href = "https://uoe-digicat.github.io/04_cfmethod.html#nbp", target="_blank"), " section of the tutorials for more information.")
                         ),
  
  missingness = p(h5("Description:"),p("Missing data can reduce statistical power and introduce bias when it is related to the variables under study. In order to minimise 
                                       the impact of missingness in our data, DigiCAT offers several approaches to handle missing values. For more information on missingness, 
                                       see the ", a(id = "link","‘missing data’", href = "https://uoe-digicat.github.io/05_missing.html", target="_blank")," section of the tutorials.")),
  
  mi = p(h5("Multiple Imputation:"),
  p("The fundamental idea behind multiple imputation is to create several (M) completed datasets by predicting what the missing values would have been if we could observe them. 
  These datasets are then analysed separately, and the results of each of these datasets are combined, or ‘pooled’, together. The variability across datasets allows the standard 
  errors to take account of the uncertainty due to the fact that some of the data are predicted rather than observed. DigiCAT imputes 5 complete datasets, conducts the analysis 
  in each and then combines the results across them.", br(),
  "However, don’t forget that this statistical method relies on assumptions, which can be difficult to test. If these assumptions are violated, your inferences may be invalid. 
  Please see our ", a(id = "link","tutorial pages on multiple imputation", href = "https://uoe-digicat.github.io/05_missing.html#multiple-imputation", target="_blank"), " for a detailed discussion. Multiple imputation assumes ‘missingness 
  at random’ (MAR) meaning that the missingness depends on the data you have available in your model but not on the unobserved values of the outcome variable.")),
  
  weighting = p(h5("Non-response weights:"),
                p("Non-response weighting deals with missingness by up-weighting cases that have a low probability of being observed in the sample and down-weighting variables 
                  that have a high probability of being observed in the sample. This is done using a weighted regression approach. These weights are often supplied with datasets 
                  and are selected at the ‘data upload’ stage of DigiCAT. Please see our ", a(id = "link","‘non-response weighting’ tutorial section", href = "https://uoe-digicat.github.io/05_missing.html#non-response-weighting", target="_blank")," for more information.")),
  
  completecase = p(h5("Complete Cases:"),
                   p("Complete case analysis (CCA) (also sometimes known as ‘listwise deletion’), will only analyse the completely observed cases. This analysis will allow valid 
                     inferences of your data if the missing data are missing completely at random (MCAR), because the observed values will be a random sample of the complete dataset. 
                     If the data are missing at random (MAR) which means that the missingness can be predicted from the data available or missing not at random (MNAR), the inferences 
                     may be invalid. Please see our ", a(id = "link","tutorial pages", href = "https://uoe-digicat.github.io/05_missing.html#complete-case-analysis", target="_blank"), " for a detailed discussion of complete case analysis.")),
  
  balancing_model = div(h5("Description:"), 
                        p("In order to balance our dataset, we must first calculate the probability each individual has of being in the treatment group, based on their observed 
                          characteristics, i.e., their propensity score. On this page, you will select a balancing model to use with your dataset to predict the likelihood of an 
                          individual being treated. Different modelling approaches can be used for this step. Please see the ", a(id = "link","tutorial section on balancing models**", 
                          href = "", target="_blank"), " if you would like more guidance on choosing a balancing model.")),
  
  glm = p(h5("Logistic Regression:"),
          p("Logistic regression is a method of modelling the relations between a set of predictors (e.g., matching variables) and a binary outcome (such as whether or not someone is 
            in the treatment group). In propensity score analysis it can be used to estimate the propensity scores for each individual. See the ", 
            a(id = "link","logistic regression section of our tutorials**", href = "", target="_blank"), " for more information.")),
  
  olr = p(h5("Ordinal logistic regression:"),
          p("For ordinal treatments an ordinal logistic regression model is used to estimate the propensity scores used for matching. It predicts treatment (as an ordinal variable with 
            different dosage levels) from the set of chosen matching variables.. See the ", 
            a(id = "link","ordinal logistic regression section of our tutorials*", href = "https://uoe-digicat.github.io/04_cfmethod.html#non-bipartite-nbp-methods", target="_blank"), " for more information.")),
  
  ## Balancing----
  
  matching_method = p(h5("Matching Method:"), p("In order to balance covariates between treatment groups, propensity score matching involves matching 
                                                individuals based on their propensity scores, which represent their likelihood of being treated based on observed characteristics. 
                                                The goal is to create a pseudo-randomized comparison between the treatment and control groups by matching individuals who have similar 
                                                or close propensity scores. See our ", 
                                                a(id = "link","tutorial section on matching methods*", href = "https://uoe-digicat.github.io/04_cfmethod.html#matching-on-the-propensity-score", target="_blank")," for more detailed guidance on choosing an appropriate matching method.")),
  
  matching_ratio = p(h5("Matching Ratio:"), 
                     p("Matching ratios in propensity score matching refer to the number of control/untreated individuals that are matched to each treated 
                       individual. In DigiCAT, matching ratios can be specified to control the trade-off between achieving better balance between treatment 
                       groups and maintaining a larger sample size. See our ", 
                       a(id = "link","tutorial section on matching ratios*", href = "https://uoe-digicat.github.io/04_cfmethod.html#matching-on-the-propensity-score", target="_blank")," for more detailed guidance on choosing an appropriate matching ratio.")),
  
  nearest = p(h5("Nearest Neighbour (NN):"),
  p("Nearest neighbour matching is used in counterfactual analysis as a method for pairing treated and control cases with similar propensity scores. In DigiCat, nearest neighbour greedy matching 
              is implemented whereby the most similar treated and control cases are matched first. Then, from those left the remaining most similar treated and control cases are paired and so on and so
              forth until all viable matches have been made. See the ", a(id = "link","nearest neighbour matching tutorial*", href = "https://uoe-digicat.github.io/04_cfmethod.html#matching-on-the-propensity-score", target="_blank")," for more details.")),
  
  optimal_binary = p(h5("Optimal:"),
  p("You've chosen optimal matching. This method matches treated and untreated cases in a way that minimises the average propensity score difference within the match pairs in the sample.
                See the ", a(id = "link","optimal matching tutorial section*", href = "https://uoe-digicat.github.io/04_cfmethod.html#matching-on-the-propensity-score", target="_blank")," for more detail.")),
  
  optimal_ordinal = p(h5("Matching Method: Optimal"), 
                     p("NBP uses a version of optimal matching to pair up cases who experienced different dosages of the treatment variable (e.g., high versus medium; medium versus low). 
                     Optimal matching is based on finding matches to minimise the overall propensity score differences between matched cases. The algorithm in DigiCAT also disallows 
                     matches of pairs that are too dis-similar to help ensure that there is good balance between the groups after matching.
                     See the ", a(id = "link","NBP section of the tutorials", href = "https://uoe-digicat.github.io/03_choosecf.html#nbp", target="_blank")," for more detail.")),
  
  one_to_one_binary = p(h5("1:1:"),
  p("1:1 is the most commonly used matching ratio. In 1:1 matching, we match one unit belonging to the intervention group with one unit belonging to the control group that has the closest propensity score. 
                   See our ",a(id = "link","tutorial section on 1:1 matching*", href = "https://uoe-digicat.github.io/04_cfmethod.html#matching-on-the-propensity-score", target="_blank")," for more information.")),
  
  one_to_one_ordinal = p(h5("Matching Ratio: 1:1"), 
                        p("In NBP, 1:1 matching is used, meaning that each case is matched with one other case with a different treatment dosage.")),
  
  one_to_K = p(h5("1:K:"), p("You've chosen a 1:K matching ratio. By matching more than 1 control to each treated unit more of the sample can be used. However, this can come at the cost of the treated and 
  untreated groups being less balanced and can mean there is more bias. Note that if you have a limited number of untreated participants you may not be able to implement a matching ratio greater than 1:1.")),
  
  common_support_graph = p(h5("Common support graph:"), 
                           p("The common support graph shows how much overlap there is between different treatment groups in their propensity scores. For multiple imputation this is based on an average of 
                             propensity scores across datasets. Ideally there is a lot of overlap; however, sometimes the different treatment groups are very different from each other and there is too little overlap to make
                             counterfactual analysis feasible.")),
  
  ## Outcome----
  
  outcome_model = p(h5("Outcome Model:"), 
                    p("Linear regression is a way of modelling the associations between exploratory variable(s) and a continuous outcome variable. The model takes the form y = bX + e, where y and x are our outcome and 
                    explanatory variables respectively, and e is the random error. Of interest here is b, which is the estimated effect of our treatment variable on our outcome. By fitting a model to a sample that is either 
                    matched or weighted according to the propensity of treatment, we can better estimate the **causal** effect of the treatment on our outcome variable. 
                      As you select an outcome model option more information will appear.")),
  
  linear_regression_wo_mvars = p(h5("Linear regression without matching variables as covariates:"),
                                 p("This option will fit a linear regression with your chosen outcome and just the treatment variable as a predictor using the matched data or IPTW weights. Note that this method does 
                                   not take into account the paired nature of the data when used with a matching method like propensity score matching for binary treatments or NBP for ordinal treatments.")),
                    
  linear_regression_w_mvars = p(h5("Linear regression adjusted for matching variables as covariates:"),
                                 p("This option will fit a linear regression with your chosen outcome and the treatment variable and matching variables as predictors. 
                                   This option is recommended for matching methods as it addresses the fact that dependencies (or ‘clustering’) are introduced into the 
                                   data due to matching. It can also help to deal with the effects of any remaining imbalance that isn’t completely addressed by 
                                   the matching or weighting.")),
  
  linear_regression_w_mvars_interactions = p(h5("Linear regression adjusted for matching variables as covariates and their interactions with the treatment variable:"),
                                 p("This option will fit a linear regression with your chosen outcome and the following predictors: i) the treatment variable ii) all 
                                   matching variables iii) all interactions between treatment and matching variables. This method addresses the fact that dependencies 
                                   (or ‘clustering’) are introduced into the data due to matching and can help to deal with the effects of any remaining imbalance that 
                                   isn’t completely addressed by the matching or weighting. It additionally deals with imbalance and bias that might be related to the 
                                   fact that treatment status might interact with the matching variables (e.g., a treatment might affect people of different genders 
                                   differently so if there isn’t a perfect gender balance there might be some bias in estimating the treatment effect).")),
  
  estimate = p("In counterfactual analysis, the estimate can be used to quantify the potential causal effect of specific factors, interventions, or treatments on mental health outcomes."),
  
  standard_error = p("The standard error is a statistical measure that quantifies the variability or uncertainty associated with the estimate. It provides a measure of how much the estimate is likely to vary from the true population value."),
  
  p_value = p("In null-hypothesis significance testing, the p-value represents the the probability of obtaining a test statistic as extreme or more extreme than the one observed, assuming that the null hypothesis is true. Typically, if the p-value is below a predetermined significance level (often 0.05), the null hypothesis is rejected in favour of an alternative hypothesis, implying that there is a statistically significant effect or relationship in the data.")                  
                    
)