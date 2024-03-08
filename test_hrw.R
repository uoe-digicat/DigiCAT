source("R/propensity_estimation_stage.R")
source("R/evaluate_imputations.R")
source("R/evaluate_propensity_stage.R")
source("R/balance_data.R")
source("R/outcome_analysis_stage.R")

N =500
A = matrix(runif(5^2)*2-1, ncol = 5)
Xmat = MASS::mvrnorm(N, mu=rnorm(5,0,3), Sigma = t(A)%*%A)
lp = apply(Xmat, 2, scale)%*%rnorm(5,0,2)
t = rbinom(N,1,plogis(lp))
y = base::cbind(Xmat,t) %*% c(rnorm(5,0,1),2) + rnorm(N,0,1)
df <- as.data.frame(base::cbind(Xmat, t, y))
names(df) <- c(letters[1:5], "t", "y")

df2 = mice::ampute(df,
                   prop = 0.05)

#### mi
# observe before mi handling
evaluate_imputations(.data = df2$amp, evaluation_method = "LittleMCARtest")
evaluate_imputations(.data = df2$amp, evaluation_method = "missing_pattern")
evaluate_imputations(.data = df2$amp, evaluation_method = "influx_outflux")

abc <- estimation_stage(.data = df2$amp, missing_method = "mi", model_type = "glm",
                        treatment_variable = "t", matching_variable = c("a", "b"), weighting_variable = "e") 
evaluate_imputations(abc, "distributional_discrepancy", "strip")
evaluate_imputations(abc, "convergence") # include guidance line as output maybe
evaluate_imputations(abc, "eventslog") # depending on logged events, recommend altering parameters xyz accordingly
evaluate_imputations(abc, "inspect_matrix")

ghi <- balance_data(counterfactual_method = "cbps", treatment_variable = "t", 
                    matching_variable = c("a", "b"), PS_estimation_object = abc,
                    missing_method = "mi")
mno <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "cbps", 
                              outcome_variable = "y",
                              treatment_variable = "t", 
                              matching_variable = c("a", "b"), 
                              psmodel_obj = abc,
                              missing_method = "mi",
                              outcome_formula = "unadjusted",
                              covariates = "d",
                              weighting_variable = "e")

#### cc

abc <- estimation_stage(.data = df2$amp, missing_method = "complete", model_type = "gbm",
                        treatment_variable = "t", matching_variable = c("a", "b")) 
evaluate_propensity_stage(abc, evaluation_method = "support")
ghi <- balance_data(counterfactual_method = "cbps", treatment_variable = "t", 
                    matching_variable = c("a", "b"), PS_estimation_object = abc,
                    missing_method = "complete")
mno <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "cbps", 
                              outcome_variable = "y",
                              treatment_variable = "t", 
                              matching_variable = c("a", "b"), 
                              psmodel_obj = abc,
                              missing_method = "complete",
                              outcome_formula = "with_matching_variables")

#### Weighting testing ####

gen_X <- function(n) {
  X <- matrix(rnorm(9 * n), nrow = n, ncol = 9)
  X[,5] <- as.numeric(X[,5] < .5)
  X
}

#~20% treated
gen_A <- function(X) {
  LP_A <- - 1.2 + log(2)*X[,1] - log(1.5)*X[,2] + log(2)*X[,4] - log(2.4)*X[,5] + 
    log(2)*X[,7] - log(1.5)*X[,8]
  P_A <- plogis(LP_A)
  rbinom(nrow(X), 1, P_A)
}

# Continuous outcome
gen_Y_C <- function(A, X) {
  2*A + 2*X[,1] + 2*X[,2] + 2*X[,3] + 1*X[,4] + 2*X[,5] + 1*X[,6] + rnorm(length(A), 0, 5)
}

gen_SW <- function(X) {
  e <- rbinom(nrow(X), 1, .3)
  1/plogis(log(1.4)*X[,2] + log(.7)*X[,4] + log(.9)*X[,6] + log(1.5)*X[,8] + log(.9)*e +
             -log(.5)*e*X[,2] + log(.6)*e*X[,4])
}

n <- 2000
X <- gen_X(n)
A <- gen_A(X)
SW <- gen_SW(X)

Y_C <- gen_Y_C(A, X)

d <- data.frame(A, X, Y_C, SW)
rm(SW)
rm(A)
rm(Y_C)
rm(X)
rm(n)

df2 = mice::ampute(d,
                   prop = 0.15)
data_to_use <- df2$amp


abc <- estimation_stage(.data = data_to_use, missing_method = "weighting", model_type = "glm",
                        treatment_variable = "A", matching_variable = c("X1", "X2"),
                        weighting_variable = "SW") 
ghi <- balance_data(counterfactual_method = "cbps", treatment_variable = "A", 
                    matching_variable = c("X1", "X2"), PS_estimation_object = abc,
                    missing_method = "weighting")

pqr <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "cbps", # outcome format needs changing for weighting approach
                              outcome_variable = "Y_C",
                              treatment_variable = "A", 
                              matching_variable = c("X1", "X2"), 
                              psmodel_obj = abc,
                              missing_method = "weighting",
                              weighting_variable = "SW",
                              outcome_formula = "with_matching_variables")


data(nhanes) 
# nb: model/variable choice makes no sense due to variable types
# but used as an example to add cluster/strata/weights etc
abc <- estimation_stage(.data = nhanes, missing_method = "weighting", model_type = "glm",
                        treatment_variable = "HI_CHOL", matching_variable = "race", 
                        weighting_variable = "WTMEC2YR", cluster_variable = "SDMVPSU",
                        strata_variable = "SDMVSTRA") 
ghi <- balance_data(counterfactual_method = "psm", treatment_variable = "HI_CHOL", 
                    matching_variable = c("race"), PS_estimation_object = abc,
                    missing_method = "weighting")

mno <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "psm", 
                              outcome_variable = "RIAGENDR",
                              treatment_variable = "HI_CHOL", 
                              matching_variable = "race", 
                              psmodel_obj = abc,
                              missing_method = "weighting",
                              weighting_variable = "WTMEC2YR")

evaluate_propensity_stage(abc, "support", missing_method = "weighting")

#### NBP testing ####

# Set a seed for reproducibility
set.seed(577)

# Function to simulate data with a known treatment effect
simulate_data <- function(n = 300) {
  # Simulate data for a treatment group
  treatment_data <- data.frame(
    ID = 1:n,
    age = rnorm(n, mean = 40, sd = 10),
    income = rnorm(n, mean = 50000, sd = 10000),
    treatment = as.factor(sample(1:3, n, replace = TRUE))
  )
  
  # Simulate data for a control group
  control_data <- data.frame(
    ID = (n + 1):(2 * n),
    age = rnorm(n, mean = 38, sd = 8),
    income = rnorm(n, mean = 48000, sd = 12000),
    treatment = as.factor(sample(1:3, n, replace = TRUE))
  )
  
  # Combine the treatment and control groups into one dataset
  simulated_data <- rbind(treatment_data, control_data)
  
  # Simulate a continuous outcome with a known treatment effect
  simulated_data$continuous_outcome <- 50 + 5 * as.numeric(simulated_data$treatment) +
    2 * simulated_data$age + rnorm(2 * n, mean = 0, sd = 10)
  
  return(simulated_data)
}

# Simulate data
simulated_data <- simulate_data()
simulated_data = mice::ampute(simulated_data,
                              prop = 0.05)

un <- estimation_stage(.data = simulated_data$amp, missing_method = "complete", model_type = "poly",
                       treatment_variable = "treatment", matching_variable = c("age", "income"))
deux <- balance_data(counterfactual_method = "nbp", treatment_variable = "treatment",
                     matching_variable = c("age", "income"), PS_estimation_object = un,
                     missing_method = "complete")
trois <- outcome_analysis_stage(balanced_data = deux, counterfactual_method = "nbp",
                                outcome_variable = "continuous_outcome", treatment_variable = "treatment",
                                matching_variable = c("age", "income"),
                                covariates = NULL,
                                outcome_formula = "with_matching_variables",
                                psmodel_obj = un, missing_method = "complete",
                                weighting_variable = "income")

un <- estimation_stage(.data = simulated_data$amp, missing_method = "mi", model_type = "poly",
                       treatment_variable = "treatment", matching_variable = c("age", "income"))

deux <- balance_data(counterfactual_method = "nbp", treatment_variable = "treatment",
                     matching_variable = c("age", "income"), PS_estimation_object = un,
                     missing_method = "mi")

trois <- outcome_analysis_stage(balanced_data = deux, counterfactual_method = "nbp",
                                outcome_variable = "continuous_outcome", treatment_variable = "treatment",
                                matching_variable = c("age", "income"),
                                covariates = NULL,
                                outcome_formula = "with_matching_variables",
                                psmodel_obj = un, missing_method = "mi",
                                weighting_variable = "income")

data(mtcars)
abc <- estimation_stage(.data = mtcars, missing_method = "mi", model_type = "poly",
                         treatment_variable = "gear", matching_variable = c("qsec", "hp", "disp")) 
ghi <- balance_data(counterfactual_method = "nbp", treatment_variable = "gear", 
                    matching_variable = c("qsec", "hp", "disp"), PS_estimation_object = abc,
                    missing_method = "mi")
jkl <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "nbp",
                              outcome_variable = "mpg", treatment_variable = "gear",
                              #matching_variable = c("qsec", "hp", "disp"),
                              covariates = NULL,
                              outcome_formula = "unadjusted",
                              psmodel_obj = abc, missing_method = "complete")










# setwd("~/Desktop/WT_data_prize/bin/DigiCAT/R")
# files.sources = list.files()
# setwd("~/Desktop/WT_data_prize/bin/DigiCAT/")
# sapply(paste0("R/",files.sources), source)














































