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
# to do - edit argument input
evaluate_imputations(.data = df2$amp, evaluation_method = "LittleMCARtest")
evaluate_imputations(.data = df2$amp, evaluation_method = "missing_pattern")
evaluate_imputations(.data = df2$amp, evaluation_method = "influx_outflux")

abc <- estimation_stage(.data = df2$amp, missing_method = "complete", model_type = "glm",
                        treatment_variable = "t", matching_variable = c("a", "b")) 
evaluate_imputations(abc, "distributional_discrepancy", "strip")
evaluate_imputations(abc, "convergence") # include guidance line as output maybe
evaluate_imputations(abc, "eventslog") # depending on logged events, recommend altering parameters xyz accordingly
evaluate_imputations(abc, "inspect_matrix")

ghi <- balance_data(counterfactual_method = "psm", treatment_variable = "t", 
                    matching_variable = c("a", "b"), PS_estimation_object = abc,
                    missing_method = "complete")
mno <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "psm", 
                              outcome_variable = "y",
                              treatment_variable = "t", 
                              matching_variable = c("a", "b"), 
                              psmodel_obj = abc,
                              missing_method = "mi",
                              outcome_formula = "unadjusted",
                              covariates = NULL)

#### cc

abc <- estimation_stage(.data = df2$amp, missing_method = "complete", model_type = "glm",
                        treatment_variable = "t", matching_variable = c("a", "b")) 
evaluate_propensity_stage(abc, evaluation_method = "support")
ghi <- balance_data(counterfactual_method = "iptw", treatment_variable = "t", 
                    matching_variable = c("a", "b"), PS_estimation_object = abc,
                    missing_method = "complete")
mno <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "iptw", 
                              outcome_variable = "y",
                              treatment_variable = "t", 
                              matching_variable = c("a", "b"), 
                              psmodel_obj = abc,
                              missing_method = "complete")

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
ghi <- balance_data(counterfactual_method = "psm", treatment_variable = "A", 
                    matching_variable = c("X1", "X2"), PS_estimation_object = abc,
                    missing_method = "weighting")

## TO DO - edit design object in extract_balanced_data

mno <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "iptw", # outcome format needs changing for weighting approach
                              outcome_variable = "Y_C",
                              treatment_variable = "A", 
                              matching_variable = c("X1", "X2"), 
                              psmodel_obj = abc,
                              missing_method = "weighting",
                              weighting_variable = "SW")

# testing weights with example sets 

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

## TO DO - edit design object in extract_balanced_data
mno <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "psm", 
                              outcome_variable = "RIAGENDR",
                              treatment_variable = "HI_CHOL", 
                              matching_variable = "race", 
                              psmodel_obj = abc,
                              missing_method = "weighting",
                              weighting_variable = "WTMEC2YR")

evaluate_propensity_stage(abc, "support", missing_method = "weighting")

### I think something is off with the weighting calculation in extract_balanced_data
# when svy object is re-incorporated
# Look into

#### NBP testing ####
# random data - ignore
data(mtcars)
abc <- estimation_stage(.data = mtcars, missing_method = "complete", model_type = "poly",
                         treatment_variable = "gear", matching_variable = c("qsec", "hp", "disp")) 
ghi <- balance_data(counterfactual_method = "nbp", treatment_variable = "gear", 
                    matching_variable = c("qsec", "hp", "disp"), PS_estimation_object = abc,
                    missing_method = "complete")
jkl <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "nbp",
                              outcome_variable = "mpg", treatment_variable = "gear",
                              #matching_variable = c("qsec", "hp", "disp"),
                              covariates = NULL,
                              outcome_formula = "unadjusted",
                              psmodel_obj = abc, missing_method = "complete")




setwd("~/Desktop/WT_data_prize/bin/DigiCAT/R")
files.sources = list.files()
setwd("~/Desktop/WT_data_prize/bin/DigiCAT/")
sapply(paste0("R/",files.sources), source)



















































# Load necessary libraries
library(nbpMatching)
library(MatchIt)  # for the matchit function
library(MASS)  # for the truehist function

# Set a seed for reproducibility
set.seed(123)

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

un <- estimation_stage(.data = simulated_data, missing_method = "complete", model_type = "poly",
                       treatment_variable = "treatment", matching_variable = c("age", "income"))
deux <- balance_data(counterfactual_method = "nbp", treatment_variable = "treatment",
                     matching_variable = c("age", "income"), PS_estimation_object = un,
                     missing_method = "complete")
contrasts(deux$treatment) <- contr.treatment(2, base = 2)
trois <- outcome_analysis_stage(balanced_data = deux, counterfactual_method = "nbp",
                                outcome_variable = "continuous_outcome", treatment_variable = "treatment",
                                matching_variable = c("age", "income"),
                                covariates = NULL,
                                outcome_formula = "unadjusted",
                                psmodel_obj = un, missing_method = "complete")


# Set seed for reproducibility
set.seed(123)

# Number of simulations
num_simulations <- 10

# Storage for results
results <- vector("list", length = num_simulations)

# Run simulations
for (i in 1:num_simulations) {
  # Simulate data
  n <- 500
  weight <- rnorm(n, mean = 70, sd = 10)
  true_effect <- 5
  treatment <- sample(1:5, n, replace = TRUE)
  
  # Apply the true effect to the highest treatment level
  weight[treatment == 3] <- weight[treatment == 3] + true_effect
  
  gender <- sample(c("Male", "Female"), n, replace = TRUE)
  age <- rnorm(n, mean = 40, sd = 5)
  
  # Combine simulated data into a data frame
  simulated_data <- data.frame(
    weight = weight,
    treatment = as.factor(treatment),
    gender = as.factor(gender),
    age = age)
    
    un <- estimation_stage(.data = simulated_data, missing_method = "complete", model_type = "poly",
                           treatment_variable = "treatment", matching_variable = c("gender", "age"))
    deux <- balance_data(counterfactual_method = "nbp", treatment_variable = "treatment",
                       matching_variable = c("gender", "age"), PS_estimation_object = un,
                       missing_method = "complete")
    contrasts(deux$treatment) <- contr.treatment(2, base = 2)
    trois <- outcome_analysis_stage(balanced_data = deux, counterfactual_method = "nbp",
                                 outcome_variable = "weight", treatment_variable = "treatment",
                                 matching_variable = c("gender", "age"),
                                 covariates = NULL,
                                 outcome_formula = "unadjusted",
                                 psmodel_obj = un, missing_method = "complete")
  
  # Perform analysis
  model <- lm(weight ~ treatment + gender + age, data = deux)
  print(summary(model))
  
  # Store results
  results[[i]] <- coef(model)[2]  # Extract the coefficient for treatment (high)
}

# Analyze the distribution of results
summary(unlist(results))
hist(unlist(results), main = "Distribution of Treatment Effects", xlab = "Treatment Effect")



# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 1000

# Simulate continuous outcome (e.g., weight)
weight <- rnorm(n, mean = 70, sd = 10)

# True effect size for the treatment variable
true_effect <- 3

# Simulate ordinal treatment variable (e.g., drug dosage with 3 levels)
treatment <- sample(1:3, n, replace = TRUE)

# Apply the true effect to the highest treatment group
weight[treatment == 3] <- weight[treatment == 3] + true_effect

# Simulate binary matching variable (e.g., gender)
gender <- sample(c("Male", "Female"), n, replace = TRUE)

# Simulate continuous matching variable (e.g., age)
age <- rnorm(n, mean = 40, sd = 5)

# Combine simulated data into a data frame
simulated_data <- data.frame(
  weight = weight,
  treatment = as.factor(treatment),
  gender = as.factor(gender),
  age = age
)


# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 200

# Simulate continuous outcome (e.g., weight)
weight <- rnorm(n, mean = 70, sd = 10)

# True effect size for the treatment variable
true_effect <- 7

# Simulate ordinal treatment variable (e.g., drug dosage with 3 levels)
treatment <- sample(1:3, n, replace = TRUE)

# Apply the true effect to the treatment group
weight[treatment == 2] <- weight[treatment == 2] + true_effect

# Simulate matching variable (e.g., height)
height <- rnorm(n, mean = 170, sd = 10)

# Combine simulated data into a data frame
simulated_data <- data.frame(
  weight = weight,
  treatment = as.factor(treatment),
  height = height
)

# Display the first few rows of the simulated data
head(simulated_data)

un <- estimation_stage(.data = simulated_data, missing_method = "complete", model_type = "poly",
                       treatment_variable = "treatment", matching_variable = "height")
deux <- balance_data(counterfactual_method = "nbp", treatment_variable = "treatment",
                     matching_variable = "height", PS_estimation_object = un,
                     missing_method = "complete")
contrasts(deux$treatment) <- contr.treatment(2, base = 2)
trois <- outcome_analysis_stage(balanced_data = deux, counterfactual_method = "nbp",
                                outcome_variable = "weight", treatment_variable = "treatment",
                                matching_variable = "height",
                                covariates = NULL,
                                outcome_formula = "unadjusted",
                                psmodel_obj = un, missing_method = "complete")
