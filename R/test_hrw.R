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

source("R/propensity_estimation_stage.R")
source("R/evaluate_imputations.R")
source("R/balance_data.R")
source("R/outcome_analysis_stage.R")

abc <- estimation_stage(.data = df2$amp, missing_method = "complete", model_type = "glm",
                        treatment_variable = "t", matching_variable = c("a", "b", "c", "d", "e"))
ghi <- balance_data(counterfactual_method = "psm", treatment_variable = "t", 
                    matching_variable = c("a", "b", "c", "d", "e"), psmodel_obj = abc,
                    missing_method = "complete")
mno <- outcome_analysis_stage(balanced_data = ghi, counterfactual_method = "psm", 
                              outcome_variable = "y",
                              treatment_variable = "t", 
                              matching_variable = c("a", "b"), 
                              psmodel_obj = abc)


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

df2 = mice::ampute(d,
                   prop = 0.05)

abc <- estimation_stage(.data = df2$amp, missing_method = "weighting", model_type = "glm",
                        treatment_variable = "A", matching_variable = c("X1", "X2", "X3"),
                        non_response_weights = SW)
ghi <- balance_data(counterfactual_method = "psm", treatment_variable = "A", 
                    matching_variable = c("X1", "X2", "X3"), psmodel_obj = abc,
                    missing_method = "weighting")

















