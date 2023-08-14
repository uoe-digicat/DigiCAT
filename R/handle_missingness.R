# function evaluate_imputations contains diagnostics post-imputation
# and useful functions for pre-missing data handling (need to move to new fun)

require(mice)

handle_missingness <- function(.data, missing_method = NULL,
                               design_object = NULL,
                                ...){
  switch(missing_method, 
         
         complete = {
           
           handled_missingness = na.omit(.data)
           },
         
         mi = {
           
           handled_missingness = mice(.data, m = 5, maxit = 20) # default options
           # default to include interaction in substantive model?
           # unless user uploads analysis-specific subset of data, will aim to include all vars, auxiliaries, predictors of missingness
           # allow user to alter m & maxit according to FMI & convergence
           # to do: add interaction term (methods = SMCFCS, passive, JAV, impute then transform) 
           # outlined below in pseudo
           # allow option or pick method with best coverage versus efficiency trade-off?
           # spec. may vary depending on variable type - continuous, cat., etc.
           # if data str is long -> multilevel, else regular with 4 default algorithms
           # if inclusion of cat data = invalid imps, coerce to robust pmm algorithm 
         },
         
         weighting = {
           
           handled_missingness = design_object # currently treats non-response and sampling the same
         },
         stop("How should i deal with missingness? Should be one of 'mi', 'complete', 'weighting'")
  )
  return(handled_missingness) 

}






# smcfcs 
# determine method default:
# if conditions - if ordinal (podds), unordered (mlogit), binary (logreg), norm (normal linear reg)
# imps <- smcfcs(data, smtype="lm", smformula="y~z*(x)", m, numit)
# method=c("","")) # as defined by class(data$variable) default
# fit <- lapply(imps$impDatasets, lm, formula = y ~ z*(x))
# summary(pool(fit))



# passive imputation for interaction term
# expr <- expression((treatment_variable - mean(treatment_variable)) * # if both cont
#                      ((matching_variables - mean(matching_variables) + 
#                          covariates - mean(covariates) )))
# data$tr.match.cov.int <- with(data, eval(expr))
# meth <- make.method(data)
# meth["tr.match.cov.int"] <- paste("~I(", expr, ")", sep - "")
# pred <- make.predictorMatrix(data)
# pred[c("treatment_variable", "matching_variables/covariates"), "tr.match.cov.int"] <- 0
# imp.int <- mice(data, m = 5, meth = meth, pred = pred, print = FALSE,
#                 maxit = 20)




















