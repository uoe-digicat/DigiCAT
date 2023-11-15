calculate_ordered_logistic_linear_predictor <- function(formula, data,
                                                        design_object) {
  # Fit ordered logistic regression model 
  model <- svyolr(formula, design = design_object)
  
  # Extract coefficients
  coefficients <- coef(model)
  
  # Initialise linear predictor
  propensity_score <- 0  # Zero for the intercept
  
  # Extract predictor variables from df
  predictors <- all.vars(formula)[-1]  # Exclude the outcome variable
  
  # Calculate linear predictor
  for (predictor in predictors) {
    propensity_score <- propensity_score + coefficients[predictor] * data[[predictor]]
  }
  
  return(propensity_score)
}

get_propensity <- function(estimated_propensity_model, model_type, treatment_variable, matching_variable, 
                           handled_missingness, missing_method,...){
  if(model_type != "poly"){
    f = paste0(treatment_variable,"~",paste0(matching_variable, collapse="+"))
  } else {
    f = as.formula(paste0("as.factor(", treatment_variable,") ~",paste0(matching_variable, collapse="+")))
  }
  
  switch(model_type, 
         
         glm = {
           if(missing_method == "mi"){
             propensity_score = lapply(complete(handled_missingness, "all"), 
                                       function(x) predict(glm(f, data = x, family=binomial(link="probit"), ...),
                                                           type = "response"))
           } else { # for CC and weighting approaches alike
             propensity_score = estimated_propensity_model$fitted.values 
           } 
         },
         
         gbm = {
           if(missing_method == "mi"){
             propensity_score = lapply(complete(handled_missingness, "all"), 
                                       function(x) predict(gbm(as.formula(f), data = x, ...),
                                                           type = "response"))  
           } else {
             propensity_score = predict(estimated_propensity_model)
           }
         },
         
         rf = {
           if(missing_method == "mi"){
             propensity_score = lapply(complete(handled_missingness, "all"), 
                                       function(x) predict(randomForest(as.formula(f), data = x, ...),
                                                           type = "response"))    
           } else{
             propensity_score = predict(estimated_propensity_model, type = "response")
           }
         },
         
         poly = { 
           if(missing_method == "mi" & model_type == "poly"){
             propensity_score <- estimated_propensity_model

           } else if (missing_method == "complete" & model_type == "poly"){
             
             
             propensity_score = as.data.frame(cbind(handled_missingness, 
                                                    #estimated_propensity_model$model, don't think this is needed
                                                    estimated_propensity_model$lp))
             names(propensity_score)[names(propensity_score) == "polly$lp"] <- "lp"
             
           } else if(missing_method == "weighting" & model_type == "poly"){
             
             propensity_score <- calculate_ordered_logistic_linear_predictor(formula = f, 
                                                                             data = handled_missingness$variables,
                                                                             design_object = handled_missingness)
             propensity_score <- cbind(handled_missingness$variables, propensity_score)
           }
         },
         stop("I need a valid model! (glm, gbm, rf, poly)")
         
  )
  return(propensity_score)
}


