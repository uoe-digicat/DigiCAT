get_propensity <- function(estimated_propensity_model, model_type, treatment_variable, matching_variable, 
                           handled_missingness, missing_method, ...){
  f = paste0(treatment_variable,"~",paste0(matching_variable, collapse="+"))
  
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
           
           # add in
         },
         
         rf = {
           # add in 
         },
         
         poly = { # complete case is below, add edited version for MI
           propensity_score = as.data.frame(cbind(handled_missingness$variables, estimated_propensity_model$model,
                                                     estimated_propensity_model$lp))
         },
         stop("I need a valid model! (glm, gbm, rforest, poly)")
         
  )
  return(propensity_score)
}