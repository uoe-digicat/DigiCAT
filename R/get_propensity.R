get_propensity <- function(estimated_propensity_model, model_type, treatment_variable, matching_variable, complete_data,...){
  f = paste0(treatment_variable,"~",paste0(matching_variable, collapse="+"))
  
  switch(model_type, 
         
         glm = {
           if(class(complete_data) == "mids"){
             propensity_score = lapply(complete(complete_data, "all"), 
                               function(x) predict(glm(f, data = x, family=binomial(link="probit"), ...),
                                                   type = "response"))
           } else {
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
           propensity_score = as.data.frame(cbind(complete_dataset, estimated_propensity_model$model,
                                                     estimated_propensity_model$lp))
         },
         stop("I need a valid model! (glm, gbm, rforest, poly)")
         
  )
  return(propensity_score)
}