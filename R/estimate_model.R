estimate_model <- function(complete_data, model_type = NULL, treatment_variable, matching_variable, ...){
  f = paste0(treatment_variable,"~",paste0(matching_variable, collapse="+"))
  
  switch(model_type, 
         
         glm = {
           if(class(complete_data) == "mids"){
           estimated_propensity_model = lapply(complete(complete_data, "all"), 
                             function(x) glm(f, data = x, family=binomial(link="probit"), ...))
           } else {
           estimated_propensity_model = glm(f, data = complete_data,
                                                      family = binomial(link="probit"))}
           },
         
         gbm = {
           
           # add in
         },
         
         rf = {
           # add in 
         },
         
         poly = {
           if(class(complete_data) == "mids"){
             estimated_propensity_model = lapply(complete(complete_data, "all"),
                                                 function(x) MASS::polr(f, data = x, method = "probit",
                                                                        Hess = T, ...))
           } else {
             estimated_propensity_model = MASS::polr(f, data = complete_data, 
                                               method = "probit", Hess = T, ...)
           }
           
         },
         stop("I need a valid model! (glm, gbm, rforest, poly)")
  )
  return(estimated_propensity_model)
}