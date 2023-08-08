estimate_model <- function(handled_missingness, model_type = NULL, treatment_variable, matching_variable, 
                           missing_method,...){
  f = paste0(treatment_variable,"~",paste0(matching_variable, collapse="+"))
  
  switch(model_type, 
         
         glm = {
           if(missing_method == "mi"){
           estimated_propensity_model = lapply(complete(handled_missingness, "all"), 
                             function(x) glm(f, data = x, family=binomial(link="probit"), ...))
           } else if(missing_method == "complete"){
           estimated_propensity_model = glm(f, data = handled_missingness,
                                                      family = binomial(link="probit"),...)
           } else if(missing_method == "weighting"){
           estimated_propensity_model = svyglm(f, design = handled_missingness) 
           }
           }, 
         
         gbm = {
           
           # add in
         },
         
         rf = {
           # add in 
         },
         
         poly = {
           if(missing_method == "mi"){
             estimated_propensity_model = lapply(complete(handled_missingness, "all"),
                                                 function(x) MASS::polr(f, data = x, method = "probit",
                                                                        Hess = T, ...))
           } else if(missing_method == "complete"){
             estimated_propensity_model = MASS::polr(f, data = handled_missingness, 
                                               method = "probit", Hess = T, ...)
           }
           
         },
         stop("I need a valid model! (glm, gbm, rforest, poly)")
  )
  return(estimated_propensity_model)
}