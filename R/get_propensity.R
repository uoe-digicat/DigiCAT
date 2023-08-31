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
         
         poly = { 
           if(missing_method == "mi" & model_type == "poly"){
             imputed.dfs <- complete(handled_missingness, "all")
             imputed.dfs <- lapply(imputed.dfs, function(x) data.frame(x, lp=estimated_propensity_model[[1]]$lp))
             propensity_score <- lapply(imputed.dfs, function(x) data.frame(x, model=estimated_propensity_model[[1]]$model))
             
             #propensity_score <- lapply(imputed.dfs, function(x) data.frame(x, impset="i"))
             
             # propensity_score$impset <- i

           } else if (missing_method == "complete" & model_type == "poly"){
           
           
           propensity_score = as.data.frame(cbind(handled_missingness, 
                                                  #estimated_propensity_model$model, don't think this is needed
                                                     estimated_propensity_model$lp))
           names(propensity_score)[names(propensity_score) == "estimated_propensity_model$lp"] <- "lp"

           } else if(missing_method == "weighting"){
            propensity_score = estimated_propensity_model$fitted.values 
            # is this correct? Polr vs Svyolr difference in output - no lp?
          }
         },
         stop("I need a valid model! (glm, gbm, rforest, poly)")
         
  )
  return(propensity_score)
}








