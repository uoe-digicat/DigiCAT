estimate_model <- function(handled_missingness, model_type = NULL, treatment_variable, matching_variable, 
                           missing_method,...){
  
  if (model_type == "gbm" | model_type == "glm" | model_type == "lm"){
    f = paste0("as.numeric(as.character(", treatment_variable,")) ~",paste0(matching_variable, collapse="+"))
  } else if (model_type == "poly" | model_type == "randomforest"){
    f = as.formula(paste0("as.factor(", treatment_variable,") ~",paste0(matching_variable, collapse="+")))
  }
  
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
           if(missing_method == "mi"){
             estimated_propensity_model = lapply(complete(handled_missingness, "all"), # 
                                                 function(x) gbm(as.formula(f), data = x, ...)) 
           }
           if(missing_method == "complete"){
             estimated_propensity_model = gbm(as.formula(f), data = handled_missingness,...) #
           }
         },
         
         randomforest = {
           if(missing_method == "mi"){
             estimated_propensity_model = lapply(complete(handled_missingness, "all"), # switch to ranger for comp speed
                                                 function(x) randomForest::randomForest(as.formula(f), data = x, ...)) 
           }
           if(missing_method == "complete"){
             estimated_propensity_model <- randomForest::randomForest(as.formula(f), data = handled_missingness, ...) # switch to ranger for comp speed
           }
         },
         
         poly = {
           if(missing_method == "mi"){
             
             # estimated_propensity_model = lapply(complete(handled_missingness, "all"),
             #                                    function(x) MASS::polr(f, data = x, Hess =T,...))
             
             comp <- mice::complete(handled_missingness, "long", include = TRUE)
             comp[[treatment_variable]] <- as.factor(comp[[treatment_variable]])
             handled_missingness <- as.mids(comp)
             comp <- mice::complete(handled_missingness, "all", include = FALSE)
             
             estimated_propensity_model <- data.frame()
             
             for (i in 1:length(comp)){
               
               polly <- MASS::polr(f, data = comp[[i]],
                                   Hess=T)
               
               res <- as.data.frame(cbind(comp[[i]], 
                                          polly$model,
                                          polly$lp))
               
               res$impset <- i
               
               estimated_propensity_model <- rbind(estimated_propensity_model,res)
               
             }
             
           }else if(missing_method == "complete"){
             handled_missingness[[treatment_variable]] <- as.factor(handled_missingness[[treatment_variable]])
             estimated_propensity_model = MASS::polr(f, data = handled_missingness, 
                                                     Hess = T, ...)
           } else if(missing_method == "weighting"){
             handled_missingness[[7]][[treatment_variable]] <- as.factor(handled_missingness[[7]][[treatment_variable]])
             estimated_propensity_model = svyolr(f, design=handled_missingness)
             
           }
           
         },
         lm = {
           if(missing_method == "mi"){
             estimated_propensity_model = lapply(complete(handled_missingness, "all"), 
                                                 function(x) glm(f, data = x, family = gaussian(link = "identity"), ...))
           } else if(missing_method == "complete"){
             estimated_propensity_model = glm(f, data = handled_missingness,
                                              family = gaussian(link = "identity"),...)
           } else if(missing_method == "weighting"){
             estimated_propensity_model = svyglm(f, design = handled_missingness) 
           }
         },
         
         stop("I need a valid model! (glm, gbm, randomforest, poly, lm)")
  )
  return(estimated_propensity_model)
}

