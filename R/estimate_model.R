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
           if(missing_method == "mi"){
             estimated_propensity_model = lapply(complete(handled_missingness, "all"), # switch to lightgbm for comp speed
                                                 function(x) gbm(f, data = x, ...)) 
           }
           if(missing_method == "complete"){
             estimated_propensity_model <- gbm(f, data = handled_missingness,...) # switch to lightgbm for comp speed
           }
         },
         
         rf = {
           if(missing_method == "mi"){
             estimated_propensity_model = lapply(complete(handled_missingness, "all"), # switch to ranger for comp speed
                                                 function(x) randomForest(f, data = x, ...)) 
           }
           if(missing_method == "complete"){
             estimated_propensity_model <- randomForest(f, data = handled_missingness,...) # switch to ranger for comp speed
           }
         },
         
         poly = {
           if(missing_method == "mi"){
             
             # estimated_propensity_model = lapply(complete(handled_missingness, "all"),
             #                                    function(x) MASS::polr(f, data = x, Hess =T,...))
                                
             comp <- complete(handled_missingness, "all", include = FALSE)
             estimated_propensity_model <- data.frame()

             for (i in 1:length(comp)){

               polly <- MASS::polr(gear~disp+qsec, data = comp[[i]],
                                   Hess=T)

               res <- as.data.frame(cbind(comp[[i]], polly$model,polly$lp))

               res$impset <- i

               estimated_propensity_model <- rbind(estimated_propensity_model,res)

             }
             
           }else if(missing_method == "complete"){
             handled_missingness[[treatment_variable]] <- as.factor(handled_missingness[[treatment_variable]])
             estimated_propensity_model = MASS::polr(f, data = handled_missingness, 
                                               Hess = T, ...)
           } else if(missing_method == "weighting"){
             handled_missingness[[treatment_variable]] <- as.factor(handled_missingness[[treatment_variable]])
             estimated_propensity_model = svyolr(f, design=handled_missingness)
             
           }
           
         },
         stop("I need a valid model! (glm, gbm, rforest, poly)")
  )
  return(estimated_propensity_model)
}
