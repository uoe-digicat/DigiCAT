rowMaxs <- function(df, na.rm=TRUE) {
  
  if (is.matrix(df)) {df <- data.frame(df, stringsAsFactors=FALSE, drop = FALSE)}
  
  valid.cols <- sapply(df, function(x) { is.numeric(x) || is.logical(x) || is.character(x)})
  stopifnot(any(valid.cols))
  # or could just return NA?:
  # if (!any(valid.cols)) {return(NA)}
  if (any(!valid.cols) ) {warning('using only numeric (double or integer) or logical or character columns -- ignoring other columns ')}
  
  result <- do.call(pmax.int, c(df[ , valid.cols, drop = FALSE], na.rm=na.rm))
  
  result[nononmissing <- rowSums(!is.na(df[ , valid.cols, drop = FALSE]))==0] <- -Inf
  if (any(nononmissing)) {warning('where no non-missing arguments, returning -Inf')}
  return(result)
  
  # df = data.frame of numeric values, i.e. a list of vectors passed to pmax
  # Value returned is vector, each element is max of a row of df
}

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
             # imputed.dfs <- complete(handled_missingness, "all")
             # imputed.dfs <- lapply(imputed.dfs, function(x) data.frame(x, lp=estimated_propensity_model[[1]]$lp))
             # propensity_score <- lapply(imputed.dfs, function(x) data.frame(x, model=estimated_propensity_model[[1]]$model))
             
             propensity_score <- estimated_propensity_model
             # names(propensity_score)[names(propensity_score) == 'polly$lp'] <- 'lp'
             
           } else if (missing_method == "complete" & model_type == "poly"){
             
             
             propensity_score = as.data.frame(cbind(handled_missingness, 
                                                    #estimated_propensity_model$model, don't think this is needed
                                                    estimated_propensity_model$lp))
             names(propensity_score)[names(propensity_score) == "polly$lp"] <- "lp"
             
           } else if(missing_method == "weighting"){
             propensity_score = estimated_propensity_model$fitted.values 
             propensity_score = rowMaxs(propensity_score)
             
             propensity_score = cbind(handled_missingness$variables, propensity_score)
             # nb: will need to iterate to calculate lp as in polr - this is temporary for ps
           }
         },
         stop("I need a valid model! (glm, gbm, rforest, poly)")
         
  )
  return(propensity_score)
}






