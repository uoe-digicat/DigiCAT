#' DigiCAT balancing model performance plot function
#' @import ggplot2
#' @param psmodel_obj an object obtained from get_score(). this is required for matching and iptw methods, and is a list containing data, model, propensity score, and string indicating class of model. note the first 3 of these may be lists if multiple imputation has been used.#' 
#' @param t_var name of treatment variable in dataset (character string)
#' @param treattype character string identifying what type of variable treatment is (binary, ordinal etc). only binary implemented thus far.
performance_plot <- function(psmodel_obj, t_var, treattype = "binary"){

  if(any(class(psmodel_obj$missingness_treated_dataset)=="mids")){
    obs = lapply(mice::complete(psmodel_obj$missingness_treated_dataset, "all"), function(d) d[,t_var])
    pred = psmodel_obj$propensity_scores
    
    switch(treattype,
           
           binary = {
             # order for by pred for thresholds
             obs2 = lapply(1:length(obs), function(m) obs[[m]][order(pred[[m]])])
             
             for(m in 1:length(obs2)){
               pos = sum(obs2[[m]]); neg = sum(!obs2[[m]])
               # TPR
               tpr = (sum(obs2[[m]]) - cumsum(obs2[[m]])) / pos
               # TNR
               tnr = cumsum(!obs2[[m]]) / neg
               df <- data.frame(x = 1-tnr, y = tpr)
               if(m==1){
                 p <- ggplot(df, aes(x=x, y=y)) +
                   geom_line(size = 0.5, alpha = 0.5) +
                   xlab("False Positive Rate") + ylab("True Positive Rate") + 
                   geom_abline(intercept = 0, slope = 1, linewidth = 0.5, alpha = 0.3, linetype = "dashed", colour = "blue") + ## Add diagonal dashed line
                   theme_classic() + ## Add minimal theme
                   scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) ## Origin to start at 0
                 
                 
                 AUC_measures <- round(sum(tnr*diff(c(0,1-tpr))),2)
    
               }else{
                 p <- p + geom_line(data = df, aes(x = x , y = y), size = 0.5, alpha = 0.5)
                 
                 AUC_measures <- c(AUC_measures, round(sum(tnr*diff(c(0,1-tpr))),2))
               }
             }
             p + annotate("text", x = 0.7, y = 0.3, label = paste0("AUC = ", paste0(AUC_measures, collapse = ", ")), colour = "blue", fontface = 1) ## Add AUC
           },
           
           ordinal = {
             cat("nope. not yet.")
           }
           
    )
  }
  if(any(class(psmodel_obj$missingness_treated_dataset)=="survey.design")){
    
    obs = psmodel_obj$survey_design_object$variables[,t_var]
    pred = psmodel_obj$propensity_scores
    
    switch(treattype,
           
           binary = {
             
             # order for by pred for thresholds
             obs2 = obs[order(pred)]
             # P and N
             pos = sum(obs2); neg = sum(!obs2)
             # TPR
             tpr = (sum(obs2) - cumsum(obs2)) / pos
             # TNR
             tnr = cumsum(!obs2) / neg
             df <- data.frame(x = 1-tnr, y = tpr)
             ggplot(df, aes(x=x, y=y)) +
               geom_line(size = 0.5, alpha = 0.5) +
               xlab("False Positive Rate") + ylab("True Positive Rate") + 
               annotate("text", x = 0.65, y = 0.5, label = paste0("AUC = ", round(sum(tnr*diff(c(0,1-tpr))),2)), colour = "blue", fontface = 1) + ## Add AUC
               geom_abline(intercept = 0, slope = 1, linewidth = 0.5, alpha = 0.3, linetype = "dashed", colour = "blue") + ## Add diagonal dashed line
               theme_classic() + ## Add minimal theme
               scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) ## Origin to start at 0
           },
           
           ordinal = {
             cat("nope. not yet.")
           }
           
    )
    
  }
  else{
    obs = psmodel_obj$missingness_treated_dataset[,t_var]
    pred = psmodel_obj$propensity_scores
    
    switch(treattype,
           
           binary = {
             
             # order for by pred for thresholds
             obs2 = obs[order(pred)]
             # P and N
             pos = sum(obs2); neg = sum(!obs2)
             # TPR
             tpr = (sum(obs2) - cumsum(obs2)) / pos
             # TNR
             tnr = cumsum(!obs2) / neg
             df <- data.frame(x = 1-tnr, y = tpr)
             ggplot(df, aes(x=x, y=y)) +
               geom_line(size = 0.5, alpha = 0.5) +
               xlab("False Positive Rate") + ylab("True Positive Rate") + 
               annotate("text", x = 0.65, y = 0.5, label = paste0("AUC = ", round(sum(tnr*diff(c(0,1-tpr))),2)), colour = "blue", fontface = 1) + ## Add AUC
               geom_abline(intercept = 0, slope = 1, linewidth = 0.5, alpha = 0.3, linetype = "dashed", colour = "blue") + ## Add diagonal dashed line
               theme_classic() + ## Add minimal theme
               scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) ## Origin to start at 0
               
           },
           
           ordinal = {
             cat("nope. not yet.")
           }
           
    )
  }
}
