#' @param psmodel_obj an object obtained from get_score(). this is required for matching and iptw methods, and is a list containing data, model, propensity score, and string indicating class of model. note the first 3 of these may be lists if multiple imputation has been used.
#' @param .data dataset including treatment variable and any matching variables 
#' @param t_var name of treatment variable in dataset (character string)
#' @param treattype character string identifying what type of variable treatment is (binary, ordinal etc). only binary implemented thus far. 
performance_plot <- function(psmodel_obj, .data, t_var, treattype = "binary"){
  
  obs = .data[,t_var]
  pred = psmodel_obj$score
  
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
           plot(x = 1-tnr, y = tpr, type = "l", 
                xlab = "False Positive Rate",
                ylab = "True Positive Rate", 
                main = paste0("AUC = ", round(sum(tnr*diff(c(0,1-tpr))),2))
           )
         },
         
         ordinal = {
           cat("nope. not yet.")
         }
         
  )
}
