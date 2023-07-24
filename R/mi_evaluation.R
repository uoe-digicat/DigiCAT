evaluate_imputations <- function(mice_output, evaluation_method, graph_display, .data, output,...){
  switch(evaluation_method, 
         distributional_discrepancy = {
           evaluation = check_distributional_discrepancy(mice_output, graph_display)
         },
         convergence = {
           evaluation = check_convergence(mice_output)
         },
         residuals_imputed_outcome = {
           evaluation = check_residuals_imputed_outcome_analysis(mice_output)
         },
         stop("I need a valid method! (distributional discrepancy, convergence, residuals)")
  )
  return(evaluation)
}

check_distributional_discrepancy <- function(mice_output, graph_display, .data, # make an option for 4 plots in one pane?
                                             ...){ # make an option for a table of obs vs imp M/SD?
  if(graph_display=="strip") {
    evaluation = stripplot(mice_output, pch = c(21, 20), cex = c(1, 1.5))
  } else if(graph_display=="whiskers"){
    evaluation = bwplot(mice_output)
  } else if(graph_display=="density"){
    evaluation = densityplot(mice_output)
  }
}

check_convergence <- function(mice_output){
  evaluation = plot(mice_output)
}  

check_residuals_imputed_outcome_analysis <- function(mice_output){
  mean_pooled_residual1=NULL
  predicted_mean_pooled_residual1=NULL
  
  for(i in 1:length(mice_output[[2]])){
    mean_pooled_residual1=rbind(mean_pooled_residual1,residuals(mice_output[[2]][[i]]))
    mean_pooled_residual=colMeans(mean_pooled_residual1)
    predicted_mean_pooled_residual1=rbind(predicted_mean_pooled_residual1,predict(mice_output[[2]][[i]])) 
    predicted_mean_pooled_residual=colMeans(predicted_mean_pooled_residual1)}
  
  evaluation = plot(mean_pooled_residual,predicted_mean_pooled_residual)
}