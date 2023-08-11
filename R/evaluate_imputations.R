evaluate_imputations <- function(estimation_model_object, evaluation_method, graph_display,...){
  switch(evaluation_method, 
         distributional_discrepancy = {
           evaluation = check_distributional_discrepancy(estimation_model_object, graph_display)
         },
         convergence = {
           evaluation = check_convergence(estimation_model_object)
         },
         eventslog = {
           evaluation = estimation_model_object$loggedevents
         },
         inspect_matrix = {
           evaluation = estimation_model_object$predictorMatrix
         },
         stop("I need a valid method! (distributional discrepancy, convergence)")
  )
  return(evaluation)
}

check_distributional_discrepancy <- function(estimation_model_object, graph_display, .data, # make an option for 4 plots in one pane?
                                             ...){ # make an option for a table of obs vs imp M/SD?
  if(graph_display=="strip") {
    evaluation = stripplot(estimation_model_object$missingness_treated_dataset, pch = c(21, 20), cex = c(1, 1.5))
  } else if(graph_display=="whiskers"){
    evaluation = bwplot(estimation_model_object$missingness_treated_dataset)
  } else if(graph_display=="density"){
    evaluation = densityplot(estimation_model_object$missingness_treated_dataset)
  }
}

check_convergence <- function(estimation_model_object){
  evaluation = plot(estimation_model_object$missingness_treated_dataset)
}  

