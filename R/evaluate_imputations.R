# todo - add residuals of imputation models
# here or in handle_missingness() - if common error, e.g., cat data -> default to more robust eg pmm
# if convergence = bad, advise to increase maxit and re-examine
# % missingness calc to inform m as per v Hippel formula?
# predmatrix = allows inspection of collinearity removals also

evaluate_imputations <- function(estimation_model_object, evaluation_method, graph_display,
                                 .data,...){
  switch(evaluation_method, 
         distributional_discrepancy = {
           evaluation = check_distributional_discrepancy(estimation_model_object, graph_display)
         },
         convergence = {
           evaluation = check_convergence(estimation_model_object)
         },
         eventslog = {
           evaluation = estimation_model_object$missingness_treated_dataset$loggedEvents
         },
         inspect_matrix = { 
           evaluation = estimation_model_object$missingness_treated_dataset$predictorMatrix
         },
         LittleMCARtest = {
           evaluation = naniar::mcar_test(.data) # typically ran before MI - put in pre-handling function?
           # null = MCAR; if p = low -> cannot assume MCAR
         },
         missing_pattern = {
           evaluation = mice::md.pattern(.data) # typically ran before MI - put in pre-handling function?
           # use to inform imp model; 1/blue = observed, 0/red = missing
           # if want tabulation, set plot = FALSE, else TRUE by default
           # primarily useful for data with small N(cols)
         },
         influx_outflux = {
           evaluation = mice::flux(.data)
         },
         # residuals = { # TO DO
         #   evaluation = 
         # },
         stop("I need a valid method! (distributional discrepancy, convergence,
              logged events, matrix inspection, Little's MCAR test, missing pattern)")
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

