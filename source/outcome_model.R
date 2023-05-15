#' @param data dataset, input$rawdata
#' @param outcome outcome variable, (character string)
#' @param treatment treatment variable (character string)
#' @param covars covariates (character vector)
#' @param PSmodel model object of type: ??? 
#' @param method input$cfmethod 1:1, k:1, iptw, etc. 
#' @param missingness ??
#' 
outcome_model <- function(.data, outcome, treatment, covars=NULL,matchvars, PSmodel, method, missingness,doubly = TRUE){
  if(doubly){
    if(!is.null(covars)){
      f = as.formula(paste0(outcome,"~",treatment,"*(",paste0(matchvars, covars, collapse="+"), ")"))
    } else{
      f = as.formula(paste0(outcome,"~",treatment,"*(",paste0(matchvars, collapse="+"), ")"))
    }
  } else {
    if(!is.null(covars)){
      f = as.formula(paste0(outcome,"~",treatment,"*(",paste0(covars, collapse="+"), ")"))
    } else{
      f = as.formula(paste0(outcome,"~",treatment))
    }
  }
  if(method %in% c("1:1 PSM","K:1 PSM")){
    psmdata = MatchIt::match.data(PSmodel)
    m = lm(f, weights = weights, data = psmdata)
    effest = marginaleffects::avg_comparisons(m, variables = treatment, vcov = ~subclass, wts = "weights")
    effplot = marginaleffects::plot_comparisons(m, variables = treatment, vcov = ~subclass,wts = "weights") +
      geom_vline(xintercept=0, lty="dotted")
  } else {
    # todo
  }
  return(list(mod=m,est=effest, plot=effplot))
}
