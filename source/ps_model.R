#' @param data dataset, input$rawdata
#' @param treatment treatment variable (character string)
#' @param matchvars matching variables (character vector)
#' @param model ps model choice (glm, etc)
#' @param method 1:1, k:1 etc
#' @param missingness ??
#' 
ps_model <- function(.data, treatment, matchvars=NULL, model, method, missingness){
  if(model == "Regression"){
    f = as.formula(paste0(treatment,"~",paste0(matchvars,collapse="+")))
    if(method=="1:1 PSM"){
      mout = MatchIt::matchit(f, data = .data, method="nearest",distance="glm")
    } else if(method=="K:1 PSM"){
      mout = MatchIt::matchit(f, data = .data, method="full",distance="glm")
    }
  } else { 
    mout = NULL 
  }
  return(mout)
}


