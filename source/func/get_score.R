require(lavaan)
require(mice)

#' @param psmodel class of model used to estimate propensity scores. character string, one of `"glm"` (default), `"gbm"`, `"rforest"`.
#' @param .data dataset including treatment variable and any matching variables 
#' @param t_var name of treatment variable in dataset (character string)
#' @param y_var name of outcome variable in dataset (character string)
#' @param m_vars vector of matching variable names in dataset
#' @param covars vector of covariate names in dataset
#' @param missing method by which missing data is handled, one of `"complete"` (complete cases), `"mi"` (multiple imputation), or `"fiml"` (full information maximum likelihood). TODO fiml not implemented bcause lavaan  
#' @param ... additional arguments to be passed to model estimating functions
get_score <- function(psmodel = "glm", .data, t_var = NULL, y_var = NULL, m_vars = NULL, covars = NULL, missing, ...){

  # 1. glm
  # 2. CART
  # 3. rforest
  # 4. gbm
  
  # return(dist) # either vector of pscores or matrix of pairwise distances

  switch(psmodel, 
         glm = {
           res = get_score_glm(.data, t_var, y_var, m_vars, covars, missing, ...)
         },
         gbm = {
           cat("GBM not yet implemented, reverting to GLM")
         },
         rforest = {
           cat("RFOREST not yet implemented, reverting to GLM")
         },
         stop("I need a valid model! (glm, gbm, rforest)")
  )
  return(res)
}




get_score_glm <- function(.data, t_var = NULL, y_var = NULL, m_vars = NULL, covars = NULL, missing = NULL, ...){

  # glm formula:  
  f = paste0(t_var,"~",paste0(m_vars,collapse="+"))
  
  switch(missing, 

         complete = {
           
           ps_data = na.omit(.data[,unique(c(t_var, y_var, m_vars, covars))])
           
           ps_mod = sem(f, ps_data, link="probit", ordered = c(t_var), ...)
           # need to work out how to write stuff to a file for 'show me the code'. something like: 
           # expr(sem(!!f, data=!!substitute(.data)), link = "probit", c(ordered = !!t_var))
           # TODO getting pred probs from lavaan is weirdly not available. this works, but needs improving
           ps_score = pnorm(ps_mod@Data@eXo[[1]] %*% coef(ps_mod)[1:ncol(ps_mod@Data@eXo[[1]])]-coef(ps_mod)[ncol(ps_mod@Data@eXo[[1]])+1])
         },
         
         fiml = {
           
           ps_data = .data
           ps_mod = sem(f, ps_data, missing = "ML", link="probit", ordered = c(t_var), ...)
           # TODO fiml not available with categorical y, so what is the thinking here? 
           ps_score = pnorm(ps_mod@Data@eXo[[1]] %*% coef(ps_mod)[1:ncol(ps_mod@Data@eXo[[1]])]-coef(ps_mod)[ncol(ps_mod@Data@eXo[[1]])+1])
           
         },
         
         mi = {
           
           # TODO what do we use for m? do we allow user config? 
           ps_data = mice(.data, m = 5)
           # TODO we don't actually _NEED_ these eval'd here, because matchthem is going to do this step in balancing()
           ps_mod = lapply(complete(ps_data, "all"), 
                           function(x) glm(as.formula(f), data = x, family=binomial(link="probit"), ...))
           ps_score = lapply(complete(ps_data, "all"), 
                             function(x) predict(glm(as.formula(f), data = x, family=binomial(link="probit"), ...), type = "response"))
         },
         stop("How should i deal with missingness? Should be one of 'complete', 'fiml', 'mi'")
  )
  return(list(data = ps_data, mod = ps_mod, score = ps_score, ps_modclass = "glm"))
}


