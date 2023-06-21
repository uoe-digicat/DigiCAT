### EXAMPLE DATA
# y~t, matched on a,b,c,d,e
N=500
# covars
A = matrix(runif(5^2)*2-1, ncol=5)
Xmat = MASS::mvrnorm(N, mu=rnorm(5,0,3), Sigma = t(A)%*%A)
lp = apply(Xmat,2,scale) %*% rnorm(5,0,2) # propensity for treatment:
t = rbinom(N,1,plogis(lp)) # treatment
# outcome. t->y path is 2. x*->y paths are N(0,1)
y = base::cbind(Xmat,t) %*% c(rnorm(5,0,1), 2) + rnorm(N,0,1)
df <- as.data.frame(base::cbind(Xmat,t,y))
names(df)<-c(letters[1:5],"t","y")

source("source/func/get_score.R")
source("source/func/balancing.R")


propmodel = get_score(psmodel = "glm", 
                      .data = df, 
                      t_var = "t", m_vars= c("a","b","c","d","e"), 
                      missing="complete")



performance_plot <- function(psmodel_obj, .data, treatment, treattype = "binary"){
  
  obs = .data[,treatment]
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

propmodel = get_score(psmodel = "glm", 
                      .data = df, 
                      t_var = "t", m_vars= c("a","b","c","d","e"), 
                      missing="complete")

performance_plot(psmodel_obj = propmodel, .data = df, treatment = "t")


balancedata = balancing(cf_method = "matching",
                        t_var = "t", m_vars= c("a","b","c","d","e"), 
                        psmodel_obj = propmodel
                        )

cobalt::love.plot(balancedata)




source("source/func/get_score.R")
source("source/func/balancing.R")

# currently, glm is implemented with complete and multiple imputation. 
# fiml doesn't work because lavaan estimator switches for categorical outcome
propmodel = get_score(psmodel = "glm", 
             .data = df, 
             t_var = "t", m_vars= c("a","b","c","d","e"), 
             missing="complete")
# propmodel is a list containing
# 1. the data (either a dataframe or mids)
# 2. the model
# 3. the propensity scores
# 4. the model class (e.g. "glm") - this is just needed to pass on to subsequent function


# we then give this function the counterfactual method (currently matching and iptw) 
# and the output of the previous function.  
l=vector(mode="list")
l[['cf_method']] = "matching"
l[['t_var']] = "t"
l[['m_vars']] = c("a","b","c","d","e")
l[['psmodel_obj']] = propmodel
addargs='method="optimal",ratio=2'
addargs2 = strsplit(addargs,",")
addargs2 = unlist(lapply(addargs2, function(x) strsplit(x,"=")), recursive=F)
addargs2 = lapply(addargs2, function(x) gsub('\"','',x))
for(i in 1:length(addargs2)){
  if(str_detect(addargs2[[i]][2], "[[:digit:]]")){
    l[[addargs2[[i]][1]]] = as.numeric(addargs2[[i]][2])
  } else {
    l[[addargs2[[i]][1]]] = addargs2[[i]][2]
  }
}
l
balanceddata = do.call(balancing, l)


# balanceddata is now an object of one of these classes:
# weightit, matchit, mimids, wimids  

# all of which can be passed to cobalt functions to show things like covariate balance:
cobalt::love.plot(balanceddata)
cobalt::bal.tab(balanceddata)




