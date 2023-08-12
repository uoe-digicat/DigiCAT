nbp_mi_quick <- function(handled_missingness, treatment_variable, ID_variable, outcome_variable,...){
  xyz <- with(handled_missingness, {
    polr_res <- MASS::polr(f, Hess = T)  
    
    dat_cols <- data.frame(variables) # recreate dataframe
    
    pollyps<-as.data.frame(cbind(dat_cols, polr_res$model,polr_res$lp))
    
    treatment_variable <- as.numeric(treatment_variable)
    
    #create distance matrix:
    
    eps<-1*10^-100 #specify epsilon to be very small positive number
    result <- matrix(ncol = ncol(handled_missingness), nrow = nrow(handled_missingness)
    
    matj <- matrix(data = treatment_variable, nrow = nrow(handled_missingness), ncol = ncol(handled_missingness), byrow = F)
    matk <- matrix(data = treatment_variable, nrow = nrow(handled_missingness), ncol = ncol(handled_missingness), byrow = T)
    
    res <- matj - matk
    res_squared <- res^2
    
    lpj <- matrix(data = polr_res$lp, nrow = nrow(handled_missingness), ncol = ncol(handled_missingness), byrow = F)
    lpk <- matrix(data = polr_res$lp, nrow = nrow(handled_missingness), ncol = ncol(handled_missingness), byrow = T)
    
    lp_res <- lpj - lpk
    lp_res_abs <- abs(lp_res)
    lp_calc <- 0.15 * sqrt(var(polr_res$lp))
    lp_logical <- lp_res_abs <= lp_calc
    
    lp_res_squared_plus_eps <- (lp_res^2) + eps
    
    res_squared[!lp_logical] <- 10^11
    res_squared[lp_logical] <- 10^11 * (lp_res_squared_plus_eps[lp_logical]) / res_squared[lp_logical]
    
    result <- res_squared
    
    row.names(result)<-pollyps$ID_variable
    
    dist1<-nbpMatching::distancematrix(result)
    
    nbpmatches<-nbpMatching::nonbimatch(dist1, threshold=999999, precision = 7)
    
    nbpmatches_matched<-nbpmatches$halves[nbpmatches$halves$Distance!=999999, ] # eliminate unmatched
    
    nbpmatches_matched$pairID<-paste("p", 1:length(nbpmatches_matched$Group1.ID), sep="") #add in 'pair ID' var
    nbpmatches_matched<-tibble(nbpmatches_matched) #tibble so that tidyverse can be leveraged
    nbpmatches_matched_long<- tidyr::pivot_longer(nbpmatches_matched,                   # long format
                                                  cols = c(Group1.ID, Group2.ID),
                                                  names_to = "group",
                                                  values_to = "ID")
    
    dat_cols <- data.frame(variables) # recreate dataframe
    
    names(dat_cols)[5]<-'ID' # find ID variable and ensure matched names to merge?
    
    newdata_paired <- left_join(dat_cols, nbpmatches_matched_long, by = "ID")
    
    newdata_paired2 <- newdata_paired %>%
      mutate(treatment_variable) = as.numeric(treatment_variable)) %>%   
      group_by(pairID) %>%  #pairID is the ID for each pair
      mutate(first = max(treatment_variable) , #create dose variable
             dose = factor(ifelse(treatment_variable == first, "high", "low"))) %>%
      #select(-c(group, treatment_variable)) %>%
      arrange(., pairID) %>%
      ungroup()
    
    # get outcome formula - Y~Tr*(X)
    
    nbp_outcome <- lm(f, data = newdata_paired2)
    # is there a marginaleffects procedure for this?
    
  })
  
  pooled_nbp <- summary(pool(xyz))
}