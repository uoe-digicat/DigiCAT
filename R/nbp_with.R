data(mtcars)
mtcars$gear <- as.factor(mtcars$gear)
handled_missingness <- mice::mice(mtcars)

### to do
### add as.factor before model
### loop over matching variables and alter format to enable multiple

nbp_mi_quick <- function(handled_missingness, treatment_variable, outcome_variable,
                         matching_variable,...){
  
  xyz <- with(handled_missingness, {

    f = paste0(treatment_variable,"~",paste0(matching_variable, collapse="+"))

    polr_res <- MASS::polr(as.formula(f), Hess = T)

    treatment_values <- get(treatment_variable)
    treatment_values <- as.numeric(as.character(treatment_values))

    # matching_values <- list()
    # for(i in matching_variable){
    #   tmp <- get(i)
    #   matching_values <- append(matching_values, tmp)
    # }})}

outcome_values <- get(outcome_variable)
matching_values <- get(matching_variable) # 1 matching variable only

  dat_cols <- data.frame(treatment_values,
                            matching_values,
                            outcome_values)

  pollyps <- as.data.frame(cbind(
     dat_cols,
    polr_res$model,polr_res$lp))

  pollyps$ID <- seq_along(pollyps[,1])
  pollyps$ID <- as.character(pollyps$ID)

     #create distance matrix:

     eps<-1*10^-100 #specify epsilon to be very small positive number
     result <- matrix(ncol = nrow(pollyps), nrow = nrow(pollyps))

matj <- matrix(data = treatment_values, nrow = nrow(pollyps), ncol = nrow(pollyps), byrow = F)
                      matk <- matrix(data = treatment_values, nrow = nrow(pollyps), ncol = nrow(pollyps), byrow = T)

                     res <- matj - matk
                     res_squared <- res^2

                     lpj <- matrix(data = polr_res$lp, nrow = nrow(pollyps), ncol = nrow(pollyps), byrow = F)
                     lpk <- matrix(data = polr_res$lp, nrow = nrow(pollyps), ncol = nrow(pollyps), byrow = T)

                     lp_res <- lpj - lpk
                     lp_res_abs <- abs(lp_res)
                     lp_calc <- 0.15 * sqrt(var(polr_res$lp))
                     lp_logical <- lp_res_abs <= lp_calc

                     lp_res_squared_plus_eps <- (lp_res^2) + eps

                     res_squared[!lp_logical] <- 10^11
                     res_squared[lp_logical] <- 10^11 * (lp_res_squared_plus_eps[lp_logical]) / res_squared[lp_logical]

                     result <- res_squared

                     row.names(result)<-pollyps$ID

                     dist1<-nbpMatching::distancematrix(result)

                     nbpmatches<-nbpMatching::nonbimatch(dist1,
                                                         threshold=999999,
                                                         precision = 7)

                     nbpmatches_matched<-nbpmatches$halves[nbpmatches$halves$Distance!=999999, ] # eliminate unmatched

                     nbpmatches_matched$pairID<-paste("p", 1:length(nbpmatches_matched$Group1.ID), sep="") #add in 'pair ID' var
                     nbpmatches_matched<-tibble(nbpmatches_matched) #tibble so that tidyverse can be leveraged
                     nbpmatches_matched_long<- tidyr::pivot_longer(nbpmatches_matched,                   # long format
                                                                   cols = c(Group1.ID, Group2.ID),
                                                                   names_to = "group",
                                                                   values_to = "ID")

                    dat_cols <- data.frame(pollyps) # recreate dataframe

                    newdata_paired <- left_join(dat_cols, nbpmatches_matched_long, by = "ID")

                    newdata_paired2 <- newdata_paired %>%
                       mutate(treatment_values = as.numeric(treatment_values)) %>%
      group_by(pairID) %>%  #pairID is the ID for each pair
      mutate(first = max(treatment_values) , #create dose variable
             treatment_exposure = factor(ifelse(treatment_values == first, "high", "low"))) %>%
      #select(-c(group, treatment_variable)) %>%
      arrange(., pairID) %>%
      ungroup()

     nbp_outcome <- lm(outcome_values ~ treatment_exposure + matching_values, data = newdata_paired2)

})

 pooled_nbp <- summary(mice::pool(xyz))
}


    






