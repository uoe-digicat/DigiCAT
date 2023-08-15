restructure_rejoin_nbp <- function(matched_data, propensity_data, treatment_variable,...){
  
  matched_data$pairID<-paste("p", 1:length(matched_data$Group1.ID), sep="") #add in 'pair ID' var
  matched_data<-tibble(matched_data) #tibble so that tidyverse can be leveraged
  matched_data_long<- tidyr::pivot_longer(matched_data,                   # long format
                                                cols = c(Group1.ID, Group2.ID),
                                                names_to = "group", 
                                                values_to = "ID")
  
  propensity_data$ID <- as.integer(propensity_data$ID)
  matched_data_long$ID <- as.integer(matched_data_long$ID)

  # merge with data incl. Tr and outcomes:
  data_paired <- left_join(propensity_data,matched_data_long, by = "ID")
  
  #create variable indicating high vs low members of pairs
  dose_paired_data <- data_paired %>%
    mutate(treatment_variable = as.numeric(data_paired[[treatment_variable]])) %>%   
    group_by(pairID) %>%  #pairID is the ID for each pair
    mutate(first = max(treatment_variable) , #create dose variable
           treatment_exposure = factor(ifelse(treatment_variable == first, "high", "low"))) %>%
    select(-c(group, treatment_variable)) %>%
    arrange(., pairID) %>%
    ungroup()
  
  return(dose_paired_data)
}


