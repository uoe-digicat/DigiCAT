library(tidyverse, quietly = T)

restructure_rejoin_nbp <- function(matched_data, propensity_data, treatment_variable, missing_method,...){
  if(missing_method == "complete"){
    matched_data$pairID<-paste("p", 1:length(matched_data$Group1.ID), sep="") #add in 'pair ID' var
    matched_data<-tibble(matched_data) #tibble so that tidyverse can be leveraged
    matched_data_long<- tidyr::pivot_longer(matched_data,                   # long format
                                            cols = c(Group1.ID, Group2.ID), #### will need to change accordingly
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

    dose_paired_data[["treatment"]] = dose_paired_data[[treatment_variable]]
    dose_paired_data[[treatment_variable]] = dose_paired_data$treatment_exposure

    dose_paired_data <- dose_paired_data %>%
      select(-treatment_exposure)
  }
  
  else if(missing_method == "mi"){
    
    matched_data <- lapply(matched_data, function(x) paste("p", 1:length(x$Group1.ID), sep = "")) 
    matched_data<- lapply(matched_data, function(x) dplyr::tibble(x)) #tibble so that tidyverse can be leveraged
    matched_data_long <- lapply(matched_data, function(x) tidyr::pivot_longer(x,                   # long format
                                                                              cols = c(Group1.ID, Group2.ID),
                                                                              names_to = "group", 
                                                                              values_to = "ID"))
    
    propensity_scores$ID <- seq_along(propensity_scores[,1]) # needs to be for each set only - repeat every nrow(data) times
    propensity_scores$ID <- as.integer(propensity_scores$ID) ### add
    matched_data_long$ID <- lapply(matched_data_long, function(x) as.integer(x$ID)) ### sort out to be added to each set
    # chameleon = NA ?
    
    ### add all below
    
    # merge with data incl. Tr and outcomes:
    data_paired <- left_join(propensity_data,matched_data_long, by = "ID") ### add
    
    #create variable indicating high vs low members of pairs
    dose_paired_data <- data_paired %>%
      mutate(treatment_variable = as.numeric(data_paired[[treatment_variable]])) %>%   
      group_by(pairID) %>%  #pairID is the ID for each pair
      mutate(first = max(treatment_variable) , #create dose variable
             treatment_exposure = factor(ifelse(treatment_variable == first, "high", "low"))) %>%
      select(-c(group, treatment_variable)) %>%
      arrange(., pairID) %>%
      ungroup()
    
    dose_paired_data[[treatment_variable]] = dose_paired_data$treatment_exposure
    
    dose_paired_data <- dose_paired_data %>%
      select(-treatment_exposure)
  } 
  else if(missing_method == "weighting"){
    matched_data$pairID<-paste("p", 1:length(matched_data$Group1.ID), sep="") #add in 'pair ID' var
    matched_data<-tibble(matched_data) #tibble so that tidyverse can be leveraged
    matched_data_long<- tidyr::pivot_longer(matched_data,                   # long format
                                            cols = c(Group1.ID, Group2.ID), #### will need to change accordingly
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
    
    dose_paired_data[["treatment"]] = dose_paired_data[[treatment_variable]]
    dose_paired_data[[treatment_variable]] = dose_paired_data$treatment_exposure
    
    dose_paired_data <- dose_paired_data %>%
      select(-treatment_exposure)
  }
  
  return(dose_paired_data)
}


