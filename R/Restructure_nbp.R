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
    
    ## Save initial treatment group as 'treatment_old' - HC
    dose_paired_data$treatment_old = dose_paired_data[[treatment_variable]]
    
    dose_paired_data[["treatment"]] = dose_paired_data[[treatment_variable]]
    dose_paired_data[[treatment_variable]] = dose_paired_data$treatment_exposure

    dose_paired_data <- dose_paired_data %>%
      select(-treatment_exposure)
  }
  
  else if(missing_method == "mi"){
    
    propensity_data <- propensity_data %>% select(unique(colnames(.)))

    # Assuming propensity_data is a data frame
    unique_impsets <- unique(propensity_data$impset)
    
    # Now you can use it in your lapply function
    propensity_data_list <- lapply(unique_impsets, function(impset_value) {
      subset(propensity_data, impset == impset_value)
    })
    
    matched_data <- lapply(matched_data, function(df) {
      df$pairID <- paste("p", 1:length(df$Group1.ID), sep = "")
      tibble(df)
    })
    
    # Pivot longer for each data frame in the list
    matched_data_long <- lapply(matched_data, function(df) {
      pivot_longer(df,
                   cols = c(Group1.ID, Group2.ID),
                   names_to = "group",
                   values_to = "ID")
    })
    
    # Assuming propensity_data_list is a list of data frames
    propensity_data_list <- lapply(propensity_data_list, function(df) {
      df$ID <- seq_along(df[, 1])
      df$ID <- as.integer(df$ID)
      df
    })
    
    # Convert "ID" column to integer in matched_data_long
    matched_data_long <- lapply(matched_data_long, function(x) {
      x$ID <- as.integer(x$ID)
      x
    })
    
    # Merge matched_data_long with propensity_data_list by ID
    merged_data_list <- Map(function(matched, propensity) {
      merge(matched, propensity, by = "ID", all.x = TRUE)
    }, matched_data_long, propensity_data_list)
    
    # Define the manipulation function
    manipulate_data <- function(data) {
      data %>%
        mutate(treatment_variable = as.numeric(data[[treatment_variable]])) %>%
        group_by(pairID) %>%
        mutate(
          first = max(treatment_variable),
          treatment_exposure = factor(ifelse(treatment_variable == first, "high", "low"))
        ) %>%
        select(-c(group, treatment_variable)) %>%
        arrange(pairID) %>%
        ungroup()
    }
    
    # Apply the manipulation to each dataframe in merged_data_list
    manipulated_data_list <- lapply(merged_data_list, manipulate_data)
    
    # Define the function for the next steps
    process_dose_paired_data <- function(data) {
      data[["treatment_old"]] <- data[[treatment_variable]]
      data[[treatment_variable]] <- data$treatment_exposure
      data <- data %>%
        select(-treatment_exposure)
      return(data)
    }
    
    # Apply the function to each dataframe in manipulated_data_list
    dose_paired_data <- lapply(manipulated_data_list, process_dose_paired_data)
    
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
    
    ## Save initial treatment group as 'treatment_old' - HC
    dose_paired_data$treatment_old = dose_paired_data[[treatment_variable]]
    
    dose_paired_data[["treatment"]] = dose_paired_data[[treatment_variable]]
    dose_paired_data[[treatment_variable]] = dose_paired_data$treatment_exposure
    
    dose_paired_data <- dose_paired_data %>%
      select(-treatment_exposure)
  }
  
  return(dose_paired_data)
}























  

