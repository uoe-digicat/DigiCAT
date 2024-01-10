
#' Function to return observation table, love plot and balance table from NBP matching.
#'
#' @param estimation_model_object 
#' @param balanced_data 
#' @param treatment_variable 
#' @param missing_method 
#' @import cobalt
#' @import ggplot2

get_NBP_balancing_output <- function(estimation_model_object, balanced_data, treatment_variable, matching_variables, missing_method){
  
  ## Observation Table ----
  
  if(missing_method == "complete" | missing_method == "weighting"){
    ## Factorize treatment variable in both matched and unmatched datasets
    estimation_model_object$propensity_scores[[treatment_variable]] <- as.factor(estimation_model_object$propensity_scores[[treatment_variable]])
    balanced_data$treatment <- as.factor(balanced_data$treatment)
    ## Count frequency of observations in each treatment group
    observation_table_all <- as.data.frame(t(table(estimation_model_object$propensity_scores[[treatment_variable]]))) ## Original treatment groups before matching
    observation_table_unmatched <- as.data.frame(t(table(balanced_data$treatment))) ## Original treatment groups after matching (unmathed subjects excluded)
    observation_table_matched <- as.data.frame(t(table(balanced_data[[treatment_variable]]))) ## Treatment groups after matching
    
    ## Get difference between observation counts before and after matching in original treatment groups
    observation_table_unmatched$Freq <- observation_table_all$Freq - observation_table_unmatched$Freq
    
    ## Combine observation tables
    observation_table <- as.data.frame(data_frame("Observations (Group name: n) " = c(paste0(observation_table_all$Var2,": ", observation_table_all$Freq, collapse = ", "),
                                                                                      paste0(str_to_title(observation_table_matched$Var2),"-Exposure Group :", observation_table_matched$Freq, collapse = ", "),
                                                                                      paste0(observation_table_unmatched$Var2,": ", observation_table_unmatched$Freq, collapse = ", "))))
    row.names(observation_table) <- c("All(ESS)", "Matched(ESS)", "Unmatched")
  }
  
  
  if(missing_method == "mi"){
    
    ## Factorize treatment variable in both matched and unmatched datasets
    estimation_model_object$propensity_scores$`as.factor(treatment)`[estimation_model_object$propensity_scores$impset == 1] <- as.factor(estimation_model_object$propensity_scores$`as.factor(treatment)`[estimation_model_object$propensity_scores$impset == 1])
    balanced_data[[1]]$treatment <- as.factor(balanced_data[[1]]$treatment)
    
    ## Count frequency of observations in each treatment group
    observation_table_all <- as.data.frame(t(table(estimation_model_object$propensity_scores$`as.factor(treatment)`[estimation_model_object$propensity_scores$impset == 1]))) ## Original treatment groups before matching
    observation_table_unmatched <- as.data.frame(t(table(balanced_data[[1]]$`as.factor(treatment)`))) ## Original treatment groups after matching (unmathed subjects excluded)
    observation_table_matched <- as.data.frame(t(table(balanced_data[[1]]$treatment))) ## Treatment groups after matching
    
    ## Get difference between observation counts before and after matching in original treatment groups
    observation_table_unmatched$Freq <- observation_table_all$Freq - observation_table_unmatched$Freq
    
    ## Combine observation tables
    observation_table <- as.data.frame(data_frame("Observations (Group name: n) " = c(paste0(observation_table_all$Var2,": ", observation_table_all$Freq, collapse = ", "),
                                                                                      paste0(str_to_title(observation_table_matched$Var2),"-Exposure Group :", observation_table_matched$Freq, collapse = ", "),
                                                                                      paste0(observation_table_unmatched$Var2,": ", observation_table_unmatched$Freq, collapse = ", "))))
    row.names(observation_table) <- c("All(ESS)", "Matched(ESS)", "Unmatched")
  }
  
  
  ## Balance Table ----
  
  if(missing_method == "complete" | missing_method == "weighting"){
    ## Get balance table
    balance_table <- bal.tab(balanced_data[,c(matching_variables)], treat = relevel(balanced_data[[treatment_variable]], ref = "low"), distance = balanced_data[["lp"]])
    balance_table <- as.data.frame(balance_table[[which(grepl("^Balance",names(balance_table)))]])
    
    ## Remove empty columns from balance table
    balance_table <- balance_table[,colSums(is.na(balance_table))<nrow(balance_table)]
    ## Round numbers in balance table to 4 decimals
    names_temp <- as.factor(row.names(balance_table))
    balance_table <- data.frame(lapply(balance_table,function(x) if(is.numeric(x)) round(x, 4) else x))
    row.names(balance_table) <- names_temp
  }
  
  
  if(missing_method == "mi"){
    
    balance_table_ls <- list()
    
    for (i in 1:length(balanced_data)){
      
      ## Get pooled distance between treatment groups - maximum distance between groups
      balance_table_unadjusted <- bal.tab(estimation_model_object$propensity_scores[estimation_model_object$propensity_scores$impset == i,c(matching_variables)],
                               treat = relevel(estimation_model_object$propensity_scores$`as.factor(treatment)`[estimation_model_object$propensity_scores$impset == 1], ref = 1),
      distance = estimation_model_object$propensity_scores$`polly$lp`[estimation_model_object$propensity_scores$impset == i])
      
      balance_table_adjusted <- bal.tab(balanced_data[[i]][,c(matching_variables)],
                               treat = relevel(balanced_data[[i]][[treatment_variable]], ref = "low"),
                               distance = balanced_data[[i]]$`polly$lp`)
      
      ## Format into single dataframe
      balance_table_unadjusted$Balance.Across.Pairs$Max.Diff.Adj <- balance_table_adjusted$Balance$Diff.Un
      balance_table_unadjusted$Balance.Across.Pairs$matching_var <- row.names(balance_table_unadjusted$Balance.Across.Pairs)
      balance_table_ls[[i]] <-  balance_table_unadjusted$Balance.Across.Pairs
    }
    
    ## Get 'Type', 'Min.Diff.Un', 'Mean.Diff.Un', 'Max.Diff.Un', 'Min.Diff.Adj', 'Mean.Diff.Adj', 'Max.Diff.Adj'
    balance_table_unls <- do.call("rbind", balance_table_ls)
    balance_table <- data.frame(matrix(NA, nrow = length(matching_variables) + 1, ncol = 7))
    names(balance_table) <- c('Type', 'Min.Diff.Un', 'Mean.Diff.Un', 'Max.Diff.Un', 'Min.Diff.Adj', 'Mean.Diff.Adj', 'Max.Diff.Adj')
    row.names(balance_table) <- c("distance", matching_variables)
    
    for(i in c("distance", matching_variables)){
      
      balance_table[i,"Type"] <- balance_table_unls[balance_table_unls$matching_var == i,"Type"][1]
      balance_table[i,"Min.Diff.Un"] <- min(balance_table_unls[balance_table_unls$matching_var == i,"Max.Diff.Un"])
      balance_table[i,"Mean.Diff.Un"] <- mean(balance_table_unls[balance_table_unls$matching_var == i,"Max.Diff.Un"], na.rm = T)
      balance_table[i,"Max.Diff.Un"] <- max(balance_table_unls[balance_table_unls$matching_var == i,"Max.Diff.Un"], na.rm = T)
      balance_table[i,"Min.Diff.Adj"] <- min(balance_table_unls[balance_table_unls$matching_var == i,"Max.Diff.Adj"])
      balance_table[i,"Mean.Diff.Adj"] <- mean(balance_table_unls[balance_table_unls$matching_var == i,"Max.Diff.Adj"], na.rm = T)
      balance_table[i,"Max.Diff.Adj"] <- max(balance_table_unls[balance_table_unls$matching_var == i,"Max.Diff.Adj"], na.rm = T)
    }
    
    ## Round numbers in balance table to 4 decimals
    names_temp <- as.factor(row.names(balance_table))
    balance_table <- data.frame(lapply(balance_table,function(x) if(is.numeric(x)) round(x, 4) else x))
    row.names(balance_table) <- names_temp
  }
  
  ## Love plot ----
  
  ## Create data frame containing unmatched and matched mean differences in all matching variables
  
  if(missing_method == "complete" | missing_method == "weighting"){
    
    names(balance_table)[names(balance_table) %in% "Diff.Un"] <- "Matched"
    balance_table_matched <- balance_table
    
    ## Get unmatched balance table
    balance_table_unmatched_ls <- bal.tab(as.data.frame(estimation_model_object$propensity_scores[,c(matching_variables)]), treat = as.factor(estimation_model_object$propensity_scores[[treatment_variable]]), distance = estimation_model_object$propensity_scores[["lp"]], which.treat = .all)
    balance_table_unmatched_ls <- unlist(balance_table_unmatched_ls, recursive = F, use.names = TRUE)
    balance_table_unmatched_ls <- map(balance_table_unmatched_ls, ~.x$Balance)
    
    ## Get average difference across all pairwise comparisons
    balance_table_unmatched <- data.frame(
      Unmatched = rowMeans(sapply(balance_table_unmatched_ls, "[[", "Diff.Un"))
    )
    row.names(balance_table_unmatched) <- row.names(balance_table_matched)
    
    ## Merge balance tables
    balance_table_unmatched$id  <- 1:nrow(balance_table_unmatched)
    balance_table_compare <- merge(balance_table_unmatched, balance_table_matched, by = 'row.names', all = TRUE)
    balance_table_compare <- balance_table_compare[order(balance_table_compare$id), ]
    
    
    ## Plot love plot
    
    # lock in factor level order
    balance_table_compare$Row.names <- factor(balance_table_compare$Row.names, levels = balance_table_compare$Row.names, )
    colors <- c("Matched" = "blue", "Unmatched" = "red")
    
    love_plot <- ggplot(balance_table_compare) +
      geom_point( aes(x=as.factor(Row.names), y=Unmatched, color="Unmatched"), size=2, alpha = 0.7) +
      geom_point( aes(x=as.factor(Row.names), y=Matched, color="Matched"), size=2, alpha = 0.7) +
      geom_hline(yintercept = 0, size=0.5) +
      coord_flip()+
      scale_x_discrete(limits = rev(levels(balance_table_compare$Row.names))) +
      theme_classic() +
      theme(panel.background = element_rect(colour = "black", size=1)) +
      labs(color = "Sample") +
      scale_color_manual(values = colors) + 
      ggtitle("Covariate Balance") +
      xlab("") +
      ylab("Mean Differences (Unmatched = Averaged across pairwise comparisons, Matched = Low vs high-exposure)")
    
  }
  
  
  if(missing_method == "mi"){
    
    balance_table$Row.names <- row.names(balance_table)
    balance_table$Row.names <- factor(balance_table$Row.names, levels = balance_table$Row.names, )
    colors <- c("Matched" = "blue", "Unmatched" = "red")
    
    love_plot <- ggplot(balance_table) +
      geom_point( aes(x=as.factor(Row.names), y=Mean.Diff.Un, color="Unmatched"), size=2, alpha = 0.7) +
      geom_linerange(aes(x=as.factor(Row.names), ymin = Min.Diff.Un, ymax = Max.Diff.Un, color="Unmatched")) +
      geom_point( aes(x=as.factor(Row.names), y=Mean.Diff.Adj, color="Matched"), size=2, alpha = 0.7) +
      geom_linerange(aes(x=as.factor(Row.names), ymin = Min.Diff.Adj, ymax = Max.Diff.Adj, color="Matched")) +
      geom_hline(yintercept = 0, size=0.5) +
      coord_flip()+
      scale_x_discrete(limits = rev(levels(balance_table$Row.names))) +
      theme_classic() +
      theme(panel.background = element_rect(colour = "black", size=1)) +
      labs(color = "Sample") +
      scale_color_manual(values = colors) + 
      ggtitle("Covariate Balance") +
      xlab("") +
      ylab("Mean Differences (Unmatched = Averaged across pairwise comparisons, Matched = Low vs high-exposure)")
    
  }
  
  return(list(observation_table = observation_table,
              balance_table = balance_table,
              love_plot = love_plot))
}


