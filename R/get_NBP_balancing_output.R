
#' Function to return observation table, love plot and balance table from NBP matching
#'
#' @param estimation_model_object 
#' @param balanced_data 
#' @param treatment_variable 
#' @param missingness 
#'
#' @import dplyr
#' @import cobalt
#' @import ggplot2
#'
#' @examples
 

# df <- data_frame(treatment = c(1,1,1,2,2,2,3,3,3,3,3,3,4))
# df$outcome <- c(10,14,15,19,15,20,22,23,34,23,45,34,65)
# df$var1 <- c(10,11,14,24,22,29,36,33,37,45,56,34,76)
# df$var2 <- c(10,15,13,27,24,27,38,32,30,34,45,67,87)
# df$var3 <- c(1:13)
# 
# 
# N =500
# A = matrix(runif(5^2)*2-1, ncol = 5)
# Xmat = MASS::mvrnorm(N, mu=rnorm(5,0,3), Sigma = t(A)%*%A)
# lp = apply(Xmat, 2, scale)%*%rnorm(5,0,2)
# t = rbinom(N,c(1,2),plogis(lp))
# y = base::cbind(Xmat,t) %*% c(rnorm(5,0,1),2) + rnorm(N,0,1)
# df <- as.data.frame(base::cbind(Xmat, t, y))
# names(df) <- c(letters[1:5], "t", "y")
# 
# 
# 
# abc <- estimation_stage(.data = df, 
#                         missing_method = "complete", 
#                         model_type = "poly",
#                         treatment_variable = "t", 
#                         matching_variable = c("a", "b", "c")) 
# 
# 
# ghi <- balance_data(counterfactual_method = "nbp", 
#                     treatment_variable = "t", 
#                     matching_variable = c("a", "b", "c"), 
#                     PS_estimation_object = abc,
#                     missing_method = "complete")
# 
# 
# estimation_model_object <- abc
# balanced_data <- ghi
# treatment_variable <- "t"
# matching_variables <- c("a" , "b", "c")
# missingness <- "complete"
# 



get_NBP_balancing_output <- function(estimation_model_object, balanced_data, treatment_variable, matching_variables, missingness){
  
  ## Observation Table ----
  
  if(missingness == "complete"){
    ## Factorize treatment variable
    estimation_model_object$propensity_scores[[treatment_variable]] <- as.factor(estimation_model_object$propensity_scores[[treatment_variable]])
    observation_table_1 <- as.data.frame(t(table(estimation_model_object$propensity_scores[[treatment_variable]])))
    observation_table_2 <- as.data.frame(t(table(balanced_data[[treatment_variable]])))
    
    observation_table <- as.data.frame(data_frame("Observations (Group name: n) " = c(paste0(observation_table_1$Var2,": ", observation_table_1$Freq, collapse = ", "),
                                                                                      paste0(str_to_title(observation_table_2$Var2),"-Exposure Group :", observation_table_2$Freq, collapse = ", "))))
    
    row.names(observation_table) <- c("All(ESS)", "Matched(ESS)")
  }
  
  
  if(missingness == "mi"){
    
    
    
  }
  
  
  ## Balance Table ----
  
  if(missingness == "complete"){
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
  
  
  if(missingness == "mi"){
    
    
    
  }
  
  ## Love plot ----
  
  ## Create data frame containing unmatched and matched mean differences in all matching variables
  
  if(missingness == "complete"){
    
    balance_table_matched <- balance_table %>% 
      dplyr::rename(Matched = `Diff.Un`)
    
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
  
  
  if(missingness == "mi"){
    
    
    
  }
  
  return(list(observation_table = observation_table,
              balance_table = balance_table,
              love_plot = love_plot))
}


