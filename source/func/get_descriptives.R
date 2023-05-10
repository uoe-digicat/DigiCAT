## Function to create descriptive statistics of input variables
library(ggplot2)
#install.packages("arsenal")
library(arsenal) 

get_descriptives <- function(df, treatment, outcome, matchvars, categorical_vars){
  
  ## Rename treatment(x) and outcome(y) vars for plotting
  df_xy <- data.frame(x=df[[input$treatment]],y=df[[input$outcome]])
  
  ## If treatment variable is categorical, create boxplots
  if (input$treatment %in% input$categorical_vars){
    
    ## Create bowplot to show association between outcome and treatment
    descriptive_plot <- ggplot(df_xy, aes(as.factor(x), y, fill = as.factor(x))) + geom_boxplot() +
      theme_classic() +
      labs(title=paste0(input$outcome, " VS ", input$treatment),x=input$treatment, y = input$outcome) +
      guides(fill=guide_legend(title="New Legend Title")) +
      scale_fill_brewer(palette="Dark2")
    
    ## Create table containing descriptive statistics
    
    mod <- paste0("as.factor(",paste0(input$treatment), ") ~ ", paste0(c(input$outcome, input$matchvars), collapse = "+"))
    descriptive_table <- summary(tableby(as.formula(mod), data = df))
    
    
    
  }else{
    
    ## Create bowplot to show association between outcome and treatment
    descriptive_plot <- ggplot(df_xy, aes(x, y)) +
      geom_point(color = 'cornflowerblue') +
      stat_smooth() +
      theme_classic() +
      labs(title=paste0(input$outcome, " VS ", input$treatment),x=input$treatment, y = input$outcome)
    
    ## Create table containing descriptive statistics - stratify treatment into two equal groups
    
    treatment_median <- median(df[,input$treatment])
    df$temp <- factor(ifelse(df[,input$treatment] < treatment_median,paste0("Low (<",treatment_median, ")"),paste0("High (>",treatment_median, ")")))
    names(df)[names(df) == "temp"] <- paste0(input$treatment, "_groups")
    
    mod <- paste0(paste0(input$treatment, "_groups")," ~ ", paste0(c(input$outcome, input$matchvars), collapse = "+"))
    descriptive_table <- summary(tableby(as.formula(mod), data = df))
    
  }
  
  output <- list(plot = descriptive_plot,
                 table = descriptive_table)
  
  
  return(output)
}


input <- list(treatment = "cyl",
              outcome = "disp",
              matchvars = c("drat", "wt", "qsec", "vs", "am", "gear", "carb"),
              categorical_vars = c("cyl", "am", "gear", "carb"))
