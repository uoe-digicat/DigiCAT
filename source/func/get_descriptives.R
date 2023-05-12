library(ggplot2)
library(gt)

## Function to create table of descriptive statistics of data
#' @param df Dataframe containing variables for analysis
#' @param categorical_vars Categorical variables

get_description <- function(df, categorical_vars){
  
  
  ## Split df into continuous and catgorcial variables
  df_cat <- as.data.frame(df[,names(df) %in% categorical_vars])
  df_con <- as.data.frame(df[,!names(df) %in% categorical_vars])
  
  ## Make sure there care categorical variables present
  
  if (ncol(df_cat) > 0){
    
    ## Get number of non-NA observations for each variable
    non_NA_observations_cat <- colSums(!is.na(df_cat))
    ## Get number of NA for each variable
    NA_observations_cat <- colSums(is.na(df_cat))
    ## Get number of distinct values for each variable
    unique_observations_cat <- apply(df_cat, 2, function(x) length(unique(x)))
    ## Get mean for each observation
    mean_observations_cat <- apply(df_cat, 2, function(x) round((mean(x, na.rm = TRUE)), digits = 2))
    
    ## Get plot for each variable
    plots_cat <- list()
    for (col in 1:ncol(df_cat)){
      temp <- na.omit(df_cat[col])
      names(temp) <- "x"
      p <-  ggplot(temp, aes(as.factor(x))) +
        geom_bar(fill = "#0073C2FF") +
        theme_classic() + xlab("") +
        theme(axis.text.x = element_text(size = 40), axis.title.y = element_text(size = 40), axis.text.y = element_text(size = 40))
      tmp <- list(p)
      plots_cat[[names(df_cat)[col]]] <- tmp
    }
    ## Make dfs with descriptive information
    df_descriptive_cat <- data.frame(Variable = names(df_cat), n = non_NA_observations_cat, missing = NA_observations_cat, distinct = unique_observations_cat, mean = mean_observations_cat)
    ## Add plots
    df_descriptive_cat <- df_descriptive_cat %>%
      mutate(frequency = NA) %>%
      gt() %>%
      tab_options(table.width = 800) %>%
      tab_header(title = md("Summary of **categorical** variables in dataset")) %>%
      text_transform(
        locations = cells_body(columns = c("frequency")),
        fn = function(x) {
          map(plots_cat, ggplot_image, height = px(100))
        }
      )
  } else{df_descriptive_cat <- NULL}
  
  ## Repeat for continuous variables
  if (nrow(df_con) > 0){
      
    ## Get number of non-NA observations for each variable
    non_NA_observations_con <- colSums(!is.na(df_con))
    ## Get number of NA for each variable
    NA_observations_con <- colSums(is.na(df_con))
    ## Get number of distinct values for each variable
    unique_observations_con <- apply(df_con, 2, function(x) length(unique(x)))
    ## Get mean for each observation
    mean_observations_con <- apply(df_con, 2, function(x) round((mean(x, na.rm = TRUE)), digits = 2))
    ## Get plot for each variable
    plots_con <- list()
    for (col in 1:ncol(df_con)){
      temp <- na.omit(df_con[col])
      names(temp) <- "x"
      p <-  ggplot(temp, aes(x)) +
        geom_histogram(fill = "#0073C2FF", bins=30) +
        theme_classic() + xlab("") +
        theme(axis.text.x = element_text(size = 40), axis.title.y = element_text(size = 40), axis.text.y = element_text(size = 40))
      tmp <- list(p)
      plots_con[[names(df_con)[col]]] <- tmp
    }
    ## Make dfs with descriptive information
    df_descriptive_con <- data.frame(Variable = names(df_con), n = non_NA_observations_con, missing = NA_observations_con, distinct = unique_observations_con, mean = mean_observations_con)
    ## Add plots
    df_descriptive_con <- df_descriptive_con %>%
      mutate(frequency = NA) %>%
      gt() %>%
      tab_options(table.width = 800) %>%
      tab_header(title = md("Summary of **continuous** variables in dataset")) %>%
      text_transform(
        locations = cells_body(columns = c("frequency")),
        fn = function(x) {
          map(plots_con, ggplot_image, height = px(100))
        }
      )
  } else{df_descriptive_con <- NULL}
  
  return(list(df_descriptive_con, df_descriptive_cat))
  
}
