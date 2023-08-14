require(tidyverse)

check_support <- function(estimation_model_object,...){ # double-check this
  prs_df = data.frame(prop_score = predict(estimation_model_object$estimated_propensity_model, 
                                           type = "response"),
                       treatment = estimation_model_object$estimated_propensity_model$model[1])
  names(prs_df)[2] <- "treatment"
  
  labs <- paste("Actual treatment:", c("1", "0"))
  common_support_plot = prs_df %>%
    mutate(treatment = ifelse(treatment == 1, "1", "0")) %>%
    ggplot()+
    geom_histogram(aes(x=prop_score, y=after_stat(density), fill=treatment), color="white", bins=30, position="identity", alpha=.3)+
    geom_density(aes(x=prop_score, color=treatment))
  
}

