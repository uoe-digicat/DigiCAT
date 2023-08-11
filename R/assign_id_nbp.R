assign_id_nbp <- function(){
  propensity_score$ID <- 1:nrow(propensity_score)
  row.names(distance_matrix_nbp)<-propensity_score$ID
}