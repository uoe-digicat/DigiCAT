assign_id_nbp <- function(distance_matrix, propensity_score, id_variable,...){

  row.names(distance_matrix)<-propensity_score[[id_variable]]
  
  }


