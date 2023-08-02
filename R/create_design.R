require(survey)

create_design <- function(.data, nonresponse_weights = NULL,
                          sampling_weights = NULL, cluster_variable = NULL,
                          ...){
  if (!is.null(nonresponse_weights)) {
    design_object = svydesign(ids = ~1, weights = nonresponse_weights, data = .data, ...) 
  } else if(is.null(nonresponse_weights) & !is.null(sampling_weights)){ 
    design_object = svydesign(ids = ~1, weights = sampling_weights, data = .data, ...) 
  } else{ 
    design_object = svydesign(ids = ~1, data = .data, ...) 
  }
  return(design_object) 
}

