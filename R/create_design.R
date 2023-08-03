require(survey)

create_design <- function(.data, weighting_variable = NULL,
                          cluster_variable = NULL, strata_variable = NULL,
                          ...){
  if (!is.null(weighting_variable) & is.null(cluster_variable) & is.null(strata_variable)) { # currently as sampling & NR have same workflow, is not controlled by NR = TRUE
    data_complete = subset(.data, !is.na(.data[[weighting_variable]]))
    design_object = svydesign(ids = ~1, 
                              weights = data_complete[[weighting_variable]], 
                              data = data_complete, 
                              ...) #
    return(design_object) 
    
  } else if(!is.null(cluster_variable)){
      data_complete = subset(.data, !is.na(.data[[weighting_variable]]))
      design_object = svydesign(ids = data_complete[[cluster_variable]],  
                                weights = data_complete[[weighting_variable]], 
                                data = data_complete, 
                                ...) 
      return(design_object) 
      
    } else if(!is.null(strata_variable)){
      data_complete = subset(.data, !is.na(.data[[weighting_variable]]))
      design_object = svydesign(ids = ~1, 
                                weights = data_complete[[weighting_variable]], 
                                strata = data_complete[[strata_variable]], 
                                data = data_complete, 
                                ...) 
      return(design_object) 
    } else if(!is.null(strata_variable) & !is.null(cluster_variable)){
      data_complete = subset(.data, !is.na(.data[[weighting_variable]]))
      design_object = svydesign(ids = data_complete[[cluster_variable]], 
                                weights = data_complete[[weighting_variable]], 
                                strata = data_complete[[strata_variable]], 
                                data = data_complete, 
                                ...) 
      return(design_object) 
  }
}














