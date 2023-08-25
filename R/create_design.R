#' @import survey


create_design <- function(.data, weighting_variable = NULL,
                          cluster_variable = NULL, strata_variable = NULL,
                          ...){
  if (!is.null(weighting_variable) & is.null(cluster_variable) & is.null(strata_variable)) { # currently as sampling & NR have same workflow, is not controlled by NR = TRUE
    data_complete = subset(.data, !is.na(.data[[weighting_variable]])) # only weighting
    design_object = svydesign(ids = ~1, 
                              weights = data_complete[[weighting_variable]], 
                              data = data_complete, 
                              ...) #
    
  } else if(!is.null(cluster_variable) & !is.null(weighting_variable) & is.null(strata_variable)){
    data_complete = subset(.data, (!is.na(.data[[weighting_variable]])) & (!is.na(.data[[cluster_variable]]))) # weighting and ids
    design_object = svydesign(ids = data_complete[[cluster_variable]],  
                                weights = data_complete[[weighting_variable]], 
                                data = data_complete, 
                                ...) 
    
    } else if(!is.null(strata_variable) & is.null(cluster_variable) & !is.null(weighting_variable)){
      data_complete = subset(.data, (!is.na(.data[[weighting_variable]])) & (!is.na(.data[[strata_variable]]))) # weighting and strata
      design_object = svydesign(ids = ~1, 
                                weights = data_complete[[weighting_variable]], 
                                strata = data_complete[[strata_variable]], 
                                data = data_complete, 
                                ...) 
    } else if(!is.null(strata_variable) & !is.null(cluster_variable) & !is.null(weighting_variable)){
      data_complete = subset(.data, (!is.na(.data[[weighting_variable]])) & (!is.na(.data[[strata_variable]])) & (!is.na(.data[[cluster_variable]]))) # all 3
      tryCatch(
        expr = {
          design_object = svydesign(ids = data_complete[[cluster_variable]], 
                                weights = data_complete[[weighting_variable]], 
                                strata = data_complete[[strata_variable]], 
                                data = data_complete, 
                                ...) 
        },
        error = function(e) {
          design_object = svydesign(ids = data_complete[[cluster_variable]], 
                                    weights = data_complete[[weighting_variable]], 
                                    strata = data_complete[[strata_variable]], 
                                    data = data_complete, nest = TRUE,
                                    ...) 
        }
      )
    } else if(!is.null(cluster_variable) & is.null(weighting_variable) & is.null(strata_variable)){
      data_complete = subset(.data, !is.na(.data[[cluster_variable]])) # ids only
      design_object = svydesign(ids = data_complete[[cluster_variable]], 
                                data = data_complete, 
                                ...) 
    }else if(!is.null(strata_variable) & is.null(weighting_variable) & is.null(cluster_variable)){
      data_complete = subset(.data, !is.na(.data[[strata_variable]])) # strata only
      design_object = svydesign(ids = ~1, 
                                strata = data_complete[[strata_variable]],
                                data = data_complete, 
                                ...) 
    }
  else if(!is.null(strata_variable) & is.null(weighting_variable) & !is.null(cluster_variable)){
    data_complete = subset(.data, (!is.na(.data[[strata_variable]])) & (!is.na(.data[[cluster_variable]]))) # strata only
    tryCatch(
      expr = {
        design_object = svydesign(ids = data_complete[[cluster_variable]], 
                                  strata = data_complete[[strata_variable]], 
                                  data = data_complete, 
                                  ...) 
      },
      error = function(e) {
        design_object = svydesign(ids = data_complete[[cluster_variable]], 
                                  strata = data_complete[[strata_variable]], 
                                  data = data_complete, nest = TRUE,
                                  ...) 
      }
    )
    }
}

























