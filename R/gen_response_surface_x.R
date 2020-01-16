#' gen_response_surface_x: Generate IVs for Response Surface
#'
#' @param n Number of observations to generate
#' @param cor_mat Correlation matrix between IVs
#' @param x_names Names of x variables (optional)
#'
#' @return Data frame of IVs with requested column names.
#' @export
#'
#' @examples
gen_response_surf_x<-function(n, cor_mat, x_names=NULL){
  sample<-n
  
  suppressMessages(
    sim_help<-MASS::mvrnorm(sample, c(0,0), cor_mat)%>%
      as_tibble(.name_repair = "unique")%>%
      rename(x1 = ...1,
             x2 = ...2)%>%
      mutate(x1_sq = x1^2,
             x2_sq = x2^2,
             int = x1*x2)
  )
  if(!is.null(x_names)){
    colnames(sim_help)<-c(names, paste0(names, "_sq"), paste0(names, collapse = "*"))
  }
}

