#' gen_response_surface_x: Generate IVs for Response Surface
#'
#' A helper function for simulating the independent variables for a response surface. Automatically calculates the interaction and quadratic terms. 
#' For simplicities sake, assumes that variables have mean 0
#' 
#' @param n Number of observations to simulate.
#' @param cov_mat Covariance matrix defining how X1 and X2 are related along with their scale.
#' @param x_names Character vector of length two containing the names of X variables (optional)
#'
#' @return Data frame of IVs with requested column names.
#' @export 
#' @importFrom magrittr %>%
#' @import dplyr
#'
#' @examples
#' 
#' # Defining Correlation Matrix describing how x1 and x2 are related
#' # Covarince and variance of x1^2, x2^2, and x1*x2 follow from this matrix
#' cov_mat<-matrix(c(1, 0,
#'                   0, 1), byrow = TRUE, 2, 2)
#' 
#' # Generating data frame for response suface examining leaders and follower agreeableness
#' x_df<-gen_response_surf_x(1000, cov_mat, x_names = c("L_Agree", "F_Agree"))
#' x_df
 

gen_response_surf_x<-function(n, cov_mat, x_names=NULL){
  sample<-n
  
  suppressMessages(
    sim_help<-MASS::mvrnorm(sample, c(0,0), cov_mat)%>%
      dplyr::as_tibble(.name_repair = "unique")%>%
      dplyr::rename(x1 = ...1,
             x2 = ...2)%>%
      dplyr::mutate(x1_sq = x1^2,
                    x2_sq = x2^2,
                    int = x1*x2)
  )
  
  if(!is.null(x_names)){
    colnames(sim_help)<-c(x_names, paste0(x_names, "_sq"), paste0(x_names, collapse = "*"))
  }
  
  sim_help
}

