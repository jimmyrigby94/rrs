# A semi-hacky way to find the appropriate error variance of y to ensure it aligns with the simulation's goal
# Uses variance algebra and the assumptions of regression to identify the appropriate error standard dev
# Given beta and predictor variance
# Based on monte carlo simulation approach


#' find_sig: Find residual variance of your outcome given a vector of coefficients
#'
#' @param n Number of Obseravtion
#' @param cor_mat Correlation matrix of IVs
#' @param beta vector of coefficients mapping terms X1, X2, X1^2, X2^2, and X1*X2 to y
#' @param target_var_y the desired variance of y
#' @param iter The number of iterations to run to estimate the residual variance.
#'
#' @return Value of residual variance given proposed model
#' @export
#'
#' @examples
find_sig<-function(n, cor_mat, beta, target_var_y = 1, iter=10000){
  
  sig_vec<-c()
  
  for(i in 1:iter){
    simmed_data<-gen_response_surf_x(n, cor_mat)%>%
      gen_response_surf_y(beta = beta)
    
    sig_vec[[i]]<-target_var_y-var(simmed_data$y)
    
  }
  
  sig_val<-mean(sig_vec)
  
  message("To generate an outcome variable with a variance of ", target_var_y, " given your betas, use a error sd of")
  
  sig_val
}
