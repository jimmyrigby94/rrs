# A semi-hacky way to find the appropriate error variance of y to ensure it aligns with the simulation's goal
# Uses variance algebra and the assumptions of regression to identify the appropriate error standard dev
# Given beta and predictor variance
# Based on monte carlo simulation approach


#' find_sig: Find residual variance of your outcome given a vector of coefficients
#' 
#' While the principles of variance algebra hold for linear relationships, they are complicated by non-linear terms and interactions. 
#' In fact, closed form solutions for the variance of product distributions is an active research area. 
#' This function capitalizes on the power of simulation to estimate the residual variance of a response surface given
#' the expected value of the outcome variance.
#' 
#' 
#' @param n Number of Obseravtion to be simulated at each iteration.
#' @param cor_mat Correlation matrix defining how X1 and X2 are related.
#' @param beta Vector of coefficients mapping terms X1, X2, X1^2, X2^2, and X1*X2 to y.
#' @param target_var_y The expected variance of y. 
#' @param iter The number of iterations to run to estimate the residual variance.
#' 
#' @return Value of residual variance given proposed model
#' @export 
#' @importFrom magrittr %>%
#' 
#' @examples
#' 
#' # Defining Correlation Matrix describing how x1 and x2 are related
#' # Covarince of x1^2, x2^2, and x1*x2 follow from this matrix
#' cor_mat<-matrix(c(1, 0,
#'                   0, 1), byrow = TRUE, 2, 2)
#' # Defining betas x1, x2, x1^2, x2^2, and x1*x2
#' beta<-c(0, 0, -.075, -.075, .15)
#' 
#' # Simulating 10,000 draws of size 1000 assuming the correlation structure and regression weights defined above.
#' sig_hat <- find_sig(n = 1000, cor_mat = cor_mat, beta = beta, target_var_y = 1)                 
#' sig_hat

find_sig<-function(n, cor_mat, beta, target_var_y = 1, iter=10000){
  
  sig_vec<-c()
  
  for(i in 1:iter){
    simmed_data<-rrs::gen_response_surf_x(n, cor_mat)%>%
      rrs::gen_response_surf_y(beta = beta)
    
    sig_vec[[i]]<-target_var_y-var(simmed_data$y)
    
  }
  
  sig_val<-mean(sig_vec)
  
  message("To generate an outcome variable with a variance of ", target_var_y, " given your betas, use a error sd of")
  
  sig_val
}
