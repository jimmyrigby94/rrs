#' plot_ly_surf: Generate Interactive Plots of Response Surface Objects
#'
#' @param obj object created by resp_surf function
#' @param max_x1 max possible value of x1. Defines limits.
#' @param min_x1 min possible value of x1. Defines limits.
#' @param max_x2 max possible value of x2. Defines limits. 
#' @param min_x2 max possible value of x2. Defines limits.
#' @param inc Increments of data to be plotted. 
#' @param x1_lab label for x axis (X1)
#' @param x2_lab label for y axis (X2)
#' @param y_lab label for z axis (Y)
#' @param showscale Show a color scale for the outcome variable via a legend.
#' @param show_princ_axis Plot the first and second principle axis
#'
#' @return Interactive plot of response surface.
#' @export 
#'
#' @examples
#' 
#' # Importing for magrittr pipe (%>%)
#' library(tidyverse)
#' 
#' # Defining Correlation Matrix describing how x1 and x2 are related
#' # Covarince and variance of x1^2, x2^2, and x1*x2 follow from this matrix
#' cov_mat<-matrix(c(1, 0,
#'                   0, 1), byrow = TRUE, 2, 2)
#' 
#' # Defining betas x1, x2, x1^2, x2^2, and x1*x2
#' beta<-c(0, 0, -.075, -.075, .15)
#' 
#' # Defining sig_hat directly to save time for example
#' sig_hat <- 0.9549575  
#' 
#' # Generating data frame for response suface examining leaders and follower agreeableness
#' simmed_df<-gen_response_surf_x(1000, cov_mat, x_names = c("L_Agree", "F_Agree"))%>%
#'   gen_response_surf_y(beta = beta, sigma = sig_hat, y_name = "Satisfaction")
#' 
#' # Fitting a Response Surface Model
#' 
#' model_1<-resp_surf(dep_var = "Satisfaction", 
#'                    fit_var = c("L_Agree", "F_Agree"), 
#'                    data = simmed_df, 
#'                    robust = FALSE)
#' 
#' # Plotting Response Surface Model 
#' 
#' plot_ly_surf(obj = model_1, 
#'              max_x = 2, 
#'              min_x = -2,
#'              max_y =  2, 
#'              min_y = -2, 
#'              inc = .1, 
#'              x1_lab = "Leader Agreeableness", 
#'              x2_lab = "Follower Agreeableness", 
#'              y_lab = "Employee Satisfaction", 
#'              showscale = TRUE)

plot_ly_surf<-function(obj = NULL, max_x = NULL, min_x = NULL, max_y = NULL, min_y = NULL, inc = NULL,  x1_lab=NULL, x2_lab=NULL, y_lab=NULL, showscale = FALSE, show_princ_axis = FALSE){
  
  #Copies the model generated in the resp_surf function for use here
  eq<-obj[["model"]]
  
  #Generates a 2xn dimension data frame with every combination of x and y
  d<-expand.grid(list(x = seq(min_x, max_x, by = inc),
                      y = seq(min_y, max_y, by = inc)))
  
  #creates quadratic and interaction terms for prediction
  d$x_sq<-d$x^2
  d$y_sq<-d$y^2
  d$int<-d$x*d$y
  
  #names the variables what you have them named in the model
  colnames(d)<-names(coef(eq)[2:6])
  
  #uses the function generated in resp_surf to predict values
  d$y<-predict(eq, d)
  
  #prepares a vector of x values for plotting
  x<-seq(min_x, max_x, by = inc)
  #Prepares a vector of y values for plotting
  y<-seq(min_y, max_y, by = inc)
  #spreads the predictions generated above into a matrix where rows correspond with x and columns correspond with y
  z <- matrix(d$y, nrow = length(x), ncol=length(y))
  
  if(is.null(x1_lab)){
    x1_lab<-names(eq$coefficients)[2]
  }
  if(is.null(x2_lab)){
    x2_lab<-names(eq$coefficients)[3]
  }
  if(is.null(y_lab)){
    y_lab<-colnames(eq$model)[1]
  }
  
  p<-plotly::plot_ly(x = x, y = y, z = ~z)%>% 
    plotly::add_surface(showscale = showscale)%>%
    plotly::add_trace(x = x[x==y], y = y[x==y], z = diag(z), line = list(color = "black"),type = 'scatter3d', mode = 'lines', name = "Line of Congruence")%>%
    plotly::add_trace(x = x, y = y[length(y):1], z = z[row(z) == (ncol(z)-col(z)+1)], line = list(color = "black", dash = 'dash'),type = 'scatter3d', mode = 'lines', name = "Line of Incongruence")%>%
    plotly::layout(scene = list(xaxis = list(title = x1_lab), 
                                yaxis = list(title = x2_lab), 
                                zaxis = list(title = y_lab)))
  
  if(show_princ_axis){
    p<-p%>%
    plotly::add_trace(x = x, 
                      y = obj$princ_axis$Estimate[1]+obj$princ_axis$Estimate[2]*x,
                      z = min(z),
                      line = list(color = "red"),type = 'scatter3d', mode = 'lines', name = "First Principal Axis")%>%
      plotly::add_trace(x = x, 
                        y = obj$princ_axis$Estimate[3]+obj$princ_axis$Estimate[4]*x,
                        z = min(z),
                        line = list(color = "red", dash = 'dash'),type = 'scatter3d', mode = 'lines', name = "Second Principal Axis")
  }
  p
}
