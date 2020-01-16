rrs: An R Package for Robust Response Surface Analysis
================
James Rigby
1/16/2020

`rrs` is an R package intended to help researchers conduct response
surface analysis. While our primary motivation was to create a package
that uses cluster robust standard errors, analysts will find that the
package can accommodate non-clustered data as well as data that exhibits
heteroskedasticty. The workhorse of this package is `resp_surf` which
conducts response surface analysis. There are, however, several helper
function that facilitate response surface analysis. For example,
`gen_response_surf_x()` and `gen_response_surf_y()` simulate response
surface data and are useful when formulating hypotheses and conducting
power analysis. `plot_ly_surf()` generates interactive plots and through
which the user can create still .png files. Users are invited to explore
the documentation and watch this repository for future developments,
including a dedicated wrapper for calculating power.

## Installation

To install `rss` run the following code.

``` r
devtools::install_github("jimmyrigby94/rrs")
```

## A Few Quick Examples

### Simulation

Simulation of response surface models requires a few things. First,
users must provide the covariance matrix of the two focal variables.
Second, users must provide the regression coefficients associated with
the linear, quadratic, and interaction terms within the polynomial
model. Third, the user must provide the number of observations they
would like to simulate. Finally, the user needs to provide the residual
variance of the outcome variable.

This final step causes problems for polynomial regression, especially
when the user has an idea of what the total variance of the outcome
variable should be. The issue stems from the calculation of the variance
of the product of two variables, which doesn’t have a closed form
solution in the general case. To sidestep this issue and provide users
control of the variance of simulated variables, we capitalized on the
algebra of random variables and regression model assumptions. The
regression model assumes that

\(var(y) = var(\hat{y} + \epsilon)\)

Expanding this term we get

\(var(y) = var(\hat{y}) + var(\epsilon) + 2\ cov(\hat{y}, \epsilon)\)

Given our model’s assumptions, we can set \(cov(\hat{y}, \epsilon)\) to
0. Which leaves us with the fact that

\(var(y) = var(\hat{y}) + var(\epsilon)\)

\(var(\hat{y})\) is given exclusively by the regression weights and
covariances provided by the user. Thus, we can solve for the error
variance of a model by providing the desired variance of y and
subtracting the variance of the predicted value.

\(var(y) - var(\hat y)\)

`find_sig()` does just that, except it does it for large data sets
thousands of times. This allows us to be exceptionally precise in our
specifications of error variance when conducting simulations.

Let’s say a user wants to simulate a fully standardized response
surface, meaning that all independent and dependent variables have a
variance equal to 1. Furthermore, let’s say that the user wants the
independent variables to be uncorrelated and the underlying regression
model should be

\(y = 0\ X_1+0\ X_2+-.075\ X_1^2+-.075\ X_2^2+.15\ X_1X_2\) This is
easily done using `rrs`.

``` r
# Setting Seed for replicability
set.seed(9999)

# Importing for magrittr pipe (%>%)
library(tidyverse)
```

    ## Warning: package 'purrr' was built under R version 3.6.2

``` r
library(rrs)

# Defining Necessary Parameters ------------------------------------------
# Defining Correlation Matrix describing how x1 and x2 are related
# Covarince and variance of x1^2, x2^2, and x1*x2 follow from this matrix
cov_mat<-matrix(c(1, 0,
                  0, 1), byrow = TRUE, 2, 2)

# Defining betas x1, x2, x1^2, x2^2, and x1*x2
beta<-c(0, 0, -.075, -.075, .15)

# Solving for the Error Variance -----------------------------------------
# Simulating 10,000 draws of size 10000 assuming the correlation structure and regression weights defined above.
sig_hat <- find_sig(n = 10000, cov_mat = cov_mat, beta = beta, target_var_y = 1)

# Simulating Data
simmed_df<-gen_response_surf_x(1000, cov_mat, x_names = c("L_Agree", "F_Agree"))%>%
  gen_response_surf_y(beta = beta, sigma = sig_hat, y_name = "Satisfaction")

# Sample variance approaches the target variance. 
var(simmed_df$Satisfaction)
```

    ## [1] 0.9789751

### Response Surface Analysis

Let’s fit a model using the simulated data. This can be done using
`resp_surf()`.

``` r
model_1<-resp_surf(dep_var = "Satisfaction", 
                   fit_var = c("L_Agree", "F_Agree"), 
                   data = simmed_df, 
                   robust = FALSE)
```

The output tells us how many people were approximately congruent, and of
the people who were in-congruent, how many exhibited a surplus or a
deficit.

``` r
model_1$dif_tab
```

    ## 
    ##            L_Agree < F_Agree L_Agree congruent to F_Agree 
    ##                         0.37                         0.27 
    ##            L_Agree > F_Agree 
    ##                         0.36

The output also provides estimates for the regression weights, lines of
interest, stationary points, and principle axes.

``` r
model_1$results
```

    ##                    Estimates Standard_Error     T_Test P_value
    ## (Intercept)      0.011205891     0.04327411  0.2589514  0.7957
    ## L_Agree          0.050569283     0.03069285  1.6475918  0.0998
    ## F_Agree         -0.008160689     0.03205734 -0.2545654  0.7991
    ## L_Agree_sq      -0.107990764     0.02147124 -5.0295539  0.0000
    ## F_Agree_sq      -0.051076183     0.02447246 -2.0870885  0.0371
    ## L_Agree:F_Agree  0.162253312     0.03131845  5.1807580  0.0000

``` r
model_1$loi
```

    ##       Line_of_Interest Parameter     Estimate Standard_Error      T_Test
    ## 1   Line of Congruence    Linear  0.042408595     0.04522003  0.93782762
    ## 2   Line of Congruence Quadratic  0.003186364     0.04483101  0.07107501
    ## 3 Line of Incongruence    Linear  0.058729972     0.04352697  1.34927787
    ## 4 Line of Incongruence Quadratic -0.321320259     0.04475427 -7.17965612
    ##        P_value
    ## 1 3.485608e-01
    ## 2 9.433524e-01
    ## 3 1.775550e-01
    ## 4 1.367281e-12

``` r
model_1$stat_pnt
```

    ##                    X.      Value
    ## F_Agree X Coordinates -0.9011427
    ## L_Agree Y Coordinates -1.5112139

``` r
model_1$princ_axis
```

    ##     Axis     Param   Estimate
    ## 1  First Intercept -0.2401398
    ## 2  First     Slope  1.4105137
    ## 3 Second Intercept -2.1500895
    ## 4 Second     Slope -0.7089616

### Plotting Functions

Finally, this can all be plotted using a wrapper that builds the
response surface plot in an interactive fashion. While the interactive
version of this document cannot be displayed in the README here is a
still shot taken from the function output.

``` r
plot_ly_surf(obj = model_1, 
             max_x = 2, 
             min_x = -2,
             max_y =  2, 
             min_y = -2, 
             inc = .1, 
             x1_lab = "Leader Agreeableness", 
             x2_lab = "Follower Agreeableness", 
             y_lab = "Employee Satisfaction", 
             showscale = TRUE)
```

![Still frame of plot\_ly\_surf() output.](README%20Example.png)

### Future Developments
1. Wrapper for the power calculations of line of congruence and line of incongruence.  
2. Formula interface for resp_surf.
3. Reworking internals of resp_surf using matrix algbra. 
4. Allowing arbitrary vcov function to be used for standard error calculation. 
