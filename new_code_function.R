library(patchwork)
library(dplyr)
library(rmweather)
library(ranger)
library(ggplot2)
library(worldmet)
library(openair)
library(tidyr)
library(foreach)
library(purrr)
require(RcppRoll)
library(zoo)
library(plotly)
library(rpatrec)
library(lubridate)
library(Metrics)


#-----------Updated fucntion that determines the CP based on ratio of previous to current point using the 2nd derivative ----------

window_length_constrain = function(df, window_length_vector, epsilon, cp_factor, date = TRUE){
  
  if(date==TRUE){
    roll_regression = rollRegres::roll_regres(value ~ date, df, 
                                              width = window_length_vector,
                                              do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
    
    
    roll_reformat_cp = as.data.frame(roll_regression$coefs) %>%
      rename(grad = date) %>%
      mutate(date = df$date,
             r.squareds = roll_regression$r.squareds,
             data = df$value,
             window_length_level = as.factor(window_length_vector),
             derv_2nd = as.numeric(abs(pracma::gradient(grad))),
             cp = derv_2nd/lag(derv_2nd) > cp_factor &
               lag(derv_2nd) > epsilon,
      ) %>%rename("Test dataset" = data,
                  "Rolling gradient" = grad,
                  "2nd derivative" = derv_2nd)%>%
      select(-"(Intercept)") %>%
      drop_na() %>%
      pivot_longer(-c(date, window_length_level, cp), 
                   names_to = "variables")%>%
      mutate(variables = factor(variables, 
                                levels = c("Test dataset", "Rolling gradient", "2nd derivative"
                                           , "r.squareds")))
    
    return(roll_reformat_cp)
  }
  else{
    roll_regression = rollRegres::roll_regres(value ~ index, df, 
                                              width = window_length_vector,
                                              do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
    
    
    roll_reformat_cp = as.data.frame(roll_regression$coefs) %>%
      rename(grad = index) %>%
      mutate(index = df$index,
             r.squareds = roll_regression$r.squareds,
             data = df$value,
             window_length_level = as.factor(window_length_vector),
             derv_2nd = as.numeric(abs(pracma::gradient(grad))),
             cp = derv_2nd/lag(derv_2nd) > cp_factor &
               derv_2nd-lag(derv_2nd) > 0 &
               lag(derv_2nd) > epsilon
      ) %>%rename("Test dataset" = data,
                  "Rolling gradient" = grad,
                  "2nd derivative" = derv_2nd)%>%
      select(-"(Intercept)") %>%
      drop_na() %>%
      pivot_longer(-c(index, window_length_level, cp), 
                   names_to = "variables")%>%
      mutate(variables = factor(variables, 
                                levels = c("Test dataset", "Rolling gradient", "2nd derivative"
                                           , "r.squareds")))
    
    return(roll_reformat_cp)
  }
}

linear_function_generator = function(index, df, seq, epsilon){
  f = approxfun(x = index, y = df)
  seq_range_x = c(1:seq) 
  linear_fit = f(seq_range_x)
  
  f_x_df = data.frame(seq_range_x, linear_fit) %>%
    rename(index = seq_range_x, value = linear_fit) %>%
    mutate(value = jitter(value, factor=epsilon, amount = NULL))%>%
    drop_na()
  
  return(f_x_df)
}