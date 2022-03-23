library(lubridate)
library(Metrics)

change_point_model_statistics = function(df, window_length, stats = TRUE){
  cp_df = df %>%
    filter(variables == "Test dataset", cp == TRUE, window_length_level == window_length) %>%
    mutate(date = as_date(date))%>%
    select(date, value)
  
  ApproxFun = approxfun(x = cp_df$date, y = cp_df$value)
  Dates_seq <- seq.Date(ymd(df$date[1]), ymd(tail(df$date, n=1)), by = 1)
  LinearFit = ApproxFun(Dates_seq)
  
  df_approx = data.frame(Dates_seq, LinearFit)%>%
    rename(date = Dates_seq, approx_value = LinearFit)
  
  df_new = df %>%
    filter(window_length_level == window_length, variables == "Test dataset") 
  
  if(stats == TRUE){
    
    df_combo_temp = left_join(df_new, df_approx, by = "date") %>%
      drop_na() %>%
      select(date, value, approx_value)
    
    rmse = Metrics::rmse(df_combo_temp$value, df_combo_temp$approx_value)
    rsquared = cor(df_combo_temp$value, df_combo_temp$approx_value, method ="pearson")
    df_combo = data.frame(window_length, rmse, rsquared)
  }
  else{
    df_combo = left_join(df_new, df_approx, by = "date") %>%
      drop_na() %>%
      select(date, value, approx_value) %>%
      pivot_longer(-date) 
  }
  
  return(df_combo)
}

test_cp_14 = change_point_model_statistics(aq_test_window_length, 7, TRUE)



df_test %>%
  ggplot(aes(date, value)) +
  geom_line(aes(colour= name))
