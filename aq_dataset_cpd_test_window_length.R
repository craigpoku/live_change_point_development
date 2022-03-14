rmweather_no2_df = delta_comparison_london_no2 %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30"),
         delta == "rmweather") %>%
  select(-delta)


print(dim(rmweather_no2_df))

plot(rmweather_no2_df, type = "l")

window_length_rm = c(7, 14 ,21, 45, 90) #4, 8, 12, 25%, 50% respectively
percentage_value_rm = c("4%", "8%", "12%", "25%", "50%")

window_length_constrain = function(df, window_length_vector, percentage_length_vector){
  
  roll_regression = rollRegres::roll_regres(value ~ date, df, 
                                                  width = window_length_vector,
                                                  do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
  
  roll_reformat_cp = as.data.frame(roll_regression$coefs) %>%
    rename(grad = date) %>%
    mutate(date = df$date,
           data = df$value,
           window_length_level = as.factor(window_length_vector),
           percentage_level = as.factor(percentage_length_vector),
           derv_2nd = pracma::gradient(grad),
           rollmax = rollmax(derv_2nd, k = 4, align = "right", fill = NA),
           cp = rollmax-lag(rollmax) > 0 & rollmax == lead(rollmax, 2)
    ) %>%rename("Test dataset" = data,
                "Rolling gradient" = grad,
                "2nd derivative" = derv_2nd,
                "Applied rollmax" = rollmax)%>%
    select(-"(Intercept)") %>%
    drop_na() %>%
    pivot_longer(-c(date, window_length_level, percentage_level, cp), names_to = "variables")%>%
    mutate(variables = factor(variables, 
                              levels = c("Test dataset", "Rolling gradient", "2nd derivative",
                                         "Applied rollmax")))
  
  return(roll_reformat_cp)
}

aq_test_window_length = map2_dfr(.x = window_length_rm, .y = percentage_value_rm,
                                  .f = ~window_length_constrain(df = rmweather_no2_df,
                                                                          .x, .y))
aq_test_window_length %>%
  ggplot(aes(x = date, y = value))+
  geom_line(aes(colour = variables), lwd = 1.2)+
  geom_point(data = ~filter(.x, variables == "Test dataset" & cp==TRUE)
             , colour = "blue", size = 2)+
  labs(x= "Scan line", y = "Solar elevation angle [deg]") +
  facet_grid(variables~percentage_level, 
             scales = "free_y")
