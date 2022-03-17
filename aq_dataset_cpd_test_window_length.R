rmweather_no2_df = delta_comparison_london_no2 %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30"),
         delta == "rmweather") %>%
  select(-delta)


print(dim(rmweather_no2_df))

plot(rmweather_no2_df, type = "l")

window_length_rm = c(6, 7, 8, 9, 10, 11) #4, 6, 8, 12, 25%, 50% respectively
percentage_value_rm = c("4%", "6%", "8%", "12%", "25%", "25%")

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
aq_test_window_length%>%
  filter(date >= as.Date("2019-04-01") & date <= as.Date("2019-06-15"),
        window_length_level == 8, variables == "Test dataset") %>%
  ggplot(aes(x = date, y = value))+
  annotate("rect", xmin = as.POSIXct(as.Date("2019-04-08")), 
           xmax = as.POSIXct(as.Date("2019-06-15")), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  geom_line(colour = "red", lwd = 1.5)+
  geom_point(data = ~filter(.x, variables == "Test dataset" & cp==TRUE)
             , colour = "blue", size = 4)+
  labs(x= "Date", y = bquote('Normalised'~NO[2]~(mu * g*~m^"-3")))+
  geom_label(aes(x = as.POSIXct(as.Date("2019-05-07")), y = 30, label = "ULEZ implementation"), 
             fill = "white",
             size = 10) +
  theme_bw(base_size = 20)

#,
#window_length_level == 8

  