rmweather_no2_df = delta_comparison_london_no2 %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-07-31"),
         delta == "rmweather") %>%
  select(-delta)


print(dim(rmweather_no2_df))

plot(rmweather_no2_df, type = "l")

window_length_rm = c(7, 14,21, 28, 35, 42) #4, 6, 8, 12, 25%, 50% respectively
percentage_value_rm = c("4%", "6%", "8%", "12%", "25%", "25%")

window_length_constrain = function(df, window_length_vector, percentage_length_vector){
  
  roll_regression = rollRegres::roll_regres(value ~ date, df, 
                                                  width = window_length_vector,
                                                  do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
  
  roll_reformat_cp = as.data.frame(roll_regression$coefs) %>%
    rename(grad = date) %>%
    mutate(date = df$date,
           r.squareds = roll_regression$r.squareds,
           data = df$value,
           grad = round(grad, 7),
           window_length_level = as.factor(window_length_vector),
           percentage_level = as.factor(percentage_length_vector),
           derv_2nd = abs(pracma::gradient(grad)),
           rollmax = rollmax(derv_2nd, k = 1, align = "right", fill = NA),
           cp = rollmax-lag(rollmax) > 0 & rollmax == lead(rollmax, 1)
    ) %>%rename("Test dataset" = data,
                "Rolling gradient" = grad,
                "2nd derivative" = derv_2nd,
                "Applied rollmax" = rollmax)%>%
    select(-"(Intercept)") %>%
    drop_na() %>%
    pivot_longer(-c(date, window_length_level, percentage_level, cp), 
                 names_to = "variables")%>%
    mutate(variables = factor(variables, 
                              levels = c("Test dataset", "Rolling gradient", "2nd derivative",
                                         "Applied rollmax", "r.squareds")))
  
  return(roll_reformat_cp)
}

write.csv(rmweather_no2_df, "rmweather_no2_df")


aq_test_window_length = map2_dfr(.x = window_length_rm, .y = percentage_value_rm,
                                  .f = ~window_length_constrain(df = rmweather_no2_df,
                                                                          .x, .y))
w = 14

aq_test_window_length%>%
  filter(date >= as.Date("2019-03-01") & date <= as.Date("2019-07-30"),
         window_length_level == w, variables != "r.squareds") %>%
  ggplot(aes(x = date, y = value))+
  annotate("rect", xmin = as.POSIXct(as.Date("2019-04-08")), 
           xmax = as.POSIXct(as.Date("2019-06-15")), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  geom_line(aes(colour = variables), lwd = 1.5)+
  geom_point(data = filter(aq_test_window_length, date >= as.Date("2019-03-01") & date <= as.Date("2019-07-30"),
                           cp==TRUE & variables == "Test dataset"
                           & window_length_level == w)
             , size  = 3, colour = "blue")+
  labs(x= "Date", y = bquote('Normalised'~NO[2]~(mu * g*~m^"-3"))) +
  facet_grid(vars(variables), vars(window_length_level), scales = "free_y") +
  theme_bw(base_size = 15)

#
#,
#window_length_level == 8

  