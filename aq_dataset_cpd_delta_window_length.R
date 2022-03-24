rmweather_no2_df = delta_comparison_london_no2 %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-07-31"),
         delta == "rmweather") %>%
  select(-delta)


delta_no2_df = delta_comparison_london_no2 %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-07-31"),
         delta == "BAU - observed (delta)") %>%
  select(-delta)

window_length_rm = c(7, 14,21, 28, 35, 42) #4, 6, 8, 12, 25%, 50% respectively
percentage_value_rm = c("4%", "6%", "8%", "12%", "25%", "25%")

write.csv(rmweather_no2_df, "rmweather_no2_df")


aq_rmweather_window_length = map_dfr(.x = window_length_rm,
                                  .f = ~window_length_constrain(df = rmweather_no2_df,
                                                                          .x, k = 7))

aq_delta_window_length = map_dfr(.x = window_length_rm,
                                .f = ~window_length_constrain(df = delta_no2_df,
                                                              .x, k = 6))
w = 35



aq_delta_window_length%>%
  filter(date >= as.Date("2019-03-01") & date <= as.Date("2019-07-30"),
         window_length_level == w, variables != "r.squareds") %>%
  ggplot(aes(x = date, y = value))+
  annotate("rect", xmin = as.POSIXct(as.Date("2019-04-08")), 
           xmax = as.POSIXct(as.Date("2019-06-15")), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  geom_line(aes(colour = variables), lwd = 1.5)+
  geom_point(data = filter(aq_rmweather_window_length, date >= as.Date("2019-03-01") & date <= as.Date("2019-07-30"),
                           cp==TRUE & variables == "Test dataset"
                           & window_length_level == w)
             , size  = 3, colour = "blue")+
  labs(x= "Date", y = bquote('Normalised'~NO[2]~(mu * g*~m^"-3"))) +
  facet_grid(vars(variables), vars(window_length_level), scales = "free_y") +
  theme_bw(base_size = 15)

linear_approx_delta_cp = map_dfr(.x = window_length_rm,
                           .f = ~change_point_model_statistics(df = aq_delta_window_length,
                                                               .x, FALSE))

linear_approx_delta_cp_stats = map_dfr(.x = window_length_rm,
                                 .f = ~change_point_model_statistics(df = aq_delta_window_length,
                                                                     .x, TRUE))


linear_approx_delta_cp %>%
  filter(date >= as.Date("2019-03-01") & date <= as.Date("2019-07-30")) %>%
  ggplot(aes(date, value)) +
  geom_line(aes(colour= variables), lwd = 1.5)+
  geom_point(data = filter(linear_approx_delta_cp, date >= as.Date("2019-03-01") & date <= as.Date("2019-07-30"),
                           cp==TRUE), colour = "blue", size = 3) +
  labs(x= "Date", y = "7 day rolling mean observed-predicted \U0394", colour = "Change point functions") +
  facet_wrap(~window_length_level)+
  theme_bw(base_size = 15)

  