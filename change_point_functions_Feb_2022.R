#creates df with rolling regression gradient, leading to a cp detection (with no date column), 
#option to include additional statistics



solar_regress_cp = df_cp_detection(solar_response_df, 15, FALSE)
solar_regress_statistic = df_cp_detection_statistics(solar_response_df, 15, FALSE)

solar_response_df %>% 
  ggplot(aes(x = index, y = value)) +
  geom_line(lwd = 1.5, colour = "red") +
  geom_vline(data = filter(solar_regress_cp, d>5e-5, flag==TRUE),
             aes(xintercept = index))

#Note, the scale at which you choose your window will depend on how quickly you expect a change

london_cases_cp = df_cp_detection(total_london_cases_prepared, 15, TRUE)
london_cases_statistic = df_cp_detection_statistics(total_london_cases_prepared, 15, TRUE)

total_london_cases_prepared %>% 
  filter(between(date,
                 lubridate::ymd("2020-08-01"),
                 lubridate::ymd("2020-12-01"))) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line(lwd = 1.5, colour = "red") +
  geom_vline(data = filter(london_cases_cp, flag==TRUE),
             aes(xintercept = date)) +
  labs(x = "Date", y = "7 Day rolling average: Daily Cases")