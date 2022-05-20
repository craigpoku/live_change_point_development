x = c(0, 300, 600, 900, 1200, 1500)
f_x = c(-5, 8, 1, 2, 3, 6)
g_x = c(-5, 8, 1, 2, 3, 4.5)



plot(x, f_x, type = "l")
plot(x, g_x, type = "l")


fun_1_df = linear_function_generator(x, f_x, tail(x, n=1), 0.01)
fun_2_df = linear_function_generator(x, g_x, tail(x, n=1), 0.01)

plot(fun_2_df, type = "l")


win_length_vector = c(6, 7, 8)

fun_1_cp_detection = map_dfr(.x = win_length_vector,
                             .f = ~window_length_constrain(df = fun_1_df,
                                                           .x, cp_factor = 1e2,
                                                           epsilon = 1e-9, date = FALSE))

fun_2_cp_detection = map_dfr(.x = win_length_vector,
                             .f = ~window_length_constrain(df = fun_2_df,
                                                           .x, cp_factor = 1e2,
                                                           epsilon = 1e-9, date = FALSE))

cp_urban_background_no2_df_all = normalised_urban_background_no2_all_sites_reformat %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30")) %>%
  select(date, d7_rollavg_normal_mean) %>%
  rename(value = d7_rollavg_normal_mean)

cp_roadside_no2_df_all = normalised_roadside_no2_all_sites_reformat %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30")) %>%
  select(date, d7_rollavg_normal_mean) %>%
  rename(value = d7_rollavg_normal_mean)

urban_background_no2_cp_detection = map_dfr(.x = win_length_vector,
                             .f = ~window_length_constrain(df = cp_urban_background_no2_df_all,
                                                           .x, cp_factor = 2.5,
                                                           epsilon = 1e-9, date = TRUE))

roadside_no2_cp_detection = map_dfr(.x = win_length_vector,
                                            .f = ~window_length_constrain(df = cp_roadside_no2_df_all,
                                                                          .x, cp_factor = 2.5,
                                                                          epsilon = 1e-9, date = TRUE))

roadside_no2_cp_detection %>%
  ggplot(aes(x = date, y = value))+
  geom_line(aes(colour = variables), lwd = 1.5)+
  geom_point(data = filter(roadside_no2_cp_detection,
                           cp==TRUE & variables %in% c("Test dataset", "2nd derivative"))
             , size  = 3, colour = "blue")+
  labs(x= "Date", y = "Various units") +
  facet_grid(vars(variables), vars(window_length_level), scales = "free_y") +
  theme_bw(base_size = 15)

urban_background_no2_cp_detection %>%
  ggplot(aes(x = date, y = value))+
  geom_line(aes(colour = variables), lwd = 1.5)+
  geom_point(data = filter(urban_background_no2_cp_detection,
                           cp==TRUE & variables %in% c("Test dataset", "2nd derivative"))
             , size  = 3, colour = "blue")+
  labs(x= "Date", y = "Various units") +
  facet_grid(vars(variables), vars(window_length_level), scales = "free_y") +
  theme_bw(base_size = 15)






