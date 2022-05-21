library(plotly)


x = c(0, 300, 600, 900, 1200, 1500)
f_x = c(-5, 8, 1, 2, 3, 6)
g_x = c(-5, 8, 1, 2, 3, 4.5)



plot(x, f_x, type = "l")
plot(x, g_x, type = "l")


fun_1_df = linear_function_generator(x, f_x, tail(x, n=1), 0.01)
fun_2_df = linear_function_generator(x, g_x, tail(x, n=1), 0.01)

plot(fun_2_df, type = "l")


win_length_vector = c(7)

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

cp_roadside_pm25_df_ULEZ = normalised_roadside_pm25_ULEZ_reformat %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30")) %>%
  select(date, d7_rollavg_normal_mean) %>%
  rename(value = d7_rollavg_normal_mean)

cp_roadside_no2_df_ULEZ = normalised_roadside_no2_ULEZ_reformat %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30")) %>%
  select(date, d7_rollavg_normal_mean) %>%
  rename(value = d7_rollavg_normal_mean)

urban_background_no2_cp_detection = map_dfr(.x = win_length_vector,
                             .f = ~window_length_constrain(df = cp_urban_background_no2_df_all,
                                                           .x, cp_factor = 2.5,
                                                           epsilon = 1e-9, date = TRUE))

roadside_no2_cp_detection_all_sites = map_dfr(.x = win_length_vector,
                                            .f = ~window_length_constrain(df = cp_roadside_no2_df_all,
                                                                          .x, cp_factor = 2.5,
                                                                          epsilon = 1e-9, date = TRUE))

roadside_no2_cp_detection_ULEZ = map_dfr(.x = win_length_vector,
                                              .f = ~window_length_constrain(df = cp_roadside_no2_df_ULEZ,
                                                                            .x, cp_factor = 2,
                                                                            epsilon = 1e-9, date = TRUE))
roadside_pm25_cp_detection_ULEZ = map_dfr(.x = win_length_vector,
                                         .f = ~window_length_constrain(df = cp_roadside_pm25_df_ULEZ,
                                                                       .x, cp_factor = 2,
                                                                       epsilon = 1e-9, date = TRUE))



roadside_pm25_cp_detection_ULEZ_plot = roadside_pm25_cp_detection_ULEZ %>%
  filter(date >= as.Date("2019-02-15"), variables=="Test dataset")%>%
  ggplot(aes(x = date, y = value))+
  annotate("rect", xmin = as.POSIXct(as.Date("2019-04-08")), 
           xmax = as.POSIXct(as.Date("2019-06-30")), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  geom_line(colour = "red", lwd = 1.5)+
  geom_point(data = filter(roadside_pm25_cp_detection_ULEZ,
                           cp==TRUE & date >= as.Date("2019-02-15") & variables %in% c("Test dataset"))
             , size  = 3, colour = "blue")+
  labs(x= "Date")+
  ylab(quickText("Normalised pm25 (ug/m3)")) +
  geom_label(aes(x = as.POSIXct(as.Date("2019-06-01")), y = 15.5, label = "ULEZ implementation"), 
             fill = "white",
             size = 6)

roadside_no2_cp_detection_ULEZ_plot = roadside_no2_cp_detection_ULEZ %>%
  filter(date >= as.Date("2019-02-15"), variables=="Test dataset")%>%
  ggplot(aes(x = date, y = value))+
  annotate("rect", xmin = as.POSIXct(as.Date("2019-04-08")), 
           xmax = as.POSIXct(as.Date("2019-06-30")), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  geom_line(colour = "red", lwd = 1.5)+
  geom_point(data = filter(roadside_no2_cp_detection_ULEZ,
                           cp==TRUE & date >= as.Date("2019-02-15") & variables %in% c("Test dataset"))
             , size  = 3, colour = "blue")+
  labs(x= "Date")+
  ylab(quickText("Normalised no2 (ug/m3)"))

roadside_pm25_cp_detection_ULEZ_plot/roadside_no2_cp_detection_ULEZ_plot





