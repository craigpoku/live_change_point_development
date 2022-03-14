
directory_met_data = "D:/cpdav/UK_met_data/noaa_UK_met_data_"


kc1_met_df = read.csv(paste(directory_met_data, "036720-99999",".csv", sep="")) %>%
  mutate(date = lubridate::ymd_hms(date)) %>%
  filter(date >= as.Date("2016-01-01") & date <= as.Date("2020-12-31")) %>%
  rename(met_code = code)

kc1_aq_df = importKCL(site = "KC1", year = 2016:2020, meta = TRUE)

kc1_df = kc1_aq_df %>% 
  select(-any_of(c("ws", "wd", "air_temp", "latitude", "longitude"))) %>% 
  left_join(., kc1_met_df, by = "date")

kc1_rm = kc1_df %>%
  select(code, site_type, met_code, station, date, all_of("no2"), 
         ws, wd, air_temp, atmos_pres, RH, cl)%>%
  filter(site_type == "Urban Background") %>%
  pivot_longer(-c(code, site_type, met_code, station, date, ws, wd, air_temp, atmos_pres, RH, cl),
               names_to = "pollutant") %>%
  select(-site_type) %>%
  rename(atmospheric_pressure = atmos_pres) %>%
  filter(!is.na(value))

kc1_prepared_train = kc1_rm %>%
  filter(date <= as.Date("2019-01-28"))%>%
  rmw_prepare_data(na.rm = TRUE)

kc1_prepared_predict = kc1_rm %>%
  filter(date <= as.Date("2020-01-31")) %>%
  rmw_prepare_data(na.rm = TRUE)

kc1_variables_atmos = c("ws","wd","air_temp",
         "RH", "atmospheric_pressure", "date_unix", "day_julian", "weekday", "hour")

kc1_df_train = rmw_train_model(kc1_prepared_train,
                              variables = kc1_variables_atmos,
                              n_trees = 300,
                              verbose = TRUE)

kc1_df_normal = rmw_do_all(df = kc1_prepared_predict,
  variables = kc1_variables_atmos,
  n_trees = 50,
  n_samples = 50)


kc1_df_normal_time_series = kc1_df_normal$normalised %>%
  timeAverage(avg.time = "1 day")

plot(kc1_df_normal_time_series, type = "l")

plot(kc1_df_normal$normalised$value_predict, type = "l")

kc1_df_predict = kc1_prepared_predict %>% mutate(value_predict = rmw_predict(
  kc1_df_train, 
  df = kc1_prepared_predict)) %>%
  mutate(normal = kc1_df_normal$normalised$value_predict)
  select(date, value, value_predict) %>%
  rename(observed = value, BAU = value_predict)%>%
  timeAverage(avg.time = "1 day") %>%
  mutate(delta = observed - BAU,
         roll_7_delta = zoo::rollmean(delta, k = 7, align = "right", fill = NA))

?rollapply


kc1_df_predict_cp = kc1_df_predict %>%
  select(date, roll_7_delta) %>%
  rename(value = roll_7_delta) %>%
  drop_na()

kc1_cp_detection = df_cp_detection(kc1_df_predict_cp, 90, 30, TRUE)

kc1_test_plot = kc1_df_predict %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-07-31"))  %>%
  ggplot(aes(date, roll_7_delta))+
  geom_line(colour = "red")+
  geom_vline(data = filter(kc1_cp_detection, flag),
             aes(xintercept = date))

ggplotly(kc1_test_plot)


