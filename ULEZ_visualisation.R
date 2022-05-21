
win_length_vector = c(7)

ULEZ_compliance_df = read.csv("~/Github/live_change_point_development/ULEZVehiclesCharges_PCs.csv") %>%
  rename(date = "Date.of.Travel",
         total_vehicle_number = "Unique.VRMs.Captured.in.Charging.Hours",
         compliant_number = "Unique.VRMs.Compliant.with.ULEZ",
         non_compliant_number = "Unique.VRMs.Non.Compliant.with.ULEZ") %>%
  select(c(date, total_vehicle_number, compliant_number, non_compliant_number)) %>%
  filter(total_vehicle_number!="Not Yet Calculated") %>%
  mutate(date = substring(date, 5),
         date = as.Date(date, format = "%d/%m/%Y"),
         total_vehicle_number = as.numeric(gsub(",","",total_vehicle_number)),
         compliant_number = as.numeric(gsub(",","",compliant_number)),
         non_compliant_number = as.numeric(gsub(",","",non_compliant_number)),
         compliant_percentage = (compliant_number/total_vehicle_number)*100.,
         d7_rollavg_compliant_percentage = roll_mean(compliant_percentage, n = 7, align = "right", 
                                               fill = NA))%>%
  drop_na()


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

compliance_statistics_cp_df = ULEZ_compliance_df  %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30")) %>%
  select(date, d7_rollavg_compliant_percentage) %>%
  rename(value = d7_rollavg_compliant_percentage) 



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

compliance_statistics_cp_detection_ULEZ = map_dfr(.x = 6,
                                          .f = ~window_length_constrain(df = compliance_statistics_cp_df,
                                                                        .x, cp_factor = 2,
                                                                        epsilon = 0, date = TRUE))

compliance_statistics_cp_detection_ULEZ_plot = compliance_statistics_cp_detection_ULEZ %>%
  filter(date >= as.Date("2019-03-15"), variables=="Test dataset")%>%
  ggplot(aes(x = date, y = value))+
  annotate("rect", xmin = as.Date("2019-04-08"), 
           xmax = as.Date("2019-06-30"), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  geom_line(colour = "red", lwd = 1.5)+
  geom_point(data = filter(compliance_statistics_cp_detection_ULEZ,
                           cp==TRUE & date >= as.Date("2019-03-15") & variables %in% c("Test dataset"))
             , size  = 3, colour = "blue")+
  labs(x= "Date")+
  coord_cartesian(xlim = c(as.Date("2019-03-15"), as.Date("2019-06-30")))+
  ylab(quickText("Rolling 7 day ULEZ vehicle compliance (%)")) 


roadside_pm25_cp_detection_ULEZ_plot = roadside_pm25_cp_detection_ULEZ %>%
  filter(date >= as.Date("2019-03-15"), variables=="Test dataset")%>%
  ggplot(aes(x = date, y = value))+
  annotate("rect", xmin = as.POSIXct(as.Date("2019-04-08")), 
           xmax = as.POSIXct(as.Date("2019-06-30")), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  geom_line(colour = "orange", lwd = 1.5)+
  geom_point(data = filter(roadside_pm25_cp_detection_ULEZ,
                           cp==TRUE & date >= as.Date("2019-03-15") & variables %in% c("Test dataset"))
             , size  = 3, colour = "blue")+
  labs(x= "Date")+
  ylab(quickText("Rolling 7 day normal pm25 (ug/m3)")) +
  geom_label(aes(x = as.POSIXct(as.Date("2019-06-01")), y = 17, label = "ULEZ implementation"), 
             fill = "white",
             size = 6)+ggtitle("Applied CPD example - ULEZ AQ vs compliance")+ 
  theme(plot.title = element_text(size = 20, face = "bold"))

roadside_no2_cp_detection_ULEZ_plot = roadside_no2_cp_detection_ULEZ %>%
  filter(date >= as.Date("2019-03-15"), variables=="Test dataset")%>%
  ggplot(aes(x = date, y = value))+
  annotate("rect", xmin = as.POSIXct(as.Date("2019-04-08")), 
           xmax = as.POSIXct(as.Date("2019-06-30")), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  geom_line(colour = "green", lwd = 1.5)+
  geom_point(data = filter(roadside_no2_cp_detection_ULEZ,
                           cp==TRUE & date >= as.Date("2019-03-15") & variables %in% c("Test dataset"))
             , size  = 3, colour = "blue")+
  labs(x= "Date") +
  ylab(quickText("Rolling 7 day normal no2 (ug/m3)"))

roadside_pm25_cp_detection_ULEZ_plot/roadside_no2_cp_detection_ULEZ_plot/compliance_statistics_cp_detection_ULEZ_plot





