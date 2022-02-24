library(openair)
library(dplyr)
library(rmweather)
library(zoo)
require(RcppRoll)
library(ggplot2)
#Processing code----------------------------------------------

london_kcl_meta = importMeta(source = "kcl") %>%
  drop_na()

london_urb_sites <- filter(
  kcl_noaa_nearest,
  site_type %in% c("Urban Background", "Roadside"))

directory_met_data = "D:/cpdav/UK_met_data/noaa_UK_met_data_"

met_london_df_all = map2_dfr(.x = london_urb_sites$code, 
                    .y = london_urb_sites$met_code,
                    .f = ~read_met_sites_london(site_code = .x, metcode = .y, 
                                                "2016-01-01", "2020-12-31"))

met_aq_london_urban_background_no2=met_aq_prepared_rm_vs_de(met_london_df_all, 
                                               "no2", "Urban Background", TRUE)

London_code_no2_urban_background = unique(as.character(met_aq_london_urban_background_no2$code))

BAU_urban_background_no2_london = map(.x = London_code_no2_urban_background,
                                       .f = ~rmweather_BAU_observed(df = met_aq_london_urban_background_no2,
                                                          site = .x, 300, "2016-01-01", 
                                                          "2019-02-28", "2020-01-31",
                                                          0.85, 300))

london_urban_background_no2_reformat = urban_reformat_data_mean_sd(BAU_urban_background_no2_london,
                                       London_code_no2_urban_background)

london_urban_background_no2_reformat %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-12-31")) %>% 
  ggplot(aes(x = date, y = -1*d7_rollavg_delta_BAU_predict_mean)) + 
  geom_line(colour = "red", lwd = 1.5)



  geom_ribbon(aes(y = d7_rollavg_delta_BAU_predict_mean, 
                  ymin = d7_rollavg_delta_BAU_predict_mean - d7_rollavg_delta_BAU_predict_sd, 
                  ymax = d7_rollavg_delta_BAU_predict_mean + d7_rollavg_delta_BAU_predict_sd, 
                  fill = time_frame), alpha = .3) +
  geom_line(aes(color = time_frame), lwd = 1.5)  + 
  labs(x= "Date", y = "7 day rolling mean observed-predicted \U0394", 
       fill = "Time frame post prediction",  
       colour = "Time frame post prediction") + 
  geom_vline(xintercept = as.POSIXct(as.Date("2020-01-31")), 
             color = "black", 
             lwd = 1,
             linetype = "dashed") + 
  annotate(geom = "text", label = as.character("Model training end period"),
           x = as.POSIXct(as.Date("2020-02-03")), y = -7,
           angle = 90,
           size = 6) +
  annotate(geom = "text", label = as.character("Coloured shaded areas \n represent SD"),
           x = as.POSIXct(as.Date("2020-10-31")), y = -12,
           size = 4) +
  coord_cartesian(xlim = c(as.POSIXct(as.Date("2020-01-01")), as.POSIXct(as.Date("2020-08-31"))), clip = "off") +
  geom_label(aes(x = as.POSIXct(as.Date("2020-05-07")), y = 0, label = "UK National Lockdown"), 
             fill = "white",
             size = 6) +
  ggtitle(vn_predicted) + theme_minimal(
    base_size = 18)
