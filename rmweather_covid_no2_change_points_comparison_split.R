library(dplyr)
library(rmweather)
library(ranger)
library(ggplot2)
library(worldmet)
library(openair)
library(tidyr)
library(foreach)
library(purrr)
require(RcppRoll)
library(zoo)
library(plotly)

#Processing code----------------------------------------------

aurn_noaa_nearest = readRDS("aurn_noaa_nearest_COVID.RDS")

aurn_meta <- importMeta(source = "aurn", all=TRUE)

urb_sites <- filter(
  aurn_meta,
  site_type %in% c("Urban Background", "Urban Traffic"))


aurn_noaa_nearest_urb_sites <- filter(
  aurn_noaa_nearest,
  site_type %in% c("Urban Background", "Urban Traffic")
)

directory_met_data = "D:/cpdav/UK_met_data_covid/noaa_UK_met_data_"

met_uk_df_all = map2_dfr(.x = aurn_noaa_nearest_urb_sites$code, 
                         .y = aurn_noaa_nearest_urb_sites$met_code,
                         .f = ~read_met_sites(site_code = .x, metcode = .y, "2017-01-01", "2020-12-31"))

pollutant_types = c("o3", "nox", "no", "no2", "pm10")



met_aq_prepared_rmweather_urban_background_no2=met_aq_prepared_rm_vs_de(met_uk_df_all, 
                                           "no2", "Urban Background", TRUE)

met_aq_prepared_rmweather_urban_traffic_no2=met_aq_prepared_rm_vs_de(met_uk_df_all, 
                                           "no2", "Urban Traffic", TRUE)


UK_data_code_no2_urban_background = unique(as.character(met_aq_prepared_rmweather_urban_background_no2$code))

UK_data_code_no2_urban_traffic = unique(as.character(met_aq_prepared_rmweather_urban_traffic_no2$code))

BAU_urban_background_no2_control = map(.x = UK_data_code_no2_urban_background,
                                       .f = ~rmweather_BAU_observed(df = met_aq_prepared_rmweather_urban_background_no2,
                                       site = .x, 300, "2017-01-01", 
                                       "2020-01-31", "2020-08-31",
                                       0.85, 300))


urban_background_no2_control_reformat = urban_reformat_data_mean_sd(BAU_urban_background_no2_control,
                                        UK_data_code_no2_urban_background)

urban_background_no2_validation = urban_background_no2_control_reformat %>%
  filter(date >= as.Date("2020-02-01") & date <= as.Date("2020-02-29")) %>%
  select(date, d7_rollavg_CI_lower, d7_rollavg_CI_upper, d7_rollavg_observed_mean, 
         d7_rollavg_BAU_mean) %>%
  pivot_longer(-c(date, d7_rollavg_CI_lower, d7_rollavg_CI_upper), names_to = "model_output")

urban_background_no2_one_month = urban_background_no2_control_reformat %>%
  filter(date <= as.Date("2020-03-31"))

urban_background_no2_two_months = urban_background_no2_control_reformat %>%
  filter(date <= as.Date("2020-04-30"))

urban_background_no2_three_months = urban_background_no2_control_reformat %>%
  filter(date <= as.Date("2020-05-31")) 

urban_background_no2_six_months = urban_background_no2_control_reformat %>%
  filter(date <= as.Date("2020-08-31"))


urban_background_no2_cp_df = rbind(
  urban_background_no2_one_month %>% mutate(time_frame = "One Month"),
  urban_background_no2_two_months %>% mutate(time_frame = "Two Months"),
  urban_background_no2_three_months %>% mutate(time_frame = "Three Months"),
  urban_background_no2_six_months %>% mutate(time_frame = "Six Months")
) %>%
  mutate(time_frame = factor(time_frame, levels=c("One Month", "Two Months", "Three Months", "Six Months")))



#adding CPD tests - live? 

urban_background_no2_six_months_cp_prepared = urban_background_no2_six_months %>%
  select(date, d7_rollavg_delta_BAU_predict_mean) %>%
  rename(value = d7_rollavg_delta_BAU_predict_mean) %>%
  tibble() %>%
  drop_na()

urban_background_no2_one_month_cp_prepared = urban_background_no2_one_month %>%
  select(date, d7_rollavg_delta_BAU_predict_mean) %>%
  rename(value = d7_rollavg_delta_BAU_predict_mean) %>%
  tibble() %>%
  drop_na()

no2_one_month_cp_prepared = df_cp_prepared(urban_background_no2_one_month, 
                                  "d7_rollavg_delta_BAU_predict_mean")

no2_two_months_cp_prepared = df_cp_prepared(urban_background_no2_two_months, 
                                   "d7_rollavg_delta_BAU_predict_mean")

no2_three_months_cp_prepared = df_cp_prepared(urban_background_no2_three_months, 
                                    "d7_rollavg_delta_BAU_predict_mean")

no2_six_months_cp_prepared = df_cp_prepared(urban_background_no2_six_months, 
                                    "d7_rollavg_delta_BAU_predict_mean")

one_month_cp = df_cp_detection(no2_one_month_cp_prepared, 6, 2, TRUE)
two_months_cp = df_cp_detection(no2_two_months_cp_prepared, 6, 2, TRUE)
three_months_cp = df_cp_detection(no2_three_months_cp_prepared, 6, 2, TRUE)
six_months_cp = df_cp_detection(no2_six_months_cp_prepared, 6, 2, TRUE)

month_cp_df = rbind(
  one_month_cp %>% mutate(time_frame = "One Month"),
  two_months_cp %>% mutate(time_frame = "Two Months"),
  three_months_cp %>% mutate(time_frame = "Three Months")
) %>%
  mutate(time_frame = factor(time_frame, levels=c("One Month", "Two Months", "Three Months")))

urban_background_no2_cp_plot = urban_background_no2_cp_df %>%
  filter(date >= as.Date("2020-03-01") & date <= as.Date("2020-06-30"), time_frame != "Six Months") %>% 
  ggplot(aes(x = date, y = d7_rollavg_delta_BAU_predict_mean)) +
  annotate("rect", xmin = as.POSIXct(as.Date("2020-03-23")), 
           xmax = as.POSIXct(as.Date("2020-05-31")), ymin = -Inf, ymax = Inf, 
           alpha = .2) + 
  geom_ribbon(aes(y = d7_rollavg_delta_BAU_predict_mean, 
                  ymin = d7_rollavg_delta_BAU_predict_mean - d7_rollavg_delta_BAU_predict_sd, 
                  ymax = d7_rollavg_delta_BAU_predict_mean + d7_rollavg_delta_BAU_predict_sd, 
                  fill = time_frame), alpha = .3) +
  geom_line(aes(color = time_frame), lwd = 1.5)+
  geom_vline(data = filter(month_cp_df, flag,date >= as.Date("2020-03-01") & 
                             date <= as.Date("2020-06-30")),
             aes(xintercept = date, colour = time_frame))  +
  facet_grid(time_frame~., scales = "free_x") + theme(panel.spacing = unit(2, "lines"))+ 
  geom_vline(xintercept = as.POSIXct(as.Date("2020-03-01")), 
             color = "black", 
             lwd = 1,
             linetype = "dashed") +
  labs(x= "Date", y = "7 day rolling mean observed-predicted \U0394", 
       fill = "Time frame post 1st March 2020",  
       colour = "Time frame post 1st March 2020") +
  geom_label(aes(x = as.POSIXct(as.Date("2020-05-07")), y = 0, label = "UK National Lockdown"), 
             fill = "white",
             size = 5) + 
  annotate(geom = "text", label = as.character("1st March"),
           as.POSIXct(as.Date("2020-03-02")), y = c(-10, -10),
           angle = 90,
           size = 5)

ggplotly(urban_background_no2_cp_plot)
