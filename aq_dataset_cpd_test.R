library(openair)
library(dplyr)
library(rmweather)
library(zoo)
require(RcppRoll)
library(ggplot2)
library(plotly)
library(tidyr)
library(purrr)
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

#Note, ULEZ was initially implemented 8th April 2019, ran code a month prior to implementation

BAU_urban_background_no2_london = map(.x = London_code_no2_urban_background,
                                       .f = ~rmweather_BAU_observed(df = met_aq_london_urban_background_no2,
                                                          site = .x, 300, "2016-01-01", 
                                                          "2019-03-15", "2020-07-31",
                                                          0.85, 300))

BAU_urban_background_no2_london_time_2 = map(.x = London_code_no2_urban_background,
                                      .f = ~rmweather_BAU_observed(df = met_aq_london_urban_background_no2,
                                       site = .x, 300, "2016-07-01", 
                                      "2019-07-15", "2019-12-31",
                                      0.85, 300))

london_urban_background_no2_reformat = urban_reformat_data_mean_sd(BAU_urban_background_no2_london,
                                       London_code_no2_urban_background)

london_urban_background_no2_reformat_time_2 = urban_reformat_data_mean_sd(BAU_urban_background_no2_london_time_2,
                                              London_code_no2_urban_background)

london_urban_statistics = urban_model_statistics(BAU_urban_background_no2_london,
                                                 London_code_no2_urban_background)

london_urban_statistics_time_2 = urban_model_statistics(BAU_urban_background_no2_london_time_2,
                                                        London_code_no2_urban_background)

avgs_london_urban_background_no2 = london_urban_statistics %>% 
  group_by(stat, Data) %>% 
  summarise(avg = mean(value, na.rm = T), sd = sd(value, na.rm = T)) %>%
  ungroup()%>%
  mutate(lower_sd = avg-sd, upper_sd = avg+sd,
         lower_2_sd = avg-2*sd, upper_2_sd = avg+2*sd)

avgs_london_urban_background_no2_time_2 = london_urban_statistics_time_2 %>% 
  group_by(stat, Data) %>% 
  summarise(avg = mean(value, na.rm = T), sd = sd(value, na.rm = T)) %>%
  ungroup()%>%
  mutate(lower_sd = avg-sd, upper_sd = avg+sd,
         lower_2_sd = avg-2*sd, upper_2_sd = avg+2*sd)



#graphical outputs - visualisation

stats_plot = london_urban_statistics  %>%
  ggplot(aes(y = sites, x = value)) +
  geom_point(aes(color = Data), size = 4) +
  geom_vline(data = avgs_london_urban_background_no2, aes(xintercept = avg, color = Data), lty = 2,
             lwd = 1.2) +
  geom_rect(data = avgs_london_urban_background_no2, aes(xmin = lower_sd, xmax = upper_sd, ymin = -Inf, 
                                                  ymax = Inf, fill = Data),
            alpha = 0.2, inherit.aes = FALSE) + 
  labs(x= "Various Units", y = "Sites")+
  theme_bw(base_size = 15) +
  theme(legend.position = "top")+
  facet_grid(~stat, scales = "free_x") + theme(panel.spacing = unit(2, "lines"))+ 
  ggtitle("Model statistics: 68% confidence level (London Urban Background)")


stats_plot_time_2 = london_urban_statistics_time_2  %>%
  ggplot(aes(y = sites, x = value)) +
  geom_point(aes(color = Data), size = 4) +
  geom_vline(data = avgs_london_urban_background_no2_time_2, aes(xintercept = avg, color = Data), lty = 2,
             lwd = 1.2) +
  geom_rect(data = avgs_london_urban_background_no2_time_2, aes(xmin = lower_sd, xmax = upper_sd, ymin = -Inf, 
                                                         ymax = Inf, fill = Data),
            alpha = 0.2, inherit.aes = FALSE) + 
  labs(x= "Various Units", y = "Sites")+
  theme_bw(base_size = 15) +
  theme(legend.position = "top")+
  facet_grid(~stat, scales = "free_x") + theme(panel.spacing = unit(2, "lines"))+ 
  ggtitle("Model statistics: 68% confidence level (London Urban Background, timeframe 2)")

urban_london_rm_mean = london_urban_background_no2_reformat %>%
  select(date,d7_rollavg_rm_normal_mean)%>%
  rename(value=d7_rollavg_rm_normal_mean)%>%
  drop_na()

urban_london_rm_mean_time_2 = london_urban_background_no2_reformat_time_2 %>%
  select(date,d7_rollavg_rm_normal_mean)%>%
  rename(value=d7_rollavg_rm_normal_mean)%>%
  drop_na()

urban_london_delta_mean = london_urban_background_no2_reformat %>%
  select(date, d7_rollavg_delta_BAU_predict_mean)%>%
  rename(value=d7_rollavg_delta_BAU_predict_mean) %>%
  drop_na()

urban_london_delta_mean_time_2 = london_urban_background_no2_reformat_time_2 %>%
  select(date, d7_rollavg_delta_BAU_predict_mean)%>%
  rename(value=d7_rollavg_delta_BAU_predict_mean) %>%
  drop_na()

urban_london_rm_cp = df_cp_detection(urban_london_rm_mean, 14, 14, TRUE)
urban_london_delta_cp = df_cp_detection(urban_london_delta_mean, 14, 14, TRUE)

london_cp_df = rbind(
  urban_london_rm_cp %>% mutate(delta = "rmweather"),
  urban_london_delta_cp %>% mutate(delta = "BAU - observed (delta)")
)

urban_london_rm_cp_time_2 = df_cp_detection(urban_london_rm_mean_time_2, 14, 14, TRUE)
urban_london_delta_cp_time_2 = df_cp_detection(urban_london_delta_mean_time_2, 14, 14, TRUE)

london_cp_df_time_2 = rbind(
  urban_london_rm_cp_time_2 %>% mutate(delta = "rmweather"),
  urban_london_delta_cp_time_2 %>% mutate(delta = "BAU - observed (delta)")
)

delta_comparison_london_no2 = london_urban_background_no2_reformat %>%
  select(date, d7_rollavg_delta_BAU_predict_mean, d7_rollavg_rm_normal_mean) %>%
  drop_na()%>%
  rename("BAU - observed (delta)"=d7_rollavg_delta_BAU_predict_mean,
         "rmweather"=d7_rollavg_rm_normal_mean) %>%
  pivot_longer(-date, names_to = "delta")

delta_comparison_london_no2_time_2 = london_urban_background_no2_reformat_time_2 %>%
  select(date, d7_rollavg_delta_BAU_predict_mean, d7_rollavg_rm_normal_mean) %>%
  drop_na()%>%
  rename("BAU - observed (delta)"=d7_rollavg_delta_BAU_predict_mean,
         "rmweather"=d7_rollavg_rm_normal_mean) %>%
  pivot_longer(-date, names_to = "delta")

test_plot = delta_comparison_london_no2 %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30")) %>% 
  ggplot(aes(x = date, y = value))+
  annotate("rect", xmin = as.POSIXct(as.Date("2019-04-08")), 
           xmax = as.POSIXct(as.Date("2019-06-30")), ymin = -Inf, ymax = Inf, 
           alpha = .2)+
  geom_vline(data = filter(london_cp_df, flag,date >= as.Date("2019-03-15") 
                           & date <= as.Date("2019-06-30")),
             aes(xintercept = date, colour = delta)) + 
  geom_line(aes(colour = delta), lwd = 1.5) +
  facet_grid(delta~., scales = "free_y")+
  labs(x= "Date", y = "Various Units", colour = "Human impact comparison")+ 
  geom_vline(xintercept = as.POSIXct(as.Date("2019-03-15")), 
             color = "black", 
             lwd = 1,
             linetype = "dashed")+
  theme_bw(base_size = 20)

test_plot_time_2 = delta_comparison_london_no2_time_2 %>%
  filter(date >= as.Date("2019-05-01") & date <= as.Date("2019-12-30")) %>% 
  ggplot(aes(x = date, y = value))+
  geom_vline(data = filter(london_cp_df_time_2, flag,date >= as.Date("2019-07-15") 
                           & date <= as.Date("2019-12-30")),
             aes(xintercept = date, colour = delta)) + 
  geom_line(aes(colour = delta), lwd = 1.5) +
  facet_grid(delta~., scales = "free_y")+
  labs(x= "Date", y = "Various Units", colour = "Human impact comparison")+ 
  geom_vline(xintercept = as.POSIXct(as.Date("2019-07-17")), 
             color = "black", 
             lwd = 1,
             linetype = "dashed")+
  theme_bw(base_size = 20)

test_plot
stats_plot

ggplotly(test_plot)





