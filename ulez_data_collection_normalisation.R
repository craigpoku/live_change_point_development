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

met_aq_london_roadside_no2=met_aq_prepared_rm_vs_de(met_london_df_all, 
                                                   "no2", "Roadside", TRUE)

met_aq_london_roadside_pm25=met_aq_prepared_rm_vs_de(met_london_df_all, 
                                                    "pm25", "Roadside", TRUE)

London_code_no2_urban_background = unique(as.character(met_aq_london_urban_background_no2$code))

London_code_no2_roadside = unique(as.character(met_aq_london_roadside_no2$code))

London_code_pm25_roadside = unique(as.character(met_aq_london_roadside_pm25$code))

ULEZ_no2_urban_background_sites = c("BL0", "CT3", "KC1", "WM0")
ULEZ_no2_urban_roadside_sites = c("CD9", "CT4", "CT6", "NB1") 
ULEZ_pm25_urban_roadside_sites = c("CD9") 


#Note, ULEZ was initially implemented 8th April 2019, ran code a month prior to implementation


normalised_urban_background_no2_london = map(.x = London_code_no2_urban_background,
                                            .f = ~rmweather_normalised_observed(df = met_aq_london_urban_background_no2,
                                                            site = .x, 300, "2016-01-01", "2019-12-31", 0.85, 300))

normalised_roadside_no2_london = map(.x = London_code_no2_roadside,
                                             .f = ~rmweather_normalised_observed(df = met_aq_london_roadside_no2,
                                                            site = .x, 300, "2016-01-01", "2019-12-31", 0.85, 300))

normalised_roadside_pm25_london = map(.x = London_code_pm25_roadside,
                                     .f = ~rmweather_normalised_observed(df = met_aq_london_roadside_pm25,
                                                                         site = .x, 300, "2016-01-01", "2019-12-31", 0.85, 300))



normalised_urban_background_no2_ULEZ_reformat = 
  urban_reformat_data_mean_sd_no_normal(normalised_urban_background_no2_london,
                                       London_code_no2_urban_background, ULEZ_no2_urban_background_sites,
                                       normal=TRUE, ULEZ=TRUE)

normalised_urban_background_no2_all_sites_reformat = 
  urban_reformat_data_mean_sd_no_normal(normalised_urban_background_no2_london,
                                        London_code_no2_urban_background, ULEZ_no2_urban_background_sites,
                                        normal=TRUE, ULEZ=FALSE)

normalised_roadside_no2_ULEZ_reformat = 
  urban_reformat_data_mean_sd_no_normal(normalised_roadside_no2_london,
                                        London_code_no2_roadside, ULEZ_no2_urban_roadside_sites,
                                        normal=TRUE, ULEZ=TRUE)

normalised_roadside_no2_all_sites_reformat = 
  urban_reformat_data_mean_sd_no_normal(normalised_roadside_no2_london,
                                        London_code_no2_roadside, ULEZ_no2_urban_roadside_sites,
                                        normal=TRUE, ULEZ=FALSE)

normalised_roadside_pm25_ULEZ_reformat = 
  urban_reformat_data_mean_sd_no_normal(normalised_roadside_pm25_london,
                                        London_code_pm25_roadside, ULEZ_pm25_urban_roadside_sites,
                                        normal=TRUE, ULEZ=TRUE)

normalised_roadside_pm25_all_sites_reformat = 
  urban_reformat_data_mean_sd_no_normal(normalised_roadside_pm25_london,
                                        London_code_pm25_roadside, ULEZ_pm25_urban_roadside_sites,
                                        normal=TRUE, ULEZ=FALSE)


normalised_urban_background_no2_all_sites_reformat %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-06-30")) %>% 
  ggplot(aes(x = date, y = d7_rollavg_normal_mean))+
  annotate("rect", xmin = as.POSIXct(as.Date("2019-04-08")), 
           xmax = as.POSIXct(as.Date("2019-06-30")), ymin = -Inf, ymax = Inf, 
           alpha = .2) + 
  geom_line(colour="red", lwd = 1.5) +
  facet_grid(delta~., scales = "free_y")+
  labs(x= "Date", y = "Various Units", colour = "Human impact comparison")+ 
  geom_vline(xintercept = as.POSIXct(as.Date("2019-03-15")), 
             color = "black", 
             lwd = 1,
             linetype = "dashed")+
  theme_bw(base_size = 20)





