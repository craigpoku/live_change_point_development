library(dplyr)
library(tidyverse)
library(ggplot2)
library(rollRegres)
library(zoo)
library(patchwork)
library(pracma)
library(changepoint)

solar_response_df = read.delim("~/R_coding_example/solar_elevation_angle.txt") %>%
  rename(value = "X.3.841817855834960938e.00") %>%
  mutate(index = row_number())

solar_response_sliced_df = solar_response_df %>%
  filter(between(index, 1200,2000)) %>%
  select(value) %>%
  mutate(index = row_number())

df_prepared = unlist(solar_response_sliced_df %>%
                       select(any_of("value"))) %>%
  cpt.meanvar()
cp_df_values = df_prepared@cpts

solar_response_cp_meanvar = as.vector(df_prepared@cpts)

filter(solar_response_sliced_df, index %in% solar_response_cp_meanvar)
aes(xintercept = index)

solar_response_cp_example = df_cp_detection(solar_response_df, 40, 5, FALSE)

solar_plot_meanvar = solar_response_sliced_df %>%
  ggplot(aes(index, value)) +
  geom_line(lwd = 1.5, colour = "red")+
  geom_vline(data = filter(solar_response_sliced_df, index %in% solar_response_cp_meanvar),
             aes(xintercept = index)) +
  labs(x= "Scan line", y = "Solar elevation angle [deg]") +
  ggtitle("Solar elevation angle - changepoint meanvar package")

solar_plot_proposed = solar_response_df %>%
  ggplot(aes(index, value)) +
  geom_line(lwd = 1.5, colour = "red")+
  geom_vline(data = filter(solar_response_cp_example, d > 4.5e-7, flag),
             aes(xintercept = index)) +
  labs(x= "Scan line", y = "Solar elevation angle [deg]") +
  ggtitle("Solar elevation angle - gradient change")

solar_plot_proposed = solar_response_sliced_df %>%
  ggplot(aes(index, value)) +
  geom_line(lwd = 1.5, colour = "red")+
  geom_vline(data = filter(solar_response_cp_example, d > 4.5e-7, flag),
             aes(xintercept = index)) +
  labs(x= "Scan line", y = "Solar elevation angle [deg]") +
  ggtitle("Solar elevation angle - gradient change")

solar_plot_meanvar/solar_plot_proposed



solar_cp_proposed = solar_response_sliced_df %>%
  ggplot(aes(index, value)) +
  geom_line(lwd = 1.5, colour = "red")+
  geom_vline(data = filter(solar_response_cp, d > 4.5e-7, flag),
             aes(xintercept = index)) +
  labs(x= "Scan line", y = "Solar elevation angle [deg]") +
  ggtitle("Solar elevation angle - CPD example")

solar_response_sliced_df %>%
  ggplot(aes(index, value)) +
  geom_line(lwd = 1.5, colour = "red")+
  geom_vline(data = solar_response_cp_meanvar,
             aes(xintercept = index)) +
  labs(x= "Scan line", y = "Solar elevation angle [deg]") +
  ggtitle("Solar elevation angle - CPD example")



solar_response_cp %>%
  ggplot(aes(x=d)) + 
  geom_density() +
  xlim(-8e-7, 8e-7)




  


  

