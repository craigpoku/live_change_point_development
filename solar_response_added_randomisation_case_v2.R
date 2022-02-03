library(patchwork)
library(rpatrec)


#example case 1

solar_response_df = read.delim("~/live_change_point_development/solar_elevation_angle.txt") %>%
  rename(value = "X.3.841817855834960938e.00") %>%
  mutate(index = row_number())


#set.seed(001) 
solar_randomised_response_df = solar_response_df %>%
  filter(index %in% (500:1500)) %>%
  mutate(sample = sample(value),
         rollmean = roll_mean(sample, n=200, align = "right", fill = NA)) %>%
  drop_na()


ksmooth = ksmooth(solar_randomised_response_df$index, 
                  solar_randomised_response_df$sample, "normal", bandwidth = 20)

plot(ksmooth)
solar_randomised_response_smooth_df = cbind(solar_randomised_response_df, ksmooth) %>%
  select(-x) %>%
  rename(ksmooth = y)

solar_random_roll_regres = rollRegres::roll_regres(ksmooth ~ index, solar_randomised_response_smooth_df, 
                                                  width = 10,
                                         do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  

solar_random_regres_coeff = as.data.frame(solar_random_roll_regres$coefs) %>%
  rename(grad = index) %>%
  mutate(index = solar_randomised_response_smooth_df$index, grad = abs(grad)) %>%
  mutate(rollmax = rollmax(grad, k = 3, align = "right", fill = NA))


solar_random_2nd_derivative = as.data.frame(pracma::gradient(solar_random_regres_coeff$grad)) %>%
  rename(change_point = "pracma::gradient(solar_random_regres_coeff$grad)") %>%
  mutate(index = solar_random_regres_coeff$index) %>%
  mutate(rollmax = rollmax(change_point, k = 3, align = "right", fill = NA))


#Do we want a peak or a change from zero to positive? 
#rollmax-lag(rollmax) > 0 and sign(rollmax) == sign(lead(rollmax), 4)
#round(abs(change_point), 7)

solar_random_response_plot = solar_randomised_response_smooth_df %>%
  ggplot(aes(index, ksmooth)) +
  geom_line(lwd = 1.2, colour = "green") +
  ggtitle("Solar df example")

solar_random_1st_derivative_plot = solar_random_regres_coeff %>%
  ggplot(aes(index, rollmax)) +
  geom_line(lwd = 1.5, colour = "red") +
  ggtitle("1st derivative example")

solar_random_2nd_derivative_plot = solar_random_2nd_derivative %>%
  ggplot(aes(index, rollmax)) +
  geom_line(lwd = 1, colour = "blue") +
  ggtitle("2nd derivative example")

plotly::ggplotly(solar_random_1st_derivative_plot)

solar_random_response_plot/solar_random_1st_derivative_plot/solar_random_2nd_derivative_plot


solar_random_cpd_index = solar_random_regres_coeff %>%
  mutate(flag = rollmax-lag(rollmax) > 0 & rollmax == lead(rollmax, 2))

solar_random_plot_proposed = solar_randomised_response_smooth_df %>%
  mutate(changepoint = index == solar_random_cpd_index$index & solar_random_cpd_index$flag==TRUE) %>%
  drop_na()

solar_random_plot_proposed_plot = solar_randomised_response_smooth_df %>%
  ggplot(aes(index, ksmooth)) +
  geom_line(lwd = 1, colour = "red")+
  geom_point(data = filter(solar_random_plot_proposed, changepoint==TRUE), size  = 3, colour = "blue") +
  labs(x= "Scan line", y = "Solar elevation angle [deg]") +
  ggtitle("Solar elevation angle - gradient change")

plotly::ggplotly(solar_random_plot_proposed_plot)

