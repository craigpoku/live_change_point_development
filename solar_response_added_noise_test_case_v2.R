library(patchwork)
library(rpatrec)


#example case 1

solar_response_df = read.delim("~/R_coding_example/solar_elevation_angle.txt") %>%
  rename(value = "X.3.841817855834960938e.00") %>%
  mutate(index = row_number())

solar_noise_response_df = solar_response_df %>%
  mutate(noise = noise(value,'white',5),
         rollmean = zoo::rollmean(noise, k = 40, align = "right", fill = NA)) %>%
  drop_na()

plot(solar_noise_response_df$rollmean, type = "l")

solar_noise_roll_regres = rollRegres::roll_regres(noise ~ index, solar_noise_response_df, width = 40,
                                         do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  

solar_noise_regres_coeff = as.data.frame(solar_noise_roll_regres$coefs) %>%
  rename(grad = index) %>%
  mutate(index = solar_noise_response_df$index)


solar_noise_2nd_derivative = as.data.frame(pracma::gradient(solar_noise_regres_coeff$grad)) %>%
  rename(change_point = "pracma::gradient(solar_noise_regres_coeff$grad)") %>%
  mutate(index = solar_noise_regres_coeff$index) %>%
  mutate(change_point = round(abs(change_point), 4),
         rollmax = rollmax(change_point, k = 40, align = "right", fill = NA),
         flag = rollmax-lag(rollmax) > 0 & rollmax == lead(rollmax, 4) & rollmax > 0.001)

#Do we want a peak or a change from zero to positive? 
#rollmax-lag(rollmax) > 0 and sign(rollmax) == sign(lead(rollmax), 4)
max(solar_noise_2nd_derivative$rollmax)
#round(abs(change_point), 7)

solar_noise_response_plot = solar_noise_response_df %>%
  ggplot(aes(index, rollmean)) +
  geom_line(lwd = 1.5, colour = "green") +
  ggtitle("Solar df example")

solar_noise_1st_derivative_plot = solar_noise_regres_coeff %>%
  ggplot(aes(index, grad)) +
  geom_line(lwd = 1.5, colour = "red") +
  ggtitle("1st derivative example")

solar_noise_2nd_derivative_plot = solar_noise_2nd_derivative %>%
  ggplot(aes(index, change_point)) +
  geom_line(lwd = 1, colour = "blue", alpha = 0.6) +
  ggtitle("2nd derivative example")

plotly::ggplotly(solar_noise_2nd_derivative_plot)

solar_noise_response_plot/solar_noise_1st_derivative_plot/solar_noise_2nd_derivative_plot



solar_noise_plot_proposed = solar_noise_response_df %>%
  mutate(changepoint = index == solar_noise_2nd_derivative$index & solar_noise_2nd_derivative$flag==TRUE) %>%
  drop_na()

solar_noise_plot_proposed_plot = solar_noise_plot_proposed %>%
  ggplot(aes(index, noise)) +
  geom_line(lwd = 1.5, colour = "red")+
  geom_point(data = filter(solar_noise_plot_proposed, changepoint==TRUE), size  = 3, colour = "blue") +
  labs(x= "Scan line", y = "Solar elevation angle [deg]") +
  ggtitle("Solar elevation angle - gradient change")

plotly::ggplotly(solar_noise_plot_proposed_plot)

