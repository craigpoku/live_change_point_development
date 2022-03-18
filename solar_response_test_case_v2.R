library(patchwork)


#example case 1

solar_response_df = read.delim("~/live_change_point_development/solar_elevation_angle.txt") %>%
  rename(value = "X.3.841817855834960938e.00") %>%
  mutate(index = row_number())

solar_roll_regres = rollRegres::roll_regres(value ~ index, solar_response_df, width = 40,
                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  

solar_regres_coeff = as.data.frame(solar_roll_regres$r.squareds) %>%
  rename( r.squareds="solar_roll_regres$r.squareds") %>%
  mutate(index = solar_response_df$index)

plot(solar_response_df$index, solar_response_df$value, type = "l")
plot(solar_regres_coeff$r.squareds, type = "l")


solar_2nd_derivative = as.data.frame(pracma::gradient(solar_regres_coeff$grad)) %>%
  rename(change_point = "pracma::gradient(solar_regres_coeff$grad)") %>%
  mutate(index = solar_regres_coeff$index, change_point = round(abs(change_point), 4),
         rollmax = rollmax(change_point, k = 7, align = "right", fill = NA))%>%
  mutate(flag = rollmax-lag(rollmax) > 0 & rollmax == lead(rollmax, 4) & rollmax > 1e-5)

#Do we want a peak or a change from zero to positive? 
#rollmax-lag(rollmax) > 0 and sign(rollmax) == sign(lead(rollmax), 4)
max(solar_2nd_derivative$rollmax)
#round(abs(change_point), 7)

solar_response_plot = solar_response_df %>%
  ggplot(aes(index, value)) +
  geom_line(lwd = 1.5, colour = "red")+
  labs(x= "Scan line", y = "f(x)") +
  ggtitle("Test dataset")

solar_1st_derivative_plot = solar_regres_coeff %>%
  ggplot(aes(index, grad)) +
  geom_line(lwd = 1.5, colour = "green")+
  labs(x= "Scan line", y = "f'(x)") +
  ggtitle("Rolling linear regression - window length = 40")

solar_2nd_derivative_plot = solar_2nd_derivative %>%
  ggplot(aes(index, rollmax)) +
  geom_line(lwd = 1.5, colour = "blue")+
  labs(x= "Scan line", y = "f''(x)") +
  ggtitle("2nd derivative")

plotly::ggplotly(solar_2nd_derivative_plot)

solar_response_plot/solar_1st_derivative_plot/solar_2nd_derivative_plot


solar_regres_cp_plot = solar_2nd_derivative %>%
  mutate(d = change_point - lag(change_point),
         lag_d = lag(d),
         flag = sign(d) != sign(lag_d))

plot(ksmooth(solar_regres_cp_plot$index,solar_regres_cp_plot$d,
             'normal',bandwidth=15),type='l',col='blue')

solar_regres_cp_kernel = as.data.frame(ksmooth(solar_2nd_derivative$index,
                                               solar_2nd_derivative$rollmax,
                                               'normal',bandwidth=10)) %>%
  rename(index = x, d = y) %>%
  mutate(flag = sign(d) != sign(lag(d))) %>%
  filter(flag == TRUE)


solar_regres_cp %>%
  ggplot(aes(index, d)) +
  geom_line(lwd = 1.5, colour = "blue") +
  ggtitle("2nd derivative example")  


solar_plot_proposed = solar_response_df %>%
  mutate(changepoint = index == solar_2nd_derivative$index & solar_2nd_derivative$flag==TRUE) %>%
  drop_na()

solar_plot_proposed_plot = solar_plot_proposed %>%
  ggplot(aes(index, value)) +
  geom_line(lwd = 1.5, colour = "red")+
  geom_point(data = filter(solar_plot_proposed, changepoint==TRUE), size  = 3, colour = "blue") +
  labs(x= "Scan line", y = "Solar elevation angle [deg]") +
  ggtitle("Solar elevation angle - gradient change")

plotly::ggplotly(solar_plot_proposed_plot)

