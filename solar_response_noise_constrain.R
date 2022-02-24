library(patchwork)
library(rpatrec)


#example case 1

solar_response_df = read.delim("~/R_coding_example/solar_elevation_angle.txt") %>%
  rename(value = "X.3.841817855834960938e.00") %>%
  mutate(index = row_number())

window_length = c(35, 140, 280, 700) #1.25, 5, 10, 25% respectively
percentage_values = c("1.25%", "5%", "10%", "25%")
sd = c(0, 2, 5, 10)


window_length_constrain_breakdown = function(df, window_length_vector, percentage_length_vector,
                                             standard_deviation){
  
  if(standard_deviation == 0){
    
    noise_df = df %>%
      mutate(noise = value)
  }
  
  else{
    noise_df = df %>%
      mutate(noise = value + standard_deviation*runif(nrow(df)))
  }
  
  
  noise_roll_regression = rollRegres::roll_regres(noise ~ index, noise_df, 
                                                  width = window_length_vector,
                                                  do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
  
  noise_roll_reformat_cp = as.data.frame(noise_roll_regression$coefs) %>%
    rename(grad = index) %>%
    mutate(index = df$index,
           data = noise_df$noise,
           window_length_level = as.factor(window_length_vector),
           percentage_level = as.factor(percentage_length_vector),
           derv_2nd = round(abs(pracma::gradient(grad)), 4),
           rollmax = rollmax(derv_2nd, k = 7, align = "right", fill = NA),
           cp = rollmax-lag(rollmax) > 0 & rollmax == lead(rollmax, 4) & rollmax > 1e-5
    ) %>%rename("Test dataset" = data,
                     "Rolling gradient" = grad,
                     "2nd derivative" = derv_2nd,
                    "Applied rollmax" = rollmax)%>%
    select(-"(Intercept)") %>%
    drop_na() %>%
    pivot_longer(-c(index, window_length_level, percentage_level, cp), names_to = "variables")%>%
    mutate(variables = factor(variables, 
                              levels = c("Test dataset", "Rolling gradient", "2nd derivative",
                                         "Applied rollmax")))

  return(noise_roll_reformat_cp)
}


solar_white_noise_sd_0 = map2_dfr(.x = window_length, .y = percentage_values,
                                  .f = ~window_length_constrain_breakdown(df = solar_response_df,
                                                                          .x, .y, 0))

solar_white_noise_sd_2 = map2_dfr(.x = window_length, .y = percentage_values,
                                  .f = ~window_length_constrain_breakdown(df = solar_response_df,
                                                                          .x, .y, 2))

solar_white_noise_sd_5 = map2_dfr(.x = window_length, .y = percentage_values,
                                  .f = ~window_length_constrain_breakdown(df = solar_response_df,
                                                                          .x, .y, 5))


test = solar_white_noise_sd_2 %>%
  filter(value == 1)


n_labels = solar_white_noise_sd_0 %>%
  filter(variables == "Test dataset") %>%
  mutate(max_ind = max(index),
         min_ind = min(index),
         max_val = max(value),
         min_val = min(value)) %>%
  group_by(percentage_level, variables) %>%
  summarise(n_cp = sum(cp),
            across(max_ind:min_val, min)) %>%
  mutate(y = min_val + (max_val - min_val)*.8,
         x = min_ind + (max_ind - min_ind)*.2,
         n_cp = glue::glue("n = {n_cp}"),
         .keep = "unused")

plt1 = solar_white_noise_sd_0 %>%
  filter(variables == "Test dataset", percentage_level == "1.25%") %>%
  ggplot(aes(x = index, y = value))+
  geom_line(colour = "red", lwd = 1.2)+
  geom_point(data = ~filter(.x, variables == "Test dataset" & cp==TRUE & percentage_level == "1.25%")
             , colour = "blue", size = 5)+
  geom_label(data = ~filter(n_labels, percentage_level == "1.25%"), 
             aes(x = x, y = y, label = n_cp), color = "black", size = 13)+
  labs(x= "Scan line", y = "Solar elevation angle [deg]") +
  facet_grid(variables~percentage_level, 
             scales = "free_y")

ggsave(filename = "~/myploy", plot = plt1, )

solar_white_noise_sd_2 %>%
  filter(variables == "cp", value == 1)

solar_noise_df = map_dfr(.x = sd,
                         .f = ~apply_white_noise(df = solar_response_df,
                                                 .x))

solar_noise_df %>%
  ggplot(aes(x = index, y = noise))+
  geom_line(colour = "red", lwd = 1.1)+
  facet_wrap(~standard_deviation, scales = "free_y")+
  labs(x= "Scan line", y = "Solar elevation angle [deg]", 
       colour = "Model variables") +
  ggtitle("Test data with applied noise (standard deviation)")





solar_white_noise_stats_sd_2 = map2_dfr(.x = window_length, .y = percentage_values,
                                        .f = ~window_length_constrain_stats(df = solar_response_df,
                                                                            .x, .y, 2))

solar_white_noise_stats_sd_5 = map2_dfr(.x = window_length, .y = percentage_values,
                                        .f = ~window_length_constrain_stats(df = solar_response_df,
                                                                            .x, .y, 5))

solar_white_noise_stats_sd_10 = map2_dfr(.x = window_length, .y = percentage_values,
                                         .f = ~window_length_constrain_stats(df = solar_response_df,
                                                                             .x, .y, 10))




solar_white_noise_sd_2 %>%
  filter(variables != "2nd derivative") %>%
  ggplot(aes(x = index, y = value))+
  geom_line(aes(color = variables), lwd = 1.2)+
  facet_grid(variables~percentage_level, scales = "free_y")+
  labs(x= "Scan line", y = "Various Units", 
       colour = "Model variables") +
  ggtitle("White noise with SD = 2.0")

solar_white_noise_sd_5 %>%
  filter(variables != "2nd derivative") %>%
  ggplot(aes(x = index, y = value))+
  geom_line(aes(color = variables), lwd = 1.2)+
  facet_grid(variables~percentage_level, scales = "free_y")+
  labs(x= "Scan line", y = "Various Units", 
       colour = "Model variables") +
  ggtitle("White noise with SD = 5.0")


solar_white_noise_stats_sd_5 %>%
  ggplot(aes(x = index, y = value))+
  geom_line(aes(color = variables), lwd = 1.2)+
  facet_wrap(~variables+percentage_level, scales = "free_y", ncol = length(window_length))+
  labs(x= "Scan line", y = "Various Units", 
       colour = "Model variables") +
  ggtitle("White noise with SD = 5.0")

solar_white_noise_sd_10%>%
  filter(percentage_level != "1.25%") %>%
  ggplot(aes(x = index, y = value))+
  geom_line(aes(color = variables), lwd = 1.2)+
  facet_wrap(~variables+percentage_level, scales = "free_y", ncol = (length(window_length)-1))+
  labs(x= "Scan line", y = "Various Units", 
       colour = "Model variables") +
  ggtitle("White noise with SD = 10.0")


