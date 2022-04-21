x = c(0, 300, 600, 900, 1200, 1500)
f_x = c(-5, 8, 1, 2, 3, 6)
g_x = c(-5, 8, 1, 2, 3, 5)
h_x = c(-5, 8, 1, 2, 3, 4.5)


plot(x, f_x, type = "l")
plot(x, h_x, type = "l")

linear_function_generator = function(index, df, seq, epsilon){
  f = approxfun(x = index, y = df)
  seq_range_x = c(1:seq) 
  linear_fit = f(seq_range_x)
  
  f_x_df = data.frame(seq_range_x, linear_fit) %>%
    rename(index = seq_range_x, value = linear_fit) %>%
    mutate(value = jitter(value, factor=epsilon, amount = NULL))%>%
    drop_na()
  
  return(f_x_df)
}

tail(x, n=1)

fun_1_df = linear_function_generator(x, f_x, tail(x, n=1), 0.01)
fun_2_df = linear_function_generator(x, h_x, tail(x, n=1), 0.01)

plot(fun_2_df, type = "l")

fun_1_cp_detection = map_dfr(.x = c(40, 80, 120),
                                     .f = ~window_length_constrain(df = fun_1_df,
                                                                   .x, k = 8,
                                                                   rollmax_width = 100))

fun_2_cp_detection = map_dfr(.x = c(40, 80, 120),
                             .f = ~window_length_constrain(df = fun_2_df,
                                                           .x, k = 8,
                                                           rollmax_width = 100))

fun_1_cp_detection %>%
  filter(variables != "r.squareds") %>%
  ggplot(aes(x = index, y = value))+
  geom_line(aes(colour = variables), lwd = 1.5)+
  geom_point(data = filter(fun_1_cp_detection,
                           cp==TRUE & variables == "Test dataset")
             , size  = 3, colour = "blue")+
  labs(x= "Date", y = "Various units") +
  facet_grid(vars(variables), vars(window_length_level), scales = "free_y") +
  theme_bw(base_size = 15)

fun_2_cp_detection %>%
  filter(variables != "r.squareds") %>%
  ggplot(aes(x = index, y = value))+
  geom_line(aes(colour = variables), lwd = 1.5)+
  geom_point(data = filter(fun_2_cp_detection,
                           cp==TRUE & variables == "Test dataset")
             , size  = 3, colour = "blue")+
  labs(x= "Date", y = "Various units") +
  facet_grid(vars(variables), vars(window_length_level), scales = "free_y") +
  theme_bw(base_size = 15)


roll_regression_test = rollRegres::roll_regres(value ~ "index", fun_1_df, 
                                          width = 40)  


cp_function_1 = window_length_constrain(fun_1_df, 40, 2) 

cp_function_1 %>%
  ggplot(aes(x = date, y = value))+
  geom_line(aes(colour = variables), lwd = 1.5) +
  facet_wrap(~variables, scales = "free_y") +
  theme_bw(base_size = 15)

fun_1_df %>%
  ggplot(aes(x = date, y = value))+
  geom_line(colour = "red", lwd = 1.5)+
  geom_point(data = filter(cp_function_1,
                           cp==TRUE & variables == "Test dataset")
             , size  = 3, colour = "blue") +
  theme_bw(base_size = 15)
