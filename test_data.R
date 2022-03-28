x = c(0, 300, 600, 900, 1200, 3000)
f_x = c(-5, 8, 5, 2, 3, 6)
g_x = c(-5, 8, 6, 2, 3, 6)
h_x = c(-5, 8, 7, 2, 3, 6)
plot(x, h_x)

function_1 = approxfun(x = x, y = f_x)
function_2 = approxfun(x = x, y = g_x)
function_3 = approxfun(x = x, y = h_x)


seq_x = c(1:3000)
linear_fit_function_1 = function_1(seq_x)
linear_fit_function_2 = function_2(seq_x)
linear_fit_function_3 = function_3(seq_x)

fun_1_df = data.frame(seq_x, linear_fit_function_1) %>%
  rename(index = seq_x, value = linear_fit_function_1) 

plot(fun_1_df)
plot(solar_response_df)


roll_regression_test = rollRegres::roll_regres(value ~ index, fun_1_df, 
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
