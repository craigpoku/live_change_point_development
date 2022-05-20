#processing functions

#packages, could be tidied? 

library(patchwork)
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
library(rpatrec)
library(lubridate)
library(Metrics)

#-------------------------------- Creates dataset that combines AQ and met data -----------------------


read_met_sites = function(site_code, metcode, begin_date, end_date){
  
  met_UK_df = read.csv(paste(directory_met_data, metcode,".csv")) %>%
    mutate(date = lubridate::ymd_hms(date)) %>%
    filter(date >= as.Date(begin_date) & date <= as.Date(end_date)) %>%
    rename(met_code = code)
  
  begin_year = as.numeric(substring(begin_date,1,4))
  end_year = as.numeric(substring(end_date,1,4))
  
  UK_raw_df = importAURN(site = site_code, year = begin_year:end_year, meta = TRUE)
  
  if(!is.null(UK_raw_df)){
    met_UK_df = UK_raw_df %>% 
      select(-any_of(c("ws", "wd", "air_temp", "latitude", "longitude"))) %>% 
      left_join(met_UK_df, ., by = "date")
    print(paste0(site_code, "/", metcode, " finished."))
  } else {
    print(paste0(site_code, "/", metcode, " is missing."))
  }
  
  return(tibble::tibble(met_UK_df))
  
}


#-------------------------------- Prepares dataset to be suitable for normalisation ----------------------

met_aq_prepared_rm_vs_de = function(df, pollutant_type, site_type_category, rmweather = FALSE){
  
  df_de = df %>%
    select(code, met_code, station, date, all_of(pollutant_type), 
           ws, wd, air_temp, atmos_pres, RH, cl) %>%
    filter(!is.na((get(pollutant_type))))
  
  df_rm = df %>%
    select(code, site_type, met_code, station, date, all_of(pollutant_type), 
           ws, wd, air_temp, atmos_pres, RH, cl)%>%
    filter(site_type == site_type_category) %>%
    pivot_longer(-c(code, site_type, met_code, station, date, ws, wd, air_temp, atmos_pres, RH, cl),
                 names_to = "pollutant") %>%
    select(-site_type) %>%
    rename(atmospheric_pressure = atmos_pres) %>%
    filter(!is.na(value))
  
  if(rmweather ==TRUE){
    return(tibble::tibble(df_rm))
  } else {
    return(tibble::tibble(df_de))
  }
}



#---------------------------Creates df that contains observed vs BAU data using rmweather--------------
#also contains relevant statistics e.g. MSE, rsquared coefficient for linear regression

rmweather_BAU_observed = function(df, site, n_tree, begin_date_train, end_date_train, 
                                  end_date_predict, data_threshold, n_sample){
  
  print(paste(site, "rm process beginning"))
  
  begin_year = as.numeric(substring(begin_date_train,1,4))
  end_year = as.numeric(substring(end_date_train,1,4))
  
  begin_month = as.numeric(substring(begin_date_train,6,7))
  end_month = as.numeric(substring(end_date_train,6,7))
  
  begin_day = as.numeric(substring(begin_date_train,9,10))
  end_day = as.numeric(substring(end_date_train,9,10))  
  
  diff_hours = c(ISOdate(end_year,end_month,end_day), ISOdate(begin_year,begin_month,begin_day))
  
  diff_hours_quality_check = difftime(diff_hours[1], diff_hours[2], units="hours")+1
  
  df_quality_check = df %>% 
    filter(date >= as.Date(begin_date_train), code == site)
  
  total_date_df = length(df_quality_check$date)
  print(length(df_quality_check$date)/as.numeric(diff_hours_quality_check))
  
  if(length(df_quality_check$date)/as.numeric(diff_hours_quality_check) >= data_threshold){
    
    df_prepared_train = df_quality_check %>% 
      filter(date <= as.Date(end_date_train))%>%
      rmw_prepare_data(na.rm = TRUE)
    
    
    df_prepared_predict = df_quality_check %>% 
      filter(date <= as.Date(end_date_predict)) %>%
      rmw_prepare_data(na.rm = TRUE)
    
    
    variables_atmos = c("ws","wd","air_temp",
                        "RH", "atmospheric_pressure", "date_unix", "day_julian", "weekday", "hour")
    
    variables_no_atmos = c("ws","wd","air_temp",
                           "RH", "date_unix", "day_julian", "weekday", "hour")
    
    
    if(sum(df_prepared_predict$atmospheric_pressure, na.rm=T)>0){
      
      print(paste(site, "df contains atmospheric pressure variable"))
      rm_df_train = rmw_train_model(df_prepared_train,
                                    variables = variables_atmos,
                                    n_trees = n_tree,
                                    verbose = TRUE)
      
      rm_df_normal = rmw_do_all(df_prepared_predict,
                                variables = variables_atmos,
                                n_trees = n_tree,
                                n_samples = n_tree,
                                verbose = TRUE)
      
      rm_df_train_test = rmw_predict_the_test_set(
        rm_df_train, 
        df = df_prepared_train
      )
      
      testing_mse = as.numeric(sum((rm_df_train_test$value-rm_df_train_test$value_predict)^2)/
                                 count(rm_df_train_test))
      testing_rsquared = cor(rm_df_train_test$value, 
                             rm_df_train_test$value_predict)^2
      
      
      rm_df_predict = df_prepared_predict %>% mutate(value_predict = rmw_predict(
        rm_df_train, 
        df = df_prepared_predict),
        rm_normal_value = rm_df_normal$normalised$value_predict,
        training_rsquared = rm_df_train$r.squared, 
        training_mse = rm_df_train$prediction.error,
        testing_rsquared = testing_rsquared, testing_mse = testing_mse) %>%
        select(date, value, rm_normal_value, value_predict, training_rsquared, training_mse, testing_rsquared,
               testing_mse)
      
    } else {
      
      print(paste(site, "df does not contains atmospheric pressure variable"))
      rm_df_train = rmw_train_model(df_prepared_train,
                                    variables = variables_no_atmos,
                                    n_trees = n_tree,
                                    verbose = TRUE)
      
      rm_df_normal = rmw_do_all(df_prepared_predict,
                                variables = variables_no_atmos,
                                n_trees = n_tree,
                                n_samples = n_tree,
                                verbose = TRUE)
      
      rm_df_train_test = rmw_predict_the_test_set(
        rm_df_train, 
        df = df_prepared_train
      )
      
      testing_mse = as.numeric(sum((rm_df_train_test$value-rm_df_train_test$value_predict)^2)/
                                 count(rm_df_train_test))
      testing_rsquared = cor(rm_df_train_test$value, 
                             rm_df_train_test$value_predict)^2
      
      
      rm_df_predict = df_prepared_predict %>% mutate(value_predict = rmw_predict(
        rm_df_train, 
        df = df_prepared_predict),
        rm_normal_value = rm_df_normal$normalised$value_predict,
        training_rsquared = rm_df_train$r.squared, 
        training_mse = rm_df_train$prediction.error,
        testing_rsquared = testing_rsquared, testing_mse = testing_mse) %>%
        select(date, value, rm_normal_value, value_predict, training_rsquared, training_mse, testing_rsquared,
               testing_mse)
      
      
      
      print(paste(site, "rm process complete"))
      print("---------------------------------------------------------------------")
      return(rm_df_predict)
    }}
  else{print(paste(site, "skipped due to insufficient data"))
    print("---------------------------------------------------------------------")
  }
}

#---------------------------Creates df that contains observed vs BAU data using rmweather--------------
#also contains relevant statistics e.g. MSE, rsquared coefficient for linear regression
# note, there is no normalisation data in this function

rmweather_BAU_observed_no_normal = function(df, site, n_tree, begin_date_train, end_date_train, 
                                  end_date_predict, data_threshold){
  
  print(paste(site, "rm process beginning"))
  
  begin_year = as.numeric(substring(begin_date_train,1,4))
  end_year = as.numeric(substring(end_date_train,1,4))
  
  begin_month = as.numeric(substring(begin_date_train,6,7))
  end_month = as.numeric(substring(end_date_train,6,7))
  
  begin_day = as.numeric(substring(begin_date_train,9,10))
  end_day = as.numeric(substring(end_date_train,9,10))  
  
  diff_hours = c(ISOdate(end_year,end_month,end_day), ISOdate(begin_year,begin_month,begin_day))
  
  diff_hours_quality_check = difftime(diff_hours[1], diff_hours[2], units="hours")+1
  
  df_quality_check = df %>% 
    filter(date >= as.Date(begin_date_train), code == site)
  
  total_date_df = length(df_quality_check$date)
  print(length(df_quality_check$date)/as.numeric(diff_hours_quality_check))
  
  if(length(df_quality_check$date)/as.numeric(diff_hours_quality_check) >= data_threshold){
    
    df_prepared_train = df_quality_check %>% 
      filter(date <= as.Date(end_date_train))%>%
      rmw_prepare_data(na.rm = TRUE)
    
    
    df_prepared_predict = df_quality_check %>% 
      filter(date <= as.Date(end_date_predict)) %>%
      rmw_prepare_data(na.rm = TRUE)
    
    
    variables_atmos = c("ws","wd","air_temp",
                        "RH", "atmospheric_pressure", "date_unix", "day_julian", "weekday", "hour")
    
    variables_no_atmos = c("ws","wd","air_temp",
                           "RH", "date_unix", "day_julian", "weekday", "hour")
    
    
    if(sum(df_prepared_predict$atmospheric_pressure, na.rm=T)>0){
      
      print(paste(site, "df contains atmospheric pressure variable"))
      rm_df_train = rmw_train_model(df_prepared_train,
                                    variables = variables_atmos,
                                    n_trees = n_tree,
                                    verbose = TRUE)
      
      
      rm_df_train_test = rmw_predict_the_test_set(
        rm_df_train, 
        df = df_prepared_train
      )
      
      testing_mse = as.numeric(sum((rm_df_train_test$value-rm_df_train_test$value_predict)^2)/
                                 count(rm_df_train_test))
      testing_rsquared = cor(rm_df_train_test$value, 
                             rm_df_train_test$value_predict)^2
      
      
      rm_df_predict = df_prepared_predict %>% mutate(value_predict = rmw_predict(
        rm_df_train, 
        df = df_prepared_predict),
        training_rsquared = rm_df_train$r.squared, 
        training_mse = rm_df_train$prediction.error,
        testing_rsquared = testing_rsquared, testing_mse = testing_mse) %>%
        select(date, value, value_predict, training_rsquared, training_mse, testing_rsquared,
               testing_mse)
      
    } else {
      
      print(paste(site, "df does not contains atmospheric pressure variable"))
      rm_df_train = rmw_train_model(df_prepared_train,
                                    variables = variables_no_atmos,
                                    n_trees = n_tree,
                                    verbose = TRUE)
      
      rm_df_train_test = rmw_predict_the_test_set(
        rm_df_train, 
        df = df_prepared_train
      )
      
      testing_mse = as.numeric(sum((rm_df_train_test$value-rm_df_train_test$value_predict)^2)/
                                 count(rm_df_train_test))
      testing_rsquared = cor(rm_df_train_test$value, 
                             rm_df_train_test$value_predict)^2
      
      
      rm_df_predict = df_prepared_predict %>% mutate(value_predict = rmw_predict(
        rm_df_train, 
        df = df_prepared_predict),
        training_rsquared = rm_df_train$r.squared, 
        training_mse = rm_df_train$prediction.error,
        testing_rsquared = testing_rsquared, testing_mse = testing_mse) %>%
        select(date, value, value_predict, training_rsquared, training_mse, testing_rsquared,
               testing_mse)
      
      
      
      print(paste(site, "rm process complete"))
      print("---------------------------------------------------------------------")
      return(rm_df_predict)
    }}
  else{print(paste(site, "skipped due to insufficient data"))
    print("---------------------------------------------------------------------")
  }
}


#---------------reformats output from BAU vs Observed to make more user friendly ------
urban_reformat_data_mean_sd = function(df, UK_code){
  names(df) = UK_code
  
  predict_df = plyr::ldply(df, 
                           data.frame) %>%
    filter(!is.na(date)) %>%
    rename(sites = .id, BAU = value_predict, observed = value)%>%
    select(sites, date, BAU, rm_normal_value, observed)
  
  predict_df_mean = predict_df %>%
    timeAverage(avg.time = "1 day", statistic = c("mean"))%>%
    rename(BAU_mean = BAU, observed_mean = observed, rm_normal_mean = rm_normal_value) %>%
    mutate(delta_BAU_predict_mean = observed_mean- BAU_mean)
  
  predict_df_sd = predict_df %>%
    timeAverage(avg.time = "1 day", statistic = c("sd")) %>%
    rename(BAU_sd = BAU, observed_sd = observed, rm_normal_sd = rm_normal_value) %>%
    mutate(delta_BAU_predict_sd = abs(observed_sd-BAU_sd))
  
  predict_df_frequency = predict_df %>%
    select(-c(BAU, rm_normal_value)) %>%
    timeAverage(avg.time = "1 day", statistic = c("frequency"))%>%
    rename(observed_frequency = observed)
  
  predict_df_combo = left_join(predict_df_mean, predict_df_sd, by = "date") %>% 
    left_join(., predict_df_frequency, by = "date")
  
  predict_df_combo = predict_df_combo %>%
    mutate(CI_lower = observed_mean - 1.96*(observed_sd/sqrt(observed_frequency)),
           CI_upper = observed_mean + 1.96*(observed_sd/sqrt(observed_frequency))) %>%
    select(-observed_frequency) %>%
    mutate(d7_rollavg_delta_BAU_predict_mean = roll_mean(delta_BAU_predict_mean, n = 7, align = "right", 
                                                         fill = NA),
           d7_rollavg_delta_BAU_predict_sd = roll_mean(delta_BAU_predict_sd, n = 7, align = "right", 
                                                       fill = NA),
           d7_rollavg_observed_mean = roll_mean(observed_mean, n = 7, align = "right", 
                                                fill = NA),
           d7_rollavg_BAU_mean = roll_mean(BAU_mean, n = 7, align = "right", 
                                           fill = NA),
           d7_rollavg_CI_lower = roll_mean(CI_lower, n = 7, align = "right", 
                                           fill = NA),
           d7_rollavg_CI_upper = roll_mean(CI_upper, n = 7, align = "right", 
                                           fill = NA),
           d7_rollavg_rm_normal_mean = roll_mean(rm_normal_mean, n = 7, align = "right", 
                                                 fill = NA))
  
  return(predict_df_combo)
  
}

#---------returns delta alongside meteorological variables --------------

urban_reformat_data_delta_wd_ws = function(df, df2, UK_code){
  names(df) = UK_code
  
  predict_df = plyr::ldply(df, 
                           data.frame) %>%
    filter(!is.na(date)) %>%
    rename(sites = .id, BAU = value_predict, observed = value)%>%
    mutate(delta = observed - BAU)%>%
    select(sites, date, delta, observed, BAU)
  
  wd_ws = df2 %>%
    select(date, code, ws, wd, air_temp, RH) %>%
    rename(sites = code)
  
  
  predict_df_combo = merge(predict_df, wd_ws, by = c("date", "sites")) %>%
    mutate() %>%
    timeAverage(avg.time = "1 day", statistic = c("mean")) %>%
    mutate(Delta = zoo::rollapply(delta,7,mean,align='right',fill=NA),
           BAU = zoo::rollapply(BAU,7,mean,align='right',fill=NA),
           observed = zoo::rollapply(observed,7,mean,align='right',fill=NA),
           WS = zoo::rollapply(ws,7,mean,align='right',fill=NA),
           WD = zoo::rollapply(wd,7,mean,align='right',fill=NA),
           Temp = zoo::rollapply(air_temp,7,mean,align='right',fill=NA),
           RelHum = zoo::rollapply(RH,7,mean,align='right',fill=NA)) %>%
    drop_na()%>%
    select(-c(delta, wd, ws, air_temp, RH))%>%
     pivot_longer(-date, names_to = "variables")%>%
     mutate(variables = factor(variables, 
                               levels = c("Delta", "BAU", "observed", "WS", "WD", "Temp", "RelHum")))
     
  
  
  
  return(predict_df_combo)
  
  
}

#---------returns delta alongside meteorological variables --------------

urban_reformat_data_BAU_output = function(df, UK_code){
  names(df) = UK_code
  
  predict_df = plyr::ldply(df, 
                           data.frame) %>%
    filter(!is.na(date)) %>%
    rename(sites = .id, BAU = value_predict, observed = value)%>%
    mutate(delta = observed - BAU)%>%
    select(sites, date, BAU, observed, delta)%>%
    timeAverage(avg.time = "1 day", statistic = c("mean")) %>%
    mutate(delta = zoo::rollapply(delta,7,mean,align='right',fill=NA),
           BAU = zoo::rollapply(BAU,7,mean,align='right',fill=NA),
           observed = zoo::rollapply(observed,7,mean,align='right',fill=NA)) %>%
    drop_na %>%
    pivot_longer(-date, names_to = "variables")
  
  
  return(predict_df)
  
  
}


#------just observed----------------
urban_reformat_observed = function(df, UK_code){
  names(df) = UK_code
  
  predict_df = plyr::ldply(df, 
                           data.frame) %>%
    filter(!is.na(date)) %>%
    rename(sites = .id, BAU = value_predict, observed = value)%>%
    select(sites, date, observed)
  
  predict_df_mean = predict_df %>%
    timeAverage(avg.time = "1 day", statistic = c("median"))%>%
    rename(observed_mean = observed)
  
  predict_df_max = predict_df %>%
    timeAverage(avg.time = "1 day", statistic = c("max"))%>%
    rename(observed_max = observed)
  
  predict_df_min = predict_df %>%
    timeAverage(avg.time = "1 day", statistic = c("min"))%>%
    rename(observed_min = observed)
  
  predict_df_sd = predict_df %>%
    timeAverage(avg.time = "1 day", statistic = c("sd"))%>%
    rename(observed_sd = observed)

  predict_df_combo = left_join(predict_df_mean, predict_df_max, by = "date") %>% 
    left_join(., predict_df_min, by = "date") %>%
    left_join(., predict_df_sd, by = "date")
  
  predict_df_combo = predict_df_combo %>%
    mutate(d7_rollavg_mean = zoo::rollapply(observed_mean,7,mean,align='right',fill=NA),
           d7_rollavg_max = zoo::rollapply(observed_max,7,mean,align='right',fill=NA),
           d7_rollavg_min = zoo::rollapply(observed_min,7,mean,align='right',fill=NA),
           d7_rollavg_sd = zoo::rollapply(observed_sd,7,mean,align='right',fill=NA))
  
  return(predict_df_combo)
  
}

#---------------reformats output from normalised data to make more user friendly ------
urban_reformat_data_mean_sd_no_normal = function(df, UK_code, ULEZ_code,
                                                 normal=TRUE, ULEZ = TRUE){
  names(df) = UK_code
  
  if(normal==TRUE){
    if(ULEZ==TRUE){
      predict_df = plyr::ldply(df, 
                               data.frame) %>%
        filter(!is.na(date)) %>%
        rename(sites = .id, normal_value = value_predict, observed = raw_value)%>%
        select(sites, date, normal_value, observed)%>%
        filter(sites %in% ULEZ_code) 
    }
    else{
      predict_df = plyr::ldply(df, 
                               data.frame) %>%
        filter(!is.na(date)) %>%
        rename(sites = .id, normal_value = value_predict, observed = raw_value)%>%
        select(sites, date, normal_value, observed) 
    }

    
    predict_df_mean = predict_df %>%
      timeAverage(avg.time = "1 day", statistic = c("mean"))%>%
      rename(normal_mean = normal_value, observed_mean = observed)
    
    predict_df_sd = predict_df %>%
      timeAverage(avg.time = "1 day", statistic = c("sd")) %>%
      rename(normal_sd = normal_value, observed_sd = observed)
    
    predict_df_frequency = predict_df %>%
      select(-normal_value) %>%
      timeAverage(avg.time = "1 day", statistic = c("frequency"))%>%
      rename(observed_frequency = observed)
    
    predict_df_combo = left_join(predict_df_mean, predict_df_sd, by = "date") %>% 
      left_join(., predict_df_frequency, by = "date")
    
    predict_df_combo = predict_df_combo %>%
      mutate(CI_lower = observed_mean - 1.96*(observed_sd/sqrt(observed_frequency)),
             CI_upper = observed_mean + 1.96*(observed_sd/sqrt(observed_frequency))) %>%
      select(-observed_frequency) %>%
      mutate(d7_rollavg_normal_mean = zoo::rollapply(normal_mean,7,mean,align='right',fill=NA),
             d7_rollavg_normal_sd = zoo::rollapply(normal_sd,7,mean,align='right',fill=NA),
             d7_rollavg_observed_mean = zoo::rollapply(observed_mean,7,mean,align='right',fill=NA),
             d7_rollavg_observed_sd = zoo::rollapply(observed_sd,7,mean,align='right',fill=NA),
             d7_rollavg_CI_lower = zoo::rollapply(CI_lower,7,mean,align='right',fill=NA),
             d7_rollavg_CI_upper = zoo::rollapply(CI_upper,7,mean,align='right',fill=NA))
    
    return(predict_df_combo)
  }
  else{
    if(ULEZ==TRUE){
      predict_df = plyr::ldply(df, 
                               data.frame) %>%
        filter(!is.na(date)) %>%
        rename(sites = .id, BAU = value_predict, observed = value)%>%
        select(sites, date, BAU, observed)%>%
        filter(sites %in% ULEZ_code) 
    }
    else{
      predict_df = plyr::ldply(df, 
                               data.frame) %>%
        filter(!is.na(date)) %>%
        rename(sites = .id, BAU = value_predict, observed = value)%>%
        select(sites, date, BAU, observed)  
    }
    predict_df_mean = predict_df %>%
      timeAverage(avg.time = "1 day", statistic = c("mean"))%>%
      rename(BAU_mean = BAU, observed_mean = observed) %>%
      mutate(delta_BAU_predict_mean = observed_mean- BAU_mean)
    
    predict_df_sd = predict_df %>%
      timeAverage(avg.time = "1 day", statistic = c("sd")) %>%
      rename(BAU_sd = BAU, observed_sd = observed) %>%
      mutate(delta_BAU_predict_sd = abs(observed_sd-BAU_sd))
    
    predict_df_frequency = predict_df %>%
      select(-BAU) %>%
      timeAverage(avg.time = "1 day", statistic = c("frequency"))%>%
      rename(observed_frequency = observed)
    
    predict_df_combo = left_join(predict_df_mean, predict_df_sd, by = "date") %>% 
      left_join(., predict_df_frequency, by = "date")
    
    predict_df_combo = predict_df_combo %>%
      mutate(CI_lower = observed_mean - 1.96*(observed_sd/sqrt(observed_frequency)),
             CI_upper = observed_mean + 1.96*(observed_sd/sqrt(observed_frequency))) %>%
      select(-observed_frequency) %>%
      mutate(d7_rollavg_delta_BAU_predict_mean = zoo::rollapply(delta_BAU_predict_mean,7,mean,align='right',fill=NA),
             d7_rollavg_delta_BAU_predict_sd = zoo::rollapply(delta_BAU_predict_sd,7,mean,align='right',fill=NA),
             d7_rollavg_observed_mean = zoo::rollapply(observed_mean,7,mean,align='right',fill=NA),
             d7_rollavg_BAU_mean = zoo::rollapply(BAU_mean,7,mean,align='right',fill=NA),
             d7_rollavg_CI_lower = zoo::rollapply(CI_lower,7,mean,align='right',fill=NA),
             d7_rollavg_CI_upper = zoo::rollapply(CI_upper,7,mean,align='right',fill=NA))
    
    return(predict_df_combo)
  }
  
  
  
}


#---------------reformats output from BAU vs Observed to make more user friendly ------
#note doesn't include normalisation term
urban_reformat_data_mean_sd_no_normal = function(df, UK_code){
  names(df) = UK_code
  
  predict_df = plyr::ldply(df, 
                           data.frame) %>%
    filter(!is.na(date)) %>%
    rename(sites = .id, BAU = value_predict, observed = value)%>%
    select(sites, date, BAU, observed)
  
  predict_df_mean = predict_df %>%
    timeAverage(avg.time = "1 day", statistic = c("mean"))%>%
    rename(BAU_mean = BAU, observed_mean = observed) %>%
    mutate(delta_BAU_predict_mean = observed_mean- BAU_mean)
  
  predict_df_sd = predict_df %>%
    timeAverage(avg.time = "1 day", statistic = c("sd")) %>%
    rename(BAU_sd = BAU, observed_sd = observed) %>%
    mutate(delta_BAU_predict_sd = abs(observed_sd-BAU_sd))
  
  predict_df_frequency = predict_df %>%
    select(-BAU) %>%
    timeAverage(avg.time = "1 day", statistic = c("frequency"))%>%
    rename(observed_frequency = observed)
  
  predict_df_combo = left_join(predict_df_mean, predict_df_sd, by = "date") %>% 
    left_join(., predict_df_frequency, by = "date")
  
  predict_df_combo = predict_df_combo %>%
    mutate(CI_lower = observed_mean - 1.96*(observed_sd/sqrt(observed_frequency)),
           CI_upper = observed_mean + 1.96*(observed_sd/sqrt(observed_frequency))) %>%
    select(-observed_frequency) %>%
    mutate(d7_rollavg_delta_BAU_predict_mean = zoo::rollapply(delta_BAU_predict_mean,7,mean,align='right',fill=NA),
           d7_rollavg_delta_BAU_predict_sd = zoo::rollapply(delta_BAU_predict_sd,7,mean,align='right',fill=NA),
           d7_rollavg_observed_mean = zoo::rollapply(observed_mean,7,mean,align='right',fill=NA),
           d7_rollavg_BAU_mean = zoo::rollapply(BAU_mean,7,mean,align='right',fill=NA),
           d7_rollavg_CI_lower = zoo::rollapply(CI_lower,7,mean,align='right',fill=NA),
           d7_rollavg_CI_upper = zoo::rollapply(CI_upper,7,mean,align='right',fill=NA))
  
  return(predict_df_combo)
  
}

#---------crude change point detection to identify subtle changes in data--------------

df_cp_detection = function(df, window_width, rollmax_width, date = TRUE){
  if(date==TRUE){
    
    
    df_roll_regres = rollRegres::roll_regres(value ~ date, df, width = window_width,
                     do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
    
    df_regres_coeff = as.data.frame(df_roll_regres$coefs) %>%
      rename(grad = date) %>%
      mutate(date = df$date)
    
    
    df_2nd_derivative = as.data.frame(pracma::gradient(df_regres_coeff$grad)) %>%
      rename(change_point = "pracma::gradient(df_regres_coeff$grad)") %>%
      mutate(date = df_regres_coeff$date, change_point = abs(change_point),
             rollmax = zoo::rollmax(change_point, k = rollmax_width, align = "right", fill = NA),
             flag = rollmax-lag(rollmax) > 0 &  sign(rollmax) == sign(lead(rollmax, rollmax_width-1)) &
               rollmax > 0) 
  }
  else{
    df_roll_regres = rollRegres::roll_regres(value ~ index, df, width = window_width,
                                             do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
    
    df_regres_coeff = as.data.frame(df_roll_regres$coefs) %>%
      rename(grad = index) %>%
      mutate(index = df$index)
    
    
    df_2nd_derivative = as.data.frame(pracma::gradient(df_regres_coeff$grad)) %>%
      rename(change_point = "pracma::gradient(df_regres_coeff$grad)") %>%
      mutate(index = df_regres_coeff$index, change_point = round(abs(change_point), 7),
             rollmax = zoo::rollmax(change_point, k = rollmax_width, align = "right", fill = NA),
             flag = rollmax-lag(rollmax) > 0 &  rollmax == lead(rollmax, rollmax_width-1) &
               rollmax > 0)
    
  }
  
  return(df_2nd_derivative)
}

#------------creates ggplot object for statistics----------------------

df_cp_detection_statistics = function(df, window_width, date = TRUE){
  if(date==TRUE){
    df_roll_regres = rollRegres::roll_regres(value ~ date, df, width = window_width,
                                             do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
    
    df_stats = as.data.frame(df_roll_regres$sigmas)%>%
      rename(sigmas = "df_roll_regres$sigmas")%>%
      mutate(r.squareds = df_roll_regres$r.squareds, 
             date = df$date)%>%
      pivot_longer(-c(date), names_to = "statistics")
    
    df_stats_graphic = df_stats  %>%
      ggplot(aes(x = date, y = value)) +
      geom_line(aes(color = statistics))+
      theme_bw(base_size = 15) +
      theme(legend.position = "top")+
      facet_grid( statistics~., scales = "free_y") + 
      theme(panel.spacing = unit(2, "lines"))
    
  }
  else{
    df_roll_regres = rollRegres::roll_regres(value ~ index, df, width = window_width,
                                             do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
    
    df_stats = as.data.frame(df_roll_regres$sigmas)%>%
      rename(sigmas = "df_roll_regres$sigmas")%>%
      mutate(r.squareds = df_roll_regres$r.squareds, 
             index = df$index)%>%
      pivot_longer(-c(index), names_to = "statistics")
    
    df_stats_graphic = df_stats  %>%
      ggplot(aes(x = index, y = value)) +
      geom_line(aes(color = statistics))+
      theme_bw(base_size = 15) +
      theme(legend.position = "top") +
      labs(x = "Index", y = "Various units") +
      facet_grid( statistics~., scales = "free_y") + 
      theme(panel.spacing = unit(2, "lines"))
    
    
  }
  
  return(df_stats_graphic)
}

#-----------prepares df for cp detection ----------------------

df_cp_prepared = function(df, variable){
  df_prepared = df %>%
    select(date, any_of(variable))%>%
    rename(value = variable) %>%
    tibble() %>%
    drop_na()
  
  return(df_prepared)
}

#-------CPD code used to show sudden change points, uses distribution data -----

change_point_detection = function(df, begin_date, end_date, variable, ncpts, date = TRUE){
  if(date==TRUE){
    df_prepared = as.numeric(unlist(df %>%
                                     filter(date >= as.Date(begin_date) & date <= as.Date(end_date)) %>%
                                     select(any_of(variable))))
    
    cp_df_values = cpt.meanvar(df_prepared, method = "PELT", penalty = "CROPS",
                               pen.value = c(5,500))
    
    cp_df = df %>%
      select(date, any_of(variable)) %>%
      filter(date >= as.Date(begin_date) & date <= as.Date(end_date)) %>%
      mutate(index = row_number(), cp = index %in% unique(cp_df_values@cpts.full[,ncpts])) %>%
      filter(cp == TRUE)
  }
  else{
    
    df_prepared = unlist(df %>%
                           select(any_of(variable)))
    
    cp_df_values = cpt.meanvar(df_prepared, method = "PELT", penalty = "CROPS",
                               pen.value = c(5,500))
    cp_df = df %>%
      select(index, any_of(variable)) %>%
      mutate(cp = index %in% unique(cp_df_values@cpts.full[,ncpts])) %>%
      filter(cp == TRUE)
  }
 
  
  return(cp_df)
}


#-------------creates statistical output to constrain window length with SD --------------

window_length_constrain = function(df, window_length_vector, k, rollmax_width){
  
  roll_regression = rollRegres::roll_regres(value ~ index, df, 
                                            width = window_length_vector,
                                            do_compute = c("sigmas", "r.squareds", "1_step_forecasts"))  
  
  
  roll_reformat_cp = as.data.frame(roll_regression$coefs) %>%
    rename(grad = index) %>%
    mutate(index = df$index,
           r.squareds = roll_regression$r.squareds,
           data = df$value,
           grad = round(grad, k),
           window_length_level = as.factor(window_length_vector),
           derv_2nd = as.numeric(abs(pracma::gradient(grad))),
           rollmax = zoo::rollmax(derv_2nd, k = rollmax_width, align = "right", fill = NA),
           cp = rollmax-lag(rollmax) > 0 & rollmax == lead(rollmax, 4)
    ) %>%rename("Test dataset" = data,
                "Rolling gradient" = grad,
                "2nd derivative" = derv_2nd,
                "Rollmax" = rollmax)%>%
    select(-"(Intercept)") %>%
    drop_na() %>%
    pivot_longer(-c(index, window_length_level, cp), 
                 names_to = "variables")%>%
    mutate(variables = factor(variables, 
                              levels = c("Test dataset", "Rolling gradient", "2nd derivative", 
                                         "Rollmax", "r.squareds")))
  
  return(roll_reformat_cp)
}

#-----------function to apply sD vector, therefore adding noise to dataset-----------

apply_white_noise = function(df, standard_deviation_vector){
  if(standard_deviation_vector == 0){
    
    noise_df = df %>%
      mutate(noise = value,
             standard_deviation = as.factor(standard_deviation_vector))
  }
  
  else{
    noise_df = df %>%
      mutate(noise = value + standard_deviation_vector*runif(nrow(df)),
             standard_deviation = as.factor(standard_deviation_vector))
  }
  
  
  return(noise_df)
}

#--------- function to test what is a suitable choice in window length --------

change_point_model_statistics = function(df, window_length, stats = TRUE){
  df_reformat = df %>%
    filter(variables == "Test dataset", window_length_level == window_length) %>%
    mutate(date = lubridate::as_date(date))
  
  cp_df  = df_reformat %>%
    filter(cp == TRUE)
  
  ApproxFun = approxfun(x = cp_df$date, y = cp_df$value)
  Dates_seq <- seq.Date(ymd(df$date[1]), ymd(tail(df$date, n=1)), by = 1)
  LinearFit = ApproxFun(Dates_seq)
  
  df_approx = data.frame(Dates_seq, LinearFit)%>%
    rename(date = Dates_seq, approx_value = LinearFit)
  
  df_new = df %>%
    filter(window_length_level == window_length, variables == "Test dataset") 
  
  if(stats == TRUE){
    
    df_combo_temp = left_join(df_new, df_approx, by = "date") %>%
      drop_na() %>%
      select(date, value, approx_value)
    
    rmse = Metrics::rmse(df_combo_temp$value, df_combo_temp$approx_value)
    rsquared = cor(df_combo_temp$value, df_combo_temp$approx_value, method ="pearson")
    df_combo = data.frame(window_length, rmse, rsquared)
  }
  else{
    df_combo = left_join(df_new, df_approx, by = "date")%>%
      mutate(cp = df_reformat$cp)  %>%
      select(date, window_length_level, cp, value, approx_value)  %>%
      drop_na() %>%
      rename("f(x)" = value, "Approxed f(x)" = approx_value) %>%
      pivot_longer(-c(date, window_length_level, cp), names_to = "variables")
  }
  
  return(df_combo)
}


