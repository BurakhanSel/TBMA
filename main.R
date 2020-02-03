library(data.table)
library(ranger)
library(RcppRoll)
library(lubridate)
library(readr)
library(plyr)
library(forecast)
library(weathermetrics)
library(forecast)
library(caret)
library(zoo)


    number_of_replication<-1
    prediction_date<-as.Date("2018-01-01")                 #first date to predict                      
    num_of_days_to_predict<-372                          #how many days we want to predict
    directory_of_raw_data<-"C:/Users/Burakhan/Desktop/Dersler/Tree Based Moving Average 4/data.csv"
    source_date_based_info<-"C:/Users/Burakhan/Desktop/Dersler/Tree Based Moving Average 4/date_based_information.R"
    source_weather<-"C:/Users/Burakhan/Desktop/Dersler/Tree Based Moving Average 4/weather.R"
    directory_weather_file<-"C:/Users/Burakhan/Desktop/Dersler/Tree Based Moving Average 4/WeatherData/"       #only the file
    source_functions<-"C:/Users/Burakhan/Desktop/Dersler/Tree Based Moving Average 4/functions.R"
    num_of_days_in_adv<-7
    set.seed(1)
    
    
    source(source_functions)
    #data preparation
    processed_data<-data_preperation(directory_of_raw_data,source_date_based_info,source_weather,directory_weather_file)
    rm(list=setdiff(ls(), c("processed_data","prediction_date","num_of_days_in_adv","num_of_days_to_predict","number_of_replication",
                            "best_ranger","predict_arima","forecast_rfma_without_arima","ozel_gun_adlarý","ozel_gunler","feature_filter")))
    
    
    #feature selection
    processed_data<-processed_data[,colnames(processed_data) %in% c(feature_filter(processed_data),"Consumption","Date","Hour"), with=FALSE]
   
    #cv for ranger
    final_model<-best_ranger(processed_data =processed_data,prediction_date,num_of_days_in_adv = num_of_days_in_adv,method="timeslice",today=prediction_date-num_of_days_in_adv)

    
    
    
    forecast<-data.table()
    forecast_arima_table<-data.table()
    for(z in 1:num_of_days_to_predict){
        #tm=Sys.time()  
        #forecast_arima_table<-rbind(forecast_arima_table,data.table(predict_arima(processed_data = processed_data,today=prediction_date-num_of_days_in_adv+z-1,num_of_days_in_adv = num_of_days_in_adv)))
        #cat("\narima time:",(Sys.time()-tm),"   prediction date:",as.Date(prediction_date+z-1),"   num_of_days_in_adv:",num_of_days_in_adv,"\n" )
        for(y in 1:number_of_replication){ #seed
            tm=Sys.time()
            set.seed(y)
            forecast<-rbind(forecast,data.table(Today=prediction_date-num_of_days_in_adv+z-1,forecast_rfma_without_arima(processed_data = processed_data,today=prediction_date-num_of_days_in_adv+z-1,num_of_days_in_adv = num_of_days_in_adv,
                                                mtry =final_model$mtry,min_node_size = final_model$min_node_size,splitrule =final_model$splitrule),seed=y))
            cat("\n", z,"\n ranger time:",(Sys.time()-tm),"   prediction date:",as.Date(prediction_date+z-1),"  num_of_days_in_adv:",num_of_days_in_adv ,"  seed:",y,"\n")
        }
        
    }
    
    
    #result<-join(forecast_arima_table,forecast,by=c("Date","Hour","Actual Consumption","Today"))
    write.csv(forecast,file="result_witohut_arima.csv")
    write.csv(colnames(processed_data),file="colnames_after_selection.csv")
    write.csv(final_model,file="model_parameters.csv")
    
    
    