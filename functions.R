require(data.table)
require(ranger)
require(RcppRoll)
require(lubridate)
require(readr)
require(plyr)
require(forecast)
require(weathermetrics)
require(forecast)
require(caret)
library(zoo)
library(MLmetrics)

data_preperation<-function(directory_of_raw_data,source_date_based_info,source_weather,directory_weather_file){
  
  
  raw_data=as.data.table(read.csv(directory_of_raw_data)) #read data                                   
  
  raw_data[,Date:=as.Date(Date,format="%d.%m.%Y")]
  raw_data[,Hour:=as.character(hour(as.POSIXct(raw_data$Hour, format="%H:%M")))]
  raw_data[,Month:=as.character(month(as.POSIXct(raw_data$Date, format="%d.%m.%Y")))]
  raw_data[,Day_of_Month:=as.character(day(as.POSIXct(raw_data$Date, format="%d.%m.%Y")))]
  raw_data[,Consumption..MWh.:=as.numeric(gsub(",","",Consumption..MWh.))]
  colnames(raw_data)[colnames(raw_data)=="Consumption..MWh."] <- "Consumption"
  
  raw_data[,TWeekDay:=as.character(wday(Date))]

  #Date based information
  source(source_date_based_info)       
  
  
  #Adding Special Dates From Environment----
  for(i in 1:length(ozel_gun_adlarý)){
    list1<-as.data.table(ozel_gunler[i])
    raw_data<-raw_data[Date %in% list1$V1 , ozel_gun_adlarý[i]:=TRUE] 
    raw_data<-raw_data[!(Date %in% list1$V1) , ozel_gun_adlarý[i]:=FALSE]
    raw_data<-raw_data[(Date-1) %in% list1$V1 , ozel_gun_adlarý_lag1[i]:=TRUE] 
    raw_data<-raw_data[!((Date-1) %in% list1$V1) , ozel_gun_adlarý_lag1[i]:=FALSE]
    raw_data<-raw_data[(Date-7) %in% list1$V1 , ozel_gun_adlarý_lag7[i]:=TRUE] 
    raw_data<-raw_data[!((Date-7) %in% list1$V1) , ozel_gun_adlarý_lag7[i]:=FALSE]
  } 
  
  source(source_weather)
  wide_result<-weather_function(directory =directory_weather_file )
  
  raw_data<-join(raw_data,wide_result,by=c("Date","Hour"))
  

  
  for(k in names(wide_result)[-c(1:2)]){
    commnd=sprintf('raw_data[,%s:=as.numeric(na.interp(ts(%s)))]',k,k)
    eval(parse(text=commnd))
  }

  raw_data$day_number<- seq.int(nrow(raw_data))
  
  return(raw_data)
  
}

feature_filter<-function(processed_data){
  fit1=ranger(Consumption~ ., data = processed_data, num.trees = 500, 
             importance = "impurity_corrected", always.split.variables=c("Hour"),replace=TRUE,
             splitrule="extratrees", keep.inbag=TRUE,num.random.splits=5,num.threads=4)
  importances1<-fit1$variable.importance
  barplot(sort(importances1,decreasing = TRUE),las=2)
  importances1<-names(sort(importances1,decreasing = TRUE))[1:50]
  
  return(importances1)
}

best_ranger<-function(processed_data,prediction_date,num_of_days_in_adv,method,today){
  
  temp_processed_data<-processed_data[Date<=prediction_date]
  
  verbose=TRUE
  train=copy(temp_processed_data)
  train[,TWeekDay:=wday(Date)]
  train[,TWeekDay:=as.character(TWeekDay)]
  train[,LagConsumption7:=ifelse(Date-today>7,NA,shift(Consumption,168))]
  
  
  test=as.data.table(train[Date==prediction_date])
  result=as.data.table(test)
  train<-train[Date<=prediction_date-num_of_days_in_adv]
  ranger_test=as.data.table(test[,Consumption:=NA])
  
  trainx=train[,-c('Date'),with=F]
  train=train[complete.cases(train)]
  trainx=trainx[complete.cases(trainx)]
  ids=c(grep('Hour',names(train)))
  
  
  
  
  tgrid <- expand.grid(.mtry = c(floor(sqrt(ncol(train))),floor(ncol(train)/3)),
                       .splitrule = c("extratrees"),
                       .min.node.size = c(10,20,30))
  
  set.seed(1)
  my_partitions<-createTimeSlices(1:nrow(trainx),initialWindow = 180*24, horizon = 24*(num_of_days_in_adv),skip=(180*24-1),fixedWindow = T)
  ctrl <- trainControl(method=method,index = my_partitions$train, indexOut = my_partitions$test)
  trainx[,Hour:=as.numeric(Hour)]
  model_caret <- train(Consumption~ ., data = trainx,
                       method = "ranger",
                       trControl = ctrl,
                       tuneGrid = tgrid,
                       importance = c('impurity'),always.split.variables=c("Hour")
                       ,num.random.splits=5,num.threads=4,max.depth=14,replace=TRUE, num.trees = 500)
  return(model_caret$bestTune)
}

best_rfma<-function(processed_data,prediction_date,num_of_days_in_adv,method,today){
  
  temp_processed_data<-processed_data[Date<=prediction_date]
  
  verbose=TRUE
  train=copy(temp_processed_data)
  train[,LagConsumption7:=ifelse(Date-today>7,NA,shift(Consumption,168))]
  
  
  test=as.data.table(train[Date==prediction_date])
  result=as.data.table(test)
  train<-train[Date<=prediction_date-num_of_days_in_adv]
  ranger_test=as.data.table(test[,Consumption:=NA])
  train=train[complete.cases(train)]
  #trainx=train[,-c('Date'),with=F]
  
  
  
  
  tgrid <- expand.grid(.mtry = 7,
                       .splitrule = c("extratrees"),
                       .min.node.size = 10,
                       num_of_split=c(1),
                       with_replacement=c(TRUE),
                       always_split=list(c("Hour")))
  
  set.seed(1)
  my_partitions<-createTimeSlices(1:nrow(train),initialWindow = 180*24, horizon = 24*(num_of_days_in_adv),skip=(180*24-1),fixedWindow = T)
  ctrl <- trainControl(method=method,index = my_partitions$train, indexOut = my_partitions$test)
  
  rfma_cross_val<-data.table()

  
  for (i in 1: nrow(tgrid)){
    parameters<-tgrid[i,]
    for(k in 1:length(my_partitions$train)){
      train_ind<-unlist(my_partitions$train[k], use.names=FALSE)
      test_ind<-unlist(my_partitions$test[k], use.names=FALSE)
      train_cv<-train[train_ind,]
      train_cv_wo_date<-train_cv[,-c('Date'),with=F]
      test_cv<-train[test_ind,]
      test_cv_wo_date<-test_cv[,-c('Date'),with=F]
      test_cv_wo_date_response<-test_cv_wo_date[,Consumption:=NA]
      
      
      
      fit=ranger(Consumption~ ., data = train_cv_wo_date, num.trees = 500, mtry=parameters$.mtry, 
                 importance = "impurity",min.node.size=parameters$.min.node.size, always.split.variables=c("Hour"),replace=parameters$with_replacement,
                 splitrule=parameters$.splitrule, keep.inbag=TRUE,num.random.splits=parameters$num_of_split,num.threads=4)
      
      predicted_terminals=predict(fit,train_cv_wo_date,predict.all=TRUE,type='terminalNodes')$predictions
      inbag = data.table(do.call(cbind, fit$inbag.counts))
      inbag[inbag==0]=NA
      inbag[!is.na(inbag)]=1
      predicted_oob=predicted_terminals*as.matrix(inbag)
      baseline_train=data.table(train_cv[,list(Date,Hour,Consumption)],predicted_oob)
      
      
      predicted_terminals_test=predict(fit,test_cv_wo_date_response,predict.all=TRUE,type='terminalNodes')$predictions
      baseline_test=data.table(test_cv[,list(Date,Hour,Consumption)],predicted_terminals_test)
      
      baseline_whole=rbind(baseline_train,baseline_test)
      baseline_whole=baseline_whole[order(Date,Hour)]
      basenames=names(baseline_whole)
      baseline=melt(baseline_whole,id.vars=c('Date','Hour','Consumption'),measure_vars=basenames[!(basenames %in% c('Date','Hour','Consumption'))])
      baseline=baseline[!is.na(value)]
      
      setDT(baseline)[, id := .GRP, by=list(Hour,variable,value)]      
    
      #weighted moving average according to day difference
      baseline[, j :=as.numeric( Date-shift(Date, fill = NA,n=1)), by = id]
      baseline[, yj := shift(Consumption, fill = NA,n=1), by = id]
      #baseline[, k :=as.numeric( Date - shift(Date, fill = NA,n=2)), by = id]
      #baseline[, yk := shift(Consumption, fill = NA,n=2), by = id]
      #baseline[,lag_same_node:=ifelse( k<=30 & (!is.na(k))&(j<=30)&(!is.na(yj)),k*yj/(k+j)+j*yk/(k+j),
      #                                 ifelse((k>30|is.na(k)) & (j<=30)&(!is.na(yj)),yj,
      #                                        ifelse((j>30)&(!is.na(yj)),yj,NA)))]
      baseline[,lag_same_node:=ifelse( j<=30, yj,NA)]

      
      
      baseline_test_temp<-baseline[(baseline$Date %in% test_cv$Date)]
      baseline_test_temp[,lag_same_node:=na.locf(lag_same_node, na.rm = FALSE),by=id]
      
      temp=baseline_test_temp[,list(Similar_Consumption=median(lag_same_node,na.rm=T)),by=list(Date,Hour)]
      temp=temp[order(Date,Hour)]
      
      result<-data.table(join(test_cv, temp, by = c("Date","Hour"), type = "left", match = "all"))
      
      columns<-c("Date","Hour","Consumption","Similar_Consumption")
      result<-result[,colnames(result) %in% columns, with=FALSE]
      rfma_cross_val<-rbind(rfma_cross_val,data.table(mtry=parameters$.mtry,
                                                      splitrule=parameters$.splitrule,
                                                      min_node_size=parameters$.min.node.size,
                                                      number_of_split=as.factor(parameters$num_of_split),
                                                      always_split_variables=as.factor(parameters$always_split),
                                                      with_replacement=as.factor(parameters$with_replacement),
                                                      dataset_id=k,
                                                      mape=MAPE(y_true = result$Consumption,y_pred = result$Similar_Consumption)))
      
        }
  print(i)
  }
  
        
  cross_val<-rfma_cross_val[,mean(mape),by=list(mtry,min_node_size,number_of_split,always_split_variables,with_replacement)]

  return(cross_val)
}

forecast_rfma_without_arima<-function(processed_data,today,num_of_days_in_adv,mtry,min_node_size,splitrule){
  
  temp_processed_data<-processed_data[Date<=today+num_of_days_in_adv]
  
  verbose=TRUE
  train=copy(temp_processed_data)
  train[,TWeekDay:=wday(Date)]
  train[,TWeekDay:=as.character(TWeekDay)]
  train[,LagConsumption7:=ifelse(Date-today>7,NA,shift(Consumption,168))]
  
  
  test=as.data.table(train[Date>today & Date<=today+num_of_days_in_adv])
  result=as.data.table(test)
  ranger_train<-train[Date<=today]
  ranger_test=as.data.table(test[,Consumption:=NA])
  
  ranger_trainx=ranger_train[,-c('Date'),with=F]
  ranger_train=ranger_train[complete.cases(ranger_train)]
  ranger_trainx=ranger_trainx[complete.cases(ranger_trainx)]
  ids=c(grep('Hour',names(ranger_train)))
  
  
  
  fit=ranger(Consumption~ ., data = ranger_trainx, num.trees = 500, mtry=mtry, 
             importance = "impurity",min.node.size=min_node_size, always.split.variables=c(names(ranger_train)[ids],"ramazanbayrami","kurbanbayrami"),replace=TRUE,
             splitrule=splitrule, keep.inbag=TRUE,num.random.splits=5,num.threads=4,max.depth=14)
  
  
  prediction_data=rbind(ranger_train[complete.cases(ranger_train)])
  predicted_terminals=predict(fit,prediction_data,predict.all=TRUE,type='terminalNodes')$predictions
  inbag = data.table(do.call(cbind, fit$inbag.counts))
  inbag[inbag==0]=NA
  inbag[!is.na(inbag)]=1
  predicted_oob=predicted_terminals*as.matrix(inbag)
  baseline_train=data.table(ranger_train[,list(Date,Hour,Consumption)],predicted_oob)
  
  predicted_terminals_test=predict(fit,ranger_test,predict.all=TRUE,type='terminalNodes')$predictions
  baseline_test=data.table(ranger_test[,list(Date,Hour,Consumption)],predicted_terminals_test)
  
  baseline_whole=rbind(baseline_train,baseline_test)
  baseline_whole=baseline_whole[order(Date,Hour)]
  basenames=names(baseline_whole)
  baseline=melt(baseline_whole,id.vars=c('Date','Hour','Consumption'),measure_vars=basenames[!(basenames %in% c('Date','Hour','Consumption'))])
  baseline=baseline[!is.na(value)]
  
  setDT(baseline)[, id := .GRP, by=list(Hour,variable,value)]
  
  #weighted moving average according to day difference
  #baseline[, j :=as.numeric( Date-shift(Date, fill = NA,n=1)), by = id]
  baseline[, yj := shift(Consumption, fill = NA,n=1), by = id]
  #baseline[, k :=as.numeric( Date - shift(Date, fill = NA,n=2)), by = id]
  #baseline[, yk := shift(Consumption, fill = NA,n=2), by = id]
  #baseline[,lag_same_node:=ifelse( k<=30 & (!is.na(k))&(j<=30)&(!is.na(yj)),k*yj/(k+j)+j*yk/(k+j),
  #                                 ifelse((k>30|is.na(k)) & (j<=30)&(!is.na(yj)),yj,
  #                                        ifelse((j>30)&(!is.na(yj)),yj,NA)))]
  baseline[,lag_same_node:=yj]
                                   
                                   
  
  baseline_test_temp<-baseline[((Date>today)  & (Date<=today+num_of_days_in_adv))]
  baseline_test_temp[,lag_same_node:=na.locf(lag_same_node, na.rm = FALSE),by=id]
  
  #baseline[,lag_same_node:=roll_meanr(c(Consumption),3,fill = NA,na.rm = T,weights = c(0.5,0.5,0)),by=list(Hour,variable,value)]
  temp=baseline_test_temp[,list(Similar_Consumption=median(lag_same_node,na.rm=T)),by=list(Date,Hour)]
  temp=temp[order(Date,Hour)]
  
  #NA interpolation 
  #commnd=sprintf('temp[,%s:=as.numeric(na.interp(ts(%s)))]',"Similar_Consumption","Similar_Consumption")
  #eval(parse(text=commnd))
  
  
  #Random Forest----
  ranger_testx<-ranger_test[,-c('Date'),with=F]
  forecast_rf<-data.table(predict(fit,ranger_testx,predict.all=FALSE,type="response")$predictions)
  #end----    
  
  #test----
  
  result<-data.table(join(result, temp, by = c("Date","Hour"), type = "left", match = "all"))
  result[,Random_Forest:=forecast_rf]
  result[,Today:=today]
  #end----
  
  colnames(result)[3] <- "Actual Consumption"
  columns<-c("Date","Hour","Today","Actual Consumption","Similar_Consumption","Random_Forest")
  result<-result[,colnames(result) %in% columns, with=FALSE]
  return(result)
  
}    

predict_arima<-function(processed_data,today,num_of_days_in_adv){
  
  temp_processed_data<-processed_data[Date<=today+num_of_days_in_adv]
  
  verbose=TRUE
  train=copy(temp_processed_data)
  train[,LagConsumption7:=ifelse(Date-today>7,NA,shift(Consumption,168))]
  
  
  test=as.data.table(train[Date>today & Date<=today+num_of_days_in_adv])
  result=as.data.table(test)
  train<-train[Date<=today]
  test=as.data.table(test[,Consumption:=NA])
  
  trainx=train[,-c('Date'),with=F]
  trainx=train[complete.cases(train)]
  trainx=trainx[complete.cases(trainx)]
  
  
  #Arima----
  forecast_arima<-data.table()
  for(k in 0:23){
    consumption_ts<-ts(train[Hour==k]$Consumption,frequency=7,start = c(1))
    fit_arima<-auto.arima(consumption_ts)
    for(l in 1:num_of_days_in_adv){
      forecast_arima<-rbind(forecast_arima,data.table(Hour=k,Date=today+l,Today=today,Arima = data.table(forecast(fit_arima,h=num_of_days_in_adv)$mean[l])))
    }
  }
  #end----
  
  
  
  colnames(forecast_arima)[colnames(forecast_arima)=="Arima.V1"] <- "Arima"
  colnames(result)[3] <- "Actual Consumption"
  columns<-c("Date","Hour","Actual Consumption","Arima","Today")
  result<-join(result,forecast_arima,by=c("Date","Hour"))
  result<-result[,colnames(result) %in% columns, with=FALSE]
  return(result)
  
}


