require(lubridate)
require(data.table)
require(weathermetrics)
require(forecast)

weather_function<-function(directory){

fls=list.files(directory,full.names=T)

weather=vector('list',length(fls))
for(i in 1:length(fls)){
  weather[[i]]=fread(fls[i])
  
}
weather_info=rbindlist(weather)

weather_info[,tmpc:=as.numeric(temp)]
weather_info[,relh:=as.numeric(rh)]
weather_info[,epoch:=ts]
weather_info[,timestamp:=as_datetime(epoch,tz='Turkey')]

weather_info[,Date:=date(timestamp)]
weather_info[,Hour:=hour(timestamp)]
setnames(weather_info,'stationid','station')
result=weather_info[,list(temperature=mean(tmpc),humidity=mean(relh)),by=list(lat,lon,Date,Hour)]
result=result[order(lat,lon,Date,Hour)]

result[humidity>100,humidity:=100]
result[,temp_fahr:=celsius.to.fahrenheit(temperature)]
result[,app_temp:=fahrenheit.to.celsius(heat.index(temp_fahr,rh=humidity,temperature.metric="fahrenheit",round=2))]

result=result[,list(lat,lon,Date,Hour,app_temp)]
result[,type:='actual']

 return(wide_result=dcast(result,Date+Hour~paste0('T_',lat,lon),value.var='app_temp'))

setnames(wide_result,names(wide_result)[-c(1:2)],paste0('T_',c(1:7)))
for(k in names(wide_result)[-c(1:2)]){
  commnd=sprintf('wide_result[,%s:=as.numeric(na.interp(ts(%s)))]',k,k)
  eval(parse(text=commnd))
}}
