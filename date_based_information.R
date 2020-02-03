

#day light saving

summerTimes=c(as.Date('2016-03-27'),as.Date('2015-03-29'))
winterTimes=c(as.Date('2016-10-30'),as.Date('2015-11-08'))


yilbasi<-as.Date(c("2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01","2017-12-31","2018-01-01","2019-01-01"))
sevgililergunu<-as.Date(c("2010-02-14","2011-02-14","2012-02-14","2013-02-14","2014-02-14","2015-02-14","2016-02-14","2017-02-14","2018-02-14"))
cocukbayrami<-as.Date(c("2010-04-23","2011-04-23","2012-04-23","2013-04-23","2014-04-23","2015-04-23","2016-04-23","2017-04-23","2018-04-23"))
iscibayrami<-as.Date(c("2010-05-01","2011-05-01","2012-05-01","2013-05-01","2014-05-01","2015-05-01","2016-05-01","2017-05-01","2018-05-01"))
annelergunu<-as.Date(c("2010-05-09","2011-05-08","2012-05-13","2013-05-12","2014-05-11","2015-05-10","2016-05-08","2017-05-14","2018-05-13"))
genclikbayrami<-as.Date(c("2010-05-19","2011-05-19","2012-05-19","2013-05-19","2014-05-19","2015-05-19","2016-05-19","2017-05-19","2018-05-19"))
babalargunu<-as.Date(c("2010-06-20","2011-06-19","2012-06-17","2013-06-16","2014-06-15","2015-06-21","2016-06-19","2017-06-18"))
zaferbayrami<-as.Date(c("2010-08-30","2011-08-30","2012-08-30","2013-08-30","2014-08-30","2015-08-30","2016-08-30","2017-08-30","2018-08-30"))
cumhuriyet<-as.Date(c("2010-10-29","2011-10-29","2012-10-29","2013-10-29","2014-10-29","2015-10-29","2016-10-29","2017-10-29","2018-10-29"))
kadinlargunu<-as.Date(c("2010-03-08","2011-03-08","2012-03-08","2013-03-08","2014-03-08","2015-03-08","2016-03-08","2017-03-08","2018-03-08"))

ramazanbayrami<-as.Date(c("2010-09-09","2010-09-10","2010-09-11",
                          "2011-08-30","2011-08-31","2011-09-01",
                          "2012-08-19","2012-08-20","2012-08-21",
                          "2013-08-08","2013-08-09","2013-08-10",
                          "2014-07-24","2014-07-25","2014-07-26",
                          "2015-07-17","2015-07-18","2015-07-19",
                          "2016-07-04","2016-07-05","2016-07-06","2016-07-07",
                          "2017-06-24","2017-06-25","2017-06-26","2017-06-27",
                          "2018-06-14","2018-06-15","2018-06-16","2018-06-17")) #


kurbanbayrami<-as.Date(c("2010-11-16","2010-11-17","2010-11-18","2010-11-19",
                         "2011-11-06","2011-11-07","2011-11-08","2011-11-09",
                         "2012-10-25","2012-10-26","2012-10-27","2012-10-28",
                         "2013-10-15","2013-10-16","2013-10-17","2013-10-18",
                         "2014-10-04","2014-10-05","2014-10-06","2014-10-07",
                         "2015-09-23","2015-09-24","2015-09-25","2015-09-26","2015-09-27",
                         "2016-09-11","2016-09-12","2016-09-13","2016-09-14","2016-09-15",
                         "2017-08-31","2017-09-01","2017-09-02","2017-09-03","2017-09-04",
                         "2018-08-20","2018-08-21","2018-08-22","2018-08-23","2018-08-24")) #"2017-09-31",

uzunbayram=c(seq(as.Date('2016-09-10'),as.Date('2016-09-18'),by='day'),
                seq(as.Date('2017-08-26'),as.Date('2017-09-04'),by='day'),
                seq(as.Date('2018-08-18'),as.Date('2018-08-26'),by='day'))


okulacilisi<-as.Date(c("2010-02-08","2010-09-20",
                       "2011-02-14","2011-09-19",
                       "2012-02-06","2012-09-17",
                       "2013-02-11","2013-09-16",
                       "2014-02-10","2014-09-15",
                       "2015-02-09","2015-09-14",
                       "2016-02-09","2016-09-19"
                       ))

okulkapanisi<-as.Date(c("2010-01-22","2010-06-18",
                        "2011-01-28","2011-06-17",
                        "2012-01-20","2012-06-08",
                        "2013-01-25","2013-06-14",
                        "2014-01-24","2014-06-13",
                        "2015-01-23","2015-06-12",
                        "2016-01-23","2016-06-12"

))

semester<-as.Date(c(
  "2010-01-23","2010-01-24","2010-01-25","2010-01-26","2010-01-27","2010-01-28","2010-01-29","2010-01-30","2010-01-31","2010-02-01","2010-02-02","2010-02-03","2010-02-04","2010-02-05","2010-02-06","2010-02-07",
  "2011-01-29","2011-01-30","2011-01-31","2011-02-01","2011-02-02","2011-02-03","2011-02-04","2011-02-05","2011-02-06","2011-02-07","2011-02-08","2011-02-09","2011-02-10","2011-02-11","2011-02-12","2011-02-13",
  "2012-01-21","2012-01-22","2012-01-23","2012-01-24","2012-01-25","2012-01-26","2012-01-27","2012-01-28","2012-01-29","2012-01-30","2012-01-31","2012-02-01","2012-02-02","2012-02-03","2012-02-04","2012-02-05",
  "2013-01-26","2013-01-27","2013-01-28","2013-01-29","2013-01-30","2013-01-31","2013-02-01","2013-02-02","2013-02-03","2013-02-04","2013-02-05","2013-02-06","2013-02-07","2013-02-08","2013-02-09","2013-02-10",
  "2014-01-25","2014-01-26","2014-01-27","2014-01-28","2014-01-29","2014-01-30","2014-01-31","2014-02-01","2014-02-02","2014-02-03","2014-02-04","2014-02-05","2014-02-06","2014-02-07","2014-02-08","2014-02-09",
  "2015-01-26","2015-01-27","2015-01-28","2015-01-29","2015-01-30","2015-01-31","2015-02-01","2015-02-02","2015-02-03","2015-02-04","2015-02-05","2015-02-06",
  "2016-01-26","2016-01-27","2016-01-28","2016-01-29","2016-01-30","2016-01-31","2016-02-01","2016-02-02","2016-02-03","2016-02-04","2016-02-05","2016-02-06"
  )
)

kandil<-as.Date(c("2010-02-25","2010-06-17","2010-07-08","2010-07-26","2010-09-05",
                  "2011-04-14","2011-06-02","2011-06-28","2011-07-15","2011-08-26",
                  "2012-02-03","2012-05-24","2012-06-16","2012-07-04","2012-08-14",
                  "2013-01-23","2013-05-16","2013-06-05","2013-06-23","2013-08-03",
                  "2014-01-12","2014-05-01","2014-05-25","2014-06-12","2014-07-23",
                  "2015-01-02","2015-04-23","2015-05-15","2015-06-01","2015-07-12",
                   "2016-04-07","2016-05-03","2016-05-21","2016-07-01","2016-12-11",
                   "2017-03-30","2017-04-23","2017-05-10","2017-06-21","2017-11-30","2018-03-22","2018-04-13","2018-04-30","2018-10-19"
 ))


semester=c(semester,seq(as.Date('2018-01-22'),as.Date('2018-02-02'),by='day'),seq(as.Date('2017-01-23'),as.Date('2017-02-03'),by='day'))


ramazanbayramilkgun<-as.Date(c("2014-06-28","2015-07-17","2016-07-05","2017-06-23","2018-06-15"))
ramazanbayramisonrasi=lapply(ramazanbayramilkgun,function(x) seq(x+4,x+10,by='day'))
ramazanbayramisonrasi=as.Date(unlist(ramazanbayramisonrasi), origin = "1970-01-01")


kurbanbayramilkgun<-as.Date(c("2010-11-16","2011-11-06","2012-10-25","2013-10-15","2014-10-04","2015-09-23","2016-09-12","2017-08-31","2017-09-01","2018-08-21"))

ramazangunler=c(seq(as.Date('2015-06-18'),as.Date('2015-07-16'),by='day'),
                seq(as.Date('2016-06-06'),as.Date('2016-07-04'),by='day'),
                seq(as.Date('2017-05-27'),as.Date('2017-06-24'),by='day'),
                seq(as.Date('2018-05-16'),as.Date('2018-06-14'),by='day'))


yilbasioncesi=c(seq(as.Date('2014-12-01'),as.Date('2014-12-31'),by='day'),seq(as.Date('2015-12-01'),as.Date('2015-12-31'),by='day'))

kurbanoncesigunler=lapply(kurbanbayramilkgun,function(x) seq(x-14,x-1,by='day'))
kurbanoncesigunler=as.Date(unlist(kurbanoncesigunler), origin = "1970-01-01")

ramazanilkgun<-as.Date(c("2014-06-28","2015-06-17","2016-06-06","2017-05-27","2018-05-16"))
ramazanoncesigunler=lapply(ramazanilkgun,function(x) seq(x-30,x-1,by='day'))
ramazanoncesigunler=as.Date(unlist(ramazanoncesigunler), origin = "1970-01-01")

arife<-as.Date(c("2010-09-08", "2011-08-29","2012-08-18", "2013-08-07","2014-07-23","2015-07-16", "2016-07-04", "2017-06-24", "2018-06-14",
                "2015-09-23","2018-08-20","2017-08-31")) #

onbestemmuz<-as.Date(c("2017-07-15","2018-07-15","2019-07-15"))

holiday=c(ramazanbayrami,kurbanbayrami,yilbasi,iscibayrami,genclikbayrami,cocukbayrami,cumhuriyet,zaferbayrami,onbestemmuz)

outlierdays<-as.Date(c("2015-03-31","2016-07-15","2017-07-15","2018-06-14"))

elections<-as.Date(c("2015-10-01","2015-07-07","2017-04-16","2018-06-24","2019-03-31"))

examination<-as.Date(c("2018-06-30","2018-07-01","2017-03-12","2016-03-13","2015-03-15"))


single_day_holidays=c(yilbasi,iscibayrami,genclikbayrami,cocukbayrami,cumhuriyet,zaferbayrami,onbestemmuz)


ozel_gun_adlarý<-c( "annelergunu"      ,     "arife"          ,       "babalargunu"  ,         "cocukbayrami"   ,      
                    "cumhuriyet"       ,     "elections"   ,          "examination"       ,    "genclikbayrami"  ,     
                    "holiday"           ,    "iscibayrami"   ,        "kadinlargunu"     ,     "kandil"    ,           
                     "kurbanbayrami"      ,   "kurbanbayramilkgun"  ,  "kurbanoncesigunler"   , "okulacilisi"  ,        
                     "okulkapanisi"      ,    "onbestemmuz"     ,      "outlierdays"   ,        "ramazanbayrami"  ,     
                     "ramazanbayramilkgun",  "ramazanbayramisonrasi", "ramazangunler"      ,   "ramazanilkgun" ,       
                     "ramazanoncesigunler"  , "semester"   ,           "sevgililergunu" ,       "single_day_holidays" , 
                     "summerTimes"  ,         "uzunbayram"        ,    "winterTimes"  ,         "yilbasi"   ,           
                     "yilbasioncesi"   ,      "zaferbayrami"  )
ozel_gun_adlarý_lag1<-c( "annelergunu_lag1"      ,     "arife_lag1"          ,       "babalargunu_lag1"  ,         "cocukbayrami_lag1"   ,      
                    "cumhuriyet_lag1"       ,     "elections_lag1"   ,          "examination_lag1"       ,    "genclikbayrami_lag1"  ,     
                    "holiday_lag1"           ,    "iscibayrami_lag1"   ,        "kadinlargunu_lag1"     ,     "kandil_lag1"    ,           
                    "kurbanbayrami_lag1"      ,   "kurbanbayramilkgun_lag1"  ,  "kurbanoncesigunler_lag1"   , "okulacilisi_lag1"  ,        
                    "okulkapanisi_lag1"      ,    "onbestemmuz_lag1"     ,      "outlierdays_lag1"   ,        "ramazanbayrami_lag1"  ,     
                    "ramazanbayramilkgun_lag1",  "ramazanbayramisonrasi_lag1", "ramazangunler_lag1"      ,   "ramazanilkgun_lag1" ,       
                    "ramazanoncesigunler_lag1"  , "semester_lag1"   ,           "sevgililergunu_lag1" ,       "single_day_holidays_lag1" , 
                    "summerTimes_lag1"  ,         "uzunbayram_lag1"        ,    "winterTimes_lag1"  ,         "yilbasi_lag1"   ,           
                    "yilbasioncesi_lag1"   ,      "zaferbayrami_lag1"  )
ozel_gun_adlarý_lag7<-c( "annelergunu_lag7"      ,     "arife_lag7"          ,       "babalargunu_lag7"  ,         "cocukbayrami_lag7"   ,      
                         "cumhuriyet_lag7"       ,     "elections_lag7"   ,          "examination_lag7"       ,    "genclikbayrami_lag7"  ,     
                         "holiday_lag7"           ,    "iscibayrami_lag7"   ,        "kadinlargunu_lag7"     ,     "kandil_lag7"    ,           
                         "kurbanbayrami_lag7"      ,   "kurbanbayramilkgun_lag7"  ,  "kurbanoncesigunler_lag7"   , "okulacilisi_lag7"  ,        
                         "okulkapanisi_lag7"      ,    "onbestemmuz_lag7"     ,      "outlierdays_lag7"   ,        "ramazanbayrami_lag7"  ,     
                         "ramazanbayramilkgun_lag7",  "ramazanbayramisonrasi_lag7", "ramazangunler_lag7"      ,   "ramazanilkgun_lag7" ,       
                         "ramazanoncesigunler_lag7"  , "semester_lag7"   ,           "sevgililergunu_lag7" ,       "single_day_holidays_lag7" , 
                         "summerTimes_lag7"  ,         "uzunbayram_lag7"        ,    "winterTimes_lag7"  ,         "yilbasi_lag7"   ,           
                         "yilbasioncesi_lag7"   ,      "zaferbayrami_lag7"  )


ozel_gunler <- lapply(ozel_gun_adlarý, get)
