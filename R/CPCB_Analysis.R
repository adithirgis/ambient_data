library(lubridate)
library(scales)
library(zoo)
library(caTools)
library(xts)
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(readr)
library(openair)
library(tidyverse)
library(purrr)
library(xlsx)


### Function to have no NA values in the columns
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
### Function to check for Mean+3SD and Mean-3SD; caution needs to have all the columns without NA values
LLD <- function(x, y, z) {
  if (is.na(x) ) {
    return (NA)
  }
  else if (x > (y+(3*z)) || x <(y-(3*z)) ){
    return (NA)
  }
  else if (is.null(x) || x == '' ||is.null(y) || y == ''||is.null(z) || z == ''  ) {
    return (NA)
  }else
  {
    return (x)
  }
}

### Create 3 blank dataframes 
CPCB_hour<-data.frame()
CPCB_daily<-data.frame()
CPCB_monthly<-data.frame()
CPCB_monthly1<-data.frame()

setwd("D:/Dropbox/APMfull/CPCB/Karnataka/Karnataka/Bengaluru")

sub_dir<-list.dirs(full.names = TRUE)
### List files where the data is kept
for (fol in (sub_dir)) { 
  dir<-fol
  shp_list1<- list.files(dir, pattern="\\.xlsx$",full.names=T)
  for (fil in (shp_list1)) {
    trial <- read.xlsx2(fil ,1,  startRow=17)
    
    ### Date needs to be changed from %H:%M to %H:%M:%S
    trial$date<- gsub(":00", ":00:00", trial$To.Date, fixed = TRUE)
    attributes(trial$date)$tzone <- "Asia/Kolkata"
    
    ### This folder contains files with different columns on below other so splitting it based on empty rows after a set of parameters
    trial$tbl_id <- cumsum(!nzchar(trial$date))
    trial <- trial[nzchar(trial$date), ]
    trial$From.Date<-NULL
    trial$To.Date<-NULL
    dt_s <- split(trial[, -ncol(trial)], trial$tbl_id)
    
    ### Three dataframes representing different parameters; Also the start an dthe end date was used from each fiel to create a time series dataframe
    PM<-data.frame(dt_s$`0`)
    PM$date <- as.POSIXct(PM$date, format='%d-%m-%Y %H:%M:%S')
    attributes(PM$date)$tzone <- "Asia/Kolkata"
    x1<-PM[1, "date"]
    x2<-tail(PM$date, n=3)
    y<-data.frame(x2)
    x2<-y[1, "x2"]
    
    tseries<-seq(
      from=as.POSIXct(x1, tz="Asia/Kolkata"),
      to=as.POSIXct(x2, tz="Asia/Kolkata"),
      by="60 min"
    ) 
    tseries_df<-data.frame(tseries)
    tseries_df$date<-tseries_df$tseries
    tseries_df$tseries<-NULL
    setDT(tseries_df)
    setDT(PM)
    setkey(tseries_df, date)
    setkey(PM, date)
    
    ### Join the three idfferent dataframe into a single one
    site1_join<-left_join(tseries_df, PM, by="date")
    
    Ben<-data.frame(dt_s$`2`)
    if(!nrow(Ben))
    {
      Ben<-tseries_df
      print("empty data.frame after one row")
    }else{
      Ben<- Ben[-1, ]
      Ben$date <- as.POSIXct(Ben$date, format='%d-%m-%Y %H:%M:%S')
      attributes(Ben$date)$tzone <- "Asia/Kolkata"
      rownames(Ben) <- NULL
    }
    setDT(Ben)
    setkey(Ben, date)
    Beny<-data.frame(dt_s$`4`)
    if(!nrow(Beny))
    {
      Beny<-tseries_df
      print("empty data.frame after one row Beny as well")
    }else{
      Beny<- Beny[-1, ]
      Beny$date <- as.POSIXct(Beny$date, format='%d-%m-%Y %H:%M:%S')
      attributes(Beny$date)$tzone <- "Asia/Kolkata"
      rownames(Beny) <- NULL
    }
    setDT(Beny)
    setkey(Beny, date)
    site1_join<-left_join(site1_join, Ben, by="date")
    site1_join<-left_join(site1_join, Beny, by="date")
    
    col_number<-which( colnames(site1_join)=="date" )
    columns.of.interest <- 2:ncol( site1_join )
    
    ### Replace all negative values in the desired columns with NA
    site1_join[ , columns.of.interest ] <- sapply( X = site1_join[ , columns.of.interest ]
                                                   , FUN = function(x) as.numeric(as.character(x)))
    site1_join[ , columns.of.interest ] <- sapply( X = site1_join[ , columns.of.interest ]
                                                   , FUN = function(x) ifelse(x<0, NA, x))
    ### For PM2.5 change all values above 985 to NA
    if("PM2.5" %in% colnames(site1_join))
    {
      a<-(site1_join$PM2.5)>985
      b<-((site1_join$PM2.5)<(0))
      site1_join$PM2.5<-ifelse(a,NA, site1_join$PM2.5)
      ### If PM10 values exist then check the rario of PM2.5/PM10 and if it is gretar than 1 then remove those values
      if("PM10" %in% colnames(site1_join))
      {
        site1_join$ratio<-site1_join$PM2.5/site1_join$PM10
        site1_join$PM2.5<-ifelse(site1_join$ratio>=1, NA,site1_join$PM2.5 )
        site1_join$PM10<-ifelse(site1_join$ratio>=1, NA,site1_join$PM10 )
      }else{
        site1_join$ratio<-NA
      }
    }
    site1_join$ratio<-NULL
    site1_join_f1<-site1_join
    
    ### Check for consecutive repeated value and remove them using consecutive difference as 0
    columns.of.interest <- 2:ncol( site1_join_f1 )
    site1_join_f1[ , columns.of.interest ]<-sapply( X = site1_join_f1[, columns.of.interest ], FUN=function(j)
      ifelse(c(FALSE, diff(as.numeric(j), 1, 1) == 0), NA, j))
    columns.of.interest <-2:ncol( site1_join_f1 )
    site1_join_f1[ , columns.of.interest ] <- sapply( X = site1_join_f1[ , columns.of.interest ] , FUN = function(x) as.numeric(as.character(x)))
    site1_join_f1$day<-lubridate::floor_date(site1_join_f1$date, "day")
    site1_join_f1$month<-format(site1_join_f1$date, "%m")
    name<-site1_join_f1
    name$day<-NULL
    name$month<-NULL
    name$date<-NULL
    
    ### Now calculate the Mean and SD for all parameters to check for the Mean and SD conditions
    FinalAll<-site1_join_f1%>%
      group_by(day)%>%
      mutate_all(funs(mean, sd), na.rm = TRUE)
    fil<-gsub(".xlsx","",fil)
    for(i in names(name)){
      data_list1<-FinalAll %>% dplyr:: select(date, day, starts_with(i))
      mean<-paste0(i,"_mean")
      sd<-paste0(i,"_sd")
      data_list1<-completeFun(data_list1, c(i, mean, sd))
      x<-data_list1[[i]]
      y<-grep("_mean", colnames(data_list1))
      y<-data_list1[[y]]
      z<-grep("_sd", colnames(data_list1))
      z<-data_list1[[z]]
      data_list1[[i]]<-mapply(LLD, x,y,z)
      # try<-setDT(data_list1)[, lapply(.SD, function(x) sum(!is.na(x))), by = day]
      data_list1<-data_list1%>% 
        group_by(day) %>%
        mutate_at(vars(contains(i)),list(no_hour = ~ sum(!is.na(.))))
      rem_mean<-grep("_mean_no_hour", colnames(data_list1))
      data_list1<-data_list1[,-rem_mean]
      rem_sd<-grep("_sd_no_hour", colnames(data_list1))
      data_list1<-data_list1[,-rem_sd]
      old_no<-paste0(i, "_no_hour")
      names(data_list1)[names(data_list1) ==old_no ] <- 'no_hour'
      data_list1 <- subset(data_list1, no_hour >=18)
      if (dim(data_list1)[1] == 0)
      {
        data_list1$no_hour<-NULL
      }else{
        grap<-calendarPlot(data_list1, i)
        png(filename=paste0(fil,"_",i,"_plot.jpg"))
        plot(grap)
        dev.off()
        data_list1$no_hour<-NULL
      }
      data_list1$day<-NULL
      setDT(data_list1)
      setkey(data_list1, date)
      tseries_df<-left_join(tseries_df, data_list1, by="date")
    }
    CPCB_hour<-tseries_df
    rem_sd<-grep("_sd", colnames(CPCB_hour))
    CPCB_hour<-CPCB_hour[,-rem_sd]
    rem_mean<-grep("mean", colnames(CPCB_hour))
    CPCB_hour<-CPCB_hour[,-rem_mean]
    columns.of.interest<-2:ncol(CPCB_hour)
    CPCB_hour[ , columns.of.interest ] <- sapply( X = CPCB_hour[ , columns.of.interest ]
                                                  , FUN = function(x) as.numeric(as.character(x)))
    CPCB_hour$day<-lubridate::floor_date(CPCB_hour$date, "day")
    Final_day_1<-CPCB_hour%>%
      group_by(day)%>%
      mutate_all(funs(mean, sd, median, IQR), na.rm = TRUE)
    Final_day_1$date_mean<-NULL
    Final_day_1$date_sd<-NULL
    Final_day_1$date_IQR<-NULL
    Final_day_1$date_median<-NULL
    name_f<-paste0(fil, "_hourly.csv")
    write.csv(Final_day_1, name_f )
    df_Place2 = data.frame(lapply(Final_day_1, as.character), stringsAsFactors=FALSE)
    # write.csv(df_Place2, name_f )
    Final_day_2<-CPCB_hour%>%
      group_by(day)%>%
      summarise_all(funs(mean, sd, median, IQR), na.rm = TRUE)
    Final_day_2$date_mean<-NULL
    Final_day_2$date_sd<-NULL
    Final_day_2$date_IQR<-NULL
    Final_day_2$date_median<-NULL
    name_f<-paste0(fil, "_daily.csv")
    write.csv(Final_day_2, name_f )
    df_Place3 = data.frame(lapply(Final_day_2, as.character), stringsAsFactors=FALSE)
    CPCB_hour$date<-NULL
    Final_day<-CPCB_hour%>%
      group_by(day)%>%
      summarise_all(funs(mean), na.rm = TRUE)
    Final_day$month<-format(Final_day$day, "%m")
    tseries1<-seq(
      from=as.POSIXct(x1,format='%Y-%m-%d'),
      to=as.POSIXct(x2, format='%Y-%m-%d'),
      by="1 day"
    ) 
    tseries_df1<-data.frame(tseries1)
    tseries_df1$day<-tseries_df1$tseries1
    tseries_df1$tseries1<-NULL
    tseries_df1$day<-lubridate::floor_date(tseries_df1$day, "day")
    setDT(tseries_df1)
    setkey(tseries_df1, day)
    for(i in names(name)){
      data_list1<-Final_day %>% dplyr:: select(day, month,starts_with(i))
      data_list1<-data_list1%>% 
        group_by(month) %>%
        mutate_at(vars(contains(i)),list(no_day = ~ sum(!is.na(.))))
      names(data_list1)[names(data_list1) == old_no ] <- 'no_day'
      data_list1 <- subset(data_list1, no_day >=23)
      data_list1$no_day<-NULL
      data_list1$month<-NULL
      setDT(data_list1)
      setkey(data_list1, day)
      tseries_df1<-left_join(tseries_df1, data_list1, by="day")
    }
    CPCB_daily<-tseries_df1
    columns.of.interest<-2:ncol(CPCB_daily)
    CPCB_daily[ , columns.of.interest ] <- sapply( X = CPCB_daily[ , columns.of.interest ]
                                                   , FUN = function(x) as.numeric(as.character(x)))
    CPCB_daily$month<-format(CPCB_daily$day, "%Y-%m")
    ### Calculate the monthly statistics.
    FinalAll_month<-CPCB_daily%>%
      group_by(month)%>%
      mutate_all(funs(mean, sd, median, IQR), na.rm = TRUE)
    FinalAll_month$day_mean<-NULL
    FinalAll_month$day_sd<-NULL
    FinalAll_month$day_IQR<-NULL
    FinalAll_month$day_median<-NULL
    FinalAll_month1<-CPCB_daily%>%
      group_by(month)%>%
      summarise_all(funs(mean, sd, median, IQR), na.rm = TRUE)
    FinalAll_month1$day_mean<-NULL
    FinalAll_month1$day_sd<-NULL
    FinalAll_month1$day_IQR<-NULL
    FinalAll_month1$day_median<-NULL
    name_f<-paste0(fil, "_monthly.csv")
    write.csv(FinalAll_month1, name_f )
    # CPCB_daily<-rbind(CPCB_daily,FinalAll_daily)
    # write.csv(FinalAll_daily, "Peenya_daily_2018.csv")
  }
}

