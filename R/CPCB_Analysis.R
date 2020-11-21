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
  if (is.na(x)) {
    return (NA)
  }
  else if (x > (y + (3 * z)) || x < (y - (3 * z)) ){
    return (NA)
  }
  else if (is.null(x) || x == '' || is.null(y) || y == '' || is.null(z) || z == '') {
    return (NA)
  } else
  {
    return (x)
  }
}
setwd("D:/Dropbox/APMfull/CPCB/1 Hour/Bengaluru/CPCB_ILK") 
sub_dir <- list.dirs(full.names = TRUE)
### List files where the data is kept 
for (fol in (sub_dir)) { 
  CPCB_hourly <- data.frame()
  CPCB_daily <- data.frame()
  CPCB_monthly <- data.frame()
  dir <- paste0("D:/Dropbox/APMfull/CPCB/1 Hour/Bengaluru/CPCB_ILK", fol)
  list_files <- list.files(dir, pattern = "\\.xlsx$", full.names = T)
  for (fil in (list_files)) {
    trial <- read.xlsx2("D:/Dropbox/ILKConsultancy/ambient_data/data/Bapuji Nagar/site_16320200330134440.xlsx",
                        1, startRow = 17)
    
    ### Date needs to be changed from %H:%M to %H:%M:%S
    trial$date <- gsub(":00", ":00:00", trial$To.Date, fixed = TRUE)
    
    ### This folder contains files with different columns on below other so 
    # splitting it based on empty rows after a set of parameters
    trial$tbl_id <- cumsum(!nzchar(trial$date))
    trial <- trial[nzchar(trial$date), ]
    trial[ ,c('From.Date', 'To.Date')] <- list(NULL)
    dt_s <- split(trial[, -ncol(trial)], trial$tbl_id)
    
    ### Three dataframes representing different parameters; Also the start and 
    # the end date was used from each fiel to create a time series dataframe
    PM <- data.frame(dt_s$`0`) %>%
      mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', tz = "Asia/Kolkata"))
    ye <- format(PM[1, "date"], format = "%Y")
    x1 <- as.POSIXct(paste0(ye, "-01-01 01:00:00"), 
                     format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
    ye <- format(tail(PM$date, n = 3)[1], format = "%Y")
    x2 <- as.POSIXct(paste0(ye, "-12-31 23:00:00"), 
                    format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
    date <- seq(
      from = as.POSIXct(x1, tz = "Asia/Kolkata"),
      to = as.POSIXct(x2, tz = "Asia/Kolkata"),
      by = "60 min"
    ) 
    tseries_df <- data.frame(date)
    
    ### Join the three idfferent dataframe into a single one
    site1_join <- left_join(tseries_df, PM, by = "date")
    
    make_df <- function(y, tseries_df) {
      df <- data.frame(y)
      if(!nrow(df))
      {
        df <- tseries_df
        print("empty data.frame after one tab")
      } else {
        names(df) <- as.matrix(df[1, ])
        df <- df[-1, ]
        df[] <- lapply(df, function(x) type.convert(as.character(x)))
        df <- Filter(function(x)! all(is.na(x)), df)
        df <- df %>%
          dplyr::select("date" = `To Date`, everything()) %>%
          mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', 
                                   tz = "Asia/Kolkata"))
      }
    }
    Ben <- make_df(dt_s$`2`, tseries_df)
    Beny <- make_df(dt_s$`4`, tseries_df)
    Bent <- make_df(dt_s$`6`, tseries_df)
    all <- list(Ben, Beny, Bent) %>% reduce(left_join, by = "date")
    # col_number <- which(colnames(Ben) == "date")
    col_interest <- 2:ncol(all)
    
    ### Replace all negative values in all the columns with NA
    all[ , col_interest] <- sapply(X = all[ , col_interest], 
                                           FUN = function(x) as.numeric(as.character(x)))
    all[ , col_interest] <- sapply(X = all[ , col_interest], 
                                          FUN = function(x) ifelse(x < 0, NA, x))
    ### For PM2.5 change all values above 985 and below zero to NA 
    if("PM2.5" %in% colnames(all))
    {
      a <- (all$PM2.5) > 985
      b <- (all$PM2.5) < 0
      all$PM2.5 <- ifelse(a | b, NA, all$PM2.5)
      ### If PM10 values exist then check the rario of PM2.5 / PM10 and if it is 
      # greater than 1 then remove those values
      if("PM10" %in% colnames(all))
      {
        all$ratio <- all$PM2.5 / all$PM10
        all$PM2.5 <- ifelse(all$ratio >= 1, NA, all$PM2.5)
        all$PM10 <- ifelse(all$ratio >= 1, NA, all$PM10)
      } else {
        all$ratio <- NA
      }
    }
    site1_join_f1 <- all %>%
      mutate(day = as.Date(date, format = '%Y-%m-%d', tz = "Asia/Kolkata")) %>%
      select(date, day, everything())
    
    ### Check for consecutive repeated value and remove them using consecutive 
    # difference as 0
    col_interest <- 3:ncol(site1_join_f1)
    site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[, col_interest], 
                                            FUN = function(j)
      ifelse(c(FALSE, diff(as.numeric(j), 1, 1) == 0), NA, j))
    site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                                      FUN = function(x) 
                                                        as.numeric(as.character(x)))
    name <- site1_join_f1 %>%
      dplyr::select(everything(), -day, -date)
    
    ### Now calculate the Mean and SD for all parameters to check for some 
    # conditions
    FinalAll <- site1_join_f1 %>%
      group_by(day) %>%
      mutate_all(funs(mean, sd), na.rm = TRUE) %>%
      select(everything(), -date_mean, -date_sd)
    fil <- gsub(".xlsx", "", "site_163")
    for(i in names(name)){
      data_list <- FinalAll %>% 
        dplyr::select(date, day, starts_with(i))
      ### Check if you have similar names matching eg - NO
      if(i == "NO") {
        CPCB_hour <- tseries_df %>%
          select(-contains(c("_sd", "_mean")))
        rem_no2 <- grep("NO2", colnames(data_list))
        data_list <- data_list[, -rem_no2] 
        rem_nox <- grep("NOx", colnames(data_list))
        data_list <- data_list[, -rem_nox] 
        mean <- paste0(i, "_mean")
        sd <- paste0(i, "_sd")
      } else {
        mean <- paste0(i, "_mean")
        sd <- paste0(i, "_sd")
      }
      ### Remove empty rows
      data_list <- completeFun(data_list, c(i, mean, sd))
      x <- data_list[[i]]
      y <- grep("_mean", colnames(data_list))
      y <- data_list[[y]]
      z <- grep("_sd", colnames(data_list))
      z <- data_list[[z]]
      ### Apply the condition of removing values which are >< (Mean +- 3 * SD)
      data_list[[i]] <- mapply(LLD, x, y, z)
      ### Apply the condition of removing values of a day where data captured is less than 18 hours
      data_list <- data_list %>% 
        group_by(day) %>%
        mutate_at(vars(contains(i)), list(no_hour = ~ sum(!is.na(.)))) %>%
        select(-contains(c("_sd_no_hour", "_mean_no_hour")))
      old_no <- paste0(i, "_no_hour")
      names(data_list)[names(data_list) == old_no] <- 'no_hour'
      data_list <- subset(data_list, no_hour >= 18)
      if (dim(data_list)[1] == 0)
      {
        data_list$no_hour <- NULL
      } else {
        grap <- calendarPlot(data_list, i)
        png(filename = paste0(fil, "_", i, "_plot.jpg"))
        plot(grap)
        dev.off()
        data_list$no_hour <- NULL
      }
      data_list$day <- NULL
      tseries_df <- left_join(tseries_df, data_list, by = "date")
    }
    CPCB_hour <- tseries_df %>%
      select(-contains(c("_sd", "_mean")))
    
    col_interest <- 2:ncol(CPCB_hour)
    CPCB_hour[ , col_interest] <- sapply(X = CPCB_hour[ , col_interest], 
                                         FUN = function(x) as.numeric(as.character(x)))
    CPCB_hour$day <- as.Date(CPCB_hour$date, format = '%Y-%m-%d', tz = "Asia/Kolkata")
    CPCB_hourly <- rbind(CPCB_hourly, CPCB_hour)
    CPCB_hour1 <- CPCB_hour
    tseries_df <- data.frame(date)
    CPCB_hour1 <- left_join(tseries_df, CPCB_hour, by = "date")
    if("PM2.5" %in% colnames(CPCB_hour) & "PM10" %in% colnames(CPCB_hour))
    {
      CPCB_hour <- CPCB_hour %>%
        mutate(ratio = PM2.5 / PM10)
    } else {
      CPCB_hour$ratio <- NA
    }
    write.csv(CPCB_hour, paste0(fil, "_hourly.csv"))
    
    
    ### Daily file
    Final_day_2 <- CPCB_hour %>%
      group_by(day) %>%
      summarise_all(funs(mean, sd, median, IQR), na.rm = TRUE)
    Final_day_2[ ,c('date_mean', 'date_sd', 'date_IQR', 'date_median', 'ratio_sd', 
                    'ratio_IQR', 'ratio_median')] <- list(NULL)
    day <- seq(
      from = as.Date(x1, format = '%Y-%m-%d', tz = "Asia/Kolkata"),
      to = as.Date(x2, format = '%Y-%m-%d', tz = "Asia/Kolkata"),
      by = "1 day"
    ) 
    tseries_df2 <- data.frame(day)
    Final_day_2$day <- as.Date(Final_day_2$day, format = '%Y-%m-%d', tz = "Asia/Kolkata")
    CPCB_daily <- rbind(CPCB_daily, Final_day_2)
    Final_day_2 <- left_join(tseries_df2, Final_day_2, by = "day")
    write.csv(Final_day_2, paste0(fil, "_daily.csv"))
    
    
    
    Final_day <- CPCB_hour %>%
      select(everything(), -date) %>%
      group_by(day) %>%
      summarise_all(funs(mean), na.rm = TRUE) %>%
      mutate(month = format(day, "%m"))
    tseries_df1 <- data.frame(day)
    
    for(i in names(name)){
      data_list1<-Final_day %>% dplyr:: select(day, month,starts_with(i))
      if(i=="NO"){
        rem_no2<-grep("NO2", colnames(data_list1))
        data_list1<-data_list1[,-rem_no2] 
        rem_nox<-grep("NOx", colnames(data_list1))
        data_list1<-data_list1[,-rem_nox] 
        mean<-paste0(i,"_mean")
        
      }else{
        mean<-paste0(i,"_mean")
      }
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
    CPCB_daily[ , columns.of.interest ] <- sapply( X = CPCB_daily[ , columns.of.interest ], FUN = function(x) as.numeric(as.character(x)))
    CPCB_daily$month<-format(CPCB_daily$day, "%Y-%m")
    ### Calculate the monthly statistics.
    FinalAll_month1<-CPCB_daily%>%
      group_by(month)%>%
      summarise_all(funs(mean, sd, median, IQR), na.rm = TRUE)
    FinalAll_month1$day_mean<-NULL
    FinalAll_month1$day_sd<-NULL
    FinalAll_month1$day_IQR<-NULL
    FinalAll_month1$day_median<-NULL
    CPCB_monthly<-rbind(CPCB_monthly,FinalAll_month1)
    write.csv(FinalAll_month1, paste0(fil, "_monthly.csv"))
  }
  setwd(paste0("D:/Dropbox/APMfull/CPCB/1 Hour/Bengaluru/CPCB_ILK/", fol))
  write.csv(CPCB_hourly, paste0(fol, "_hourly.csv"))
  write.csv(CPCB_daily, paste0(fol, "_daily.csv"))
  write.csv(CPCB_monthly, paste0(fol, "_monthly.csv"))
  
}  
 
  
  
