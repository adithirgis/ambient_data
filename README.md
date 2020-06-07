# ambient_data
Ambient data analysis


## CPCB data cleaning process

For PM:

1.	Use only one-hour datasets
2.	Arrange all the one-hour data without any gaps, missing values impute with 'NaN'
3.	Delete all negatives
4.	Delete values greater than or equal to 985
5.	If PM10 is available, delete both PM2.5 and PM10 only when PM2.5 exceeds PM10
6.	If two consecutive values are the same, remove both of them
7.	On daily basis (24 data points), calculate the mean and SD, remove individual hourly values which doesn't fall in the interval [mean-3*SD  mean+3*SD]
8.	On the cleaned hourly dataset, if at least 75% (18 one-hour data points) of the data in a day is available, compute the daily mean, SD, median and IQR
9.	From daily means, compute the monthly means, SD, median and IQR only if at least 75% of the data in a month (23 days) is available
10.	Label them as daily_mean; daily_SD; daily_median; daily_IQR; monthly_mean; monthly_SD; monthly_median; monthly_IQR against date/month
11.	For each location, maintain one excel/CSV file for all parameters hourly values; another for all parameters daily values; another for all parameters monthly. Filename can be 'location_hourly/daily/monthly_startyear_endyear' (e.g. peenya_hourly _2009_2019)

For gases:

1.	Use only one-hour datasets
2.	Arrange all the one-hour data without any gaps, missing values impute with NaN
3.	Delete all negatives
4.	If two consecutive values are the same, remove both of them
5.	On daily basis (24 data points), calculate the mean and SD, remove values which doesn't fall in the interval [mean-3*SD  mean+3*SD]
6.	On the cleaned hourly dataset, if at least 75% (18 one-hour data points) of the data in a day is available, compute the daily mean, SD, median and IQR
7.	From daily means, compute the monthly means, SD, median and IQR only if at least 75% of the data in a month (23 days) is available
8.	Label them as daily_mean; daily_SD; daily_median; daily_IQR; monthly_mean; monthly_SD; monthly_median; monthly_IQR against date/month
9.	For each location, maintain one excel/CSV file for all parameters hourly values; another for all parameters daily values; another for all parameters monthly. Filename can be 'location_hourly/daily/monthly_startyear_endyear' (e.g. peenya_hourly _2009_2019)
