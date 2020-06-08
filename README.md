# Ambient Data Analysis for Mobile Monitoring and CPCB


Ambient data analysis- ILK Labs

1. BC ambient data Analysis with AE33 and AE51 (inside R folder!)
2. PM<sub>2.5</sub> ambient data analysis using BAM and DUstTrak II (inside R folder!)
3. CPCB data analysis

### Note

- _AE51 and DustTrak ran for every 30 minutes before and after the mobile monitoring ride starting from 14 May 2019 to 112 March 2020 at ILK Labs (near entrance door)._

- _While the BAM and AE33 were working from July 2019 to present all 24 hours at CSTEP._

### Folder Structure

State -> City -> Station

eg: Karnataka->Bengaluru->Bapuji Nagar

| ![\Uncorrected{fig: `DT8530_PM2.5`}](Image1.JPG) | 
|:--:| 
| *Folder Structure* |

### CPCB data cleaning process

1.	Uses one-hour datasets (xlsx format, no restriction on number of parameters downloaded).
2.	Arranges all the one-hour data without any gaps, missing values impute with 'NaN'
3.	Deletes all negatives
4.	For PM<sub>2.5</sub>  and PM<sub>10</sub>  - deletes values greater than or equal to 985
5.	If PM<sub>10</sub>  is available, deletes both PM<sub>2.5</sub>  and PM<sub>10</sub>  only when PM<sub>2.5</sub>  exceeds PM<sub>10</sub> 
7. If both PM<sub>2.5</sub> and PM<sub>10</sub> is available then calculates the ratio and names the column as `ratio`.
6.	If two consecutive values are the same, removes both of them
7.	On daily basis (24 data points), calculates the mean and SD, removes individual hourly values which don't fall in the interval [mean-3xSD  mean+3xSD]
8.	On the cleaned hourly dataset, if at least 75% (18 one-hour data points) of the data in a day is available, then computes the daily mean, SD, median and IQR
9.	From daily means, computes the monthly means, SD, median and IQR only if at least 75% of the data in a month (23 days) is available
10.For each parameter generates calendar plot for all parameters.
11.	For each location, maintains three csv file for all parameters hourly values; another for all parameters daily values; another for all parameters monthly. Filename can be 'location_hourly/daily/monthly.csv' (e.g. peenya_hourly.csv)
12. If multiple files exist for a particular Station then provides a single file for hourly , daily and monthly values, provided the parameters/pollutants are same in all the files for that particular station.
13. Example data set for trial is provided in the data folder (downloaded from CPCB).

#### Expected Output

| ![\Uncorrected{fig: `DT8530_PM2.5`}](Image2.JPG) | 
|:--:| 
| *Expected Output* |
