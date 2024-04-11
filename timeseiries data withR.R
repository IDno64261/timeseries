setwd("C:/Users/LUJ/Downloads/")
idata <-read.csv("Busan_MSL_data_1961_2022_meanx.csv")


# install.packages("TTR")
library(TTR)



# setting data route
file_path <- "Busan_MSL_data_1961_2022_meanx.csv"

# loading csv
idata <- read.csv(file_path, header = TRUE)

# deleting head, scanning
scanned_data <- scan(file_path, what = numeric(), skip = 1, sep = ",", na.strings = "")

# printing
print(scanned_data)

plot(scanned_data)




##############################################

# data index (including year)
data_indices <- 1:length(scanned_data)

# calculating year data (interval 13)
year_indices <- seq(1, length(scanned_data), by = 13)

# calculating index without year data
data_without_years_indices <- setdiff(data_indices, year_indices)

# selecting data excluding year
data_without_years <- scanned_data[data_without_years_indices]

# printing date
print(data_without_years)

# plotting from changed data
plot(data_without_years)
lines(data_without_years)


#####################################

# Time series object from data

msl_ts <- ts(data_without_years, start=c(1961, 61.8), frequency=12)

# Decomposing
msl_decomposed <- decompose(msl_ts)


######################################
## install.packages("zoo")

library(zoo)

ts_data_filled <- na.locf(msl_ts)

plot(ts_data_filled)



########################################

component_msl <- decompose(ts_data_filled)
plot(component_msl)

# 
round(component_msl$seasonal)




######################## forecasting

# Loading Libraries
library(forecast)

# Fitting AR(1) to Model
fit <- Arima(ts_data_filled,
             order=c(1,0,0),
             seasonal = c(0, 0, 0))

# Original Plot in Red, Fitted in Blue
plot(fit$x,col="red")
lines(fitted(fit),col="blue")







######################################### install.packages("tseries")


library(tseries)

adf.test(ts_data_filled, alternative = "stationary")
kpss.test(ts_data_filled)

