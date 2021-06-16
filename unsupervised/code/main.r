library(ggplot2)

#### load NYC taxi trip raw data ----

data_path = "../data/01_06_2019_NYC.csv"
raw_data = read.csv(paste(data_path, sep = ""))
n_rows = dim(raw_data)[1]

### select subset of variables (and compute some new ones) for further analysis ----
# dropped variables:
# VendorID (who created provided the trip record)
# details on how total amount to pay is computed

# compute variables pickup_time and dropoff_time in hours of day
library(lubridate)
date_fmt = "%m/%d/%Y %I:%M:%S %p"
time_origin = parse_date_time(raw_data$tpep_pickup_datetime[1], date_fmt)
pickup_time = as.numeric(difftime(parse_date_time(raw_data$tpep_pickup_datetime, date_fmt), time_origin), 
                         units = "hours") 
dropoff_time = as.numeric(difftime(parse_date_time(raw_data$tpep_dropoff_datetime, date_fmt), time_origin),
                          units = "hours")

# compute trip duration in minutes
trip_duration = (dropoff_time - pickup_time) * 60

# convert trip distance from miles to km
trip_distance = 1.609344 * raw_data$trip_distance

# take selected variables (that are not edited w.r.t to original dataset)
data = subset(raw_data, select = c(passenger_count, 
                                   PULocationID, 
                                   DOLocationID, 
                                   payment_type, 
                                   total_amount, 
                                   tip_amount))

# add new/edited variables
data = cbind(pickup_time, 
             trip_duration, 
             trip_distance, 
             data)

### visualize data to obtain first impressions ----

head(data)
summary(data)
