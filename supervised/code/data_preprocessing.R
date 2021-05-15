n_series = 100 # number of time series to extract

data = read.csv("../data/bitstampUSD_1-min_data_2012-01-01_to_2021-03-31.csv")

plot(data[, "Open"])

for (i in 1:n_series) {
  series_i = data[]
}