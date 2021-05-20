# prepare Bitcoin data from Kaggle (https://www.kaggle.com/mczielinski/bitcoin-historical-data) ----

n_series = 100 # number of time series to extract

input_data = read.csv("../data/bitstampUSD_1-min_data_2012-01-01_to_2021-03-31.csv") # original

output_data = data.frame()

for (i in 1:n_series) {
  start_index = as.integer(runif(1, min=1, max=dim(data)[1]+1))
  start_time = data[start_index, "Timestamp"]
  sub_series = data[seq(start, start+100), "Open"]
  
  output[i, 1] = start_time
  output[i, seq(2, 102)] = series_i
}

write.csv(output, "../data/test.csv", quote=FALSE, row.names=FALSE)
