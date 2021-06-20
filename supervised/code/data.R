#### user input ----

start_date = "2011-09-01"
m_list = 100

raw_data_path = "../data/raw/" 
save_data_path = "../data/"
save_moving_window_path = "../data/moving_window/"

#### create dataset from Blockchain.com API ----

# typical financial asset data
market_price = read.csv(paste(raw_data_path, "market-price.csv", sep = ""), header = FALSE)
market_cap = read.csv(paste(raw_data_path, "market-cap.csv", sep = ""), header = FALSE)
n_transactions = read.csv(paste(raw_data_path, "n-transactions.csv", sep = ""), header = FALSE)

# Bitcoin specific data
estimated_transaction_volume = read.csv(paste(raw_data_path, "estimated-transaction-volume.csv", sep = ""), header = FALSE)
avg_block_size = read.csv(paste(raw_data_path, "avg-block-size.csv", sep = ""), header = FALSE)
difficulty = read.csv(paste(raw_data_path, "difficulty.csv", sep = ""), header = FALSE)
hash_rate = read.csv(paste(raw_data_path, "hash-rate.csv", sep = ""), header = FALSE)
miners_revenue = read.csv(paste(raw_data_path, "miners-revenue.csv", sep = ""), header = FALSE)

# convert date columns from string to date datatype
market_price = data.frame(date = as.Date(market_price$V1), market_price = market_price$V2)
market_cap = data.frame(date = as.Date(market_cap$V1), market_cap = market_cap$V2)
n_transactions = data.frame(date = as.Date(n_transactions$V1), n_transactions = n_transactions$V2)
estimated_transaction_volume = data.frame(date = as.Date(estimated_transaction_volume$V1), 
                                          estimated_transaction_volume = estimated_transaction_volume$V2)
avg_block_size = data.frame(date = as.Date(avg_block_size$V1), avg_block_size = avg_block_size$V2)
difficulty = data.frame(date = as.Date(difficulty$V1), difficulty = difficulty$V2)
hash_rate = data.frame(date = as.Date(hash_rate$V1), hash_rate = hash_rate$V2)
miners_revenue = data.frame(date = as.Date(miners_revenue$V1), miners_revenue = miners_revenue$V2)

# take starting market cap for each day
# initially, csv-file contains several entries for each day
market_cap = market_cap[match(unique(market_cap$date), market_cap$date),]

# take data after given start date
market_price = market_price[market_price$date >= start_date,]
market_cap = market_cap[market_cap$date >= start_date,]
n_transactions = n_transactions[n_transactions$date >= start_date,]
estimated_transaction_volume = estimated_transaction_volume[estimated_transaction_volume$date >= start_date,]
avg_block_size = avg_block_size[avg_block_size$date >= start_date,]
difficulty = difficulty[difficulty$date >= start_date,]
hash_rate = hash_rate[hash_rate$date >= start_date,]
miners_revenue = miners_revenue[miners_revenue$date >= start_date,]

# combine everything into one data.frame (and convert to reasonable units)
data = data.frame(date = market_price$date,
                  market_price = market_price$market_price * 10^-4,
                  market_cap = market_cap$market_cap * 10^-11,
                  n_transactions = n_transactions$n_transactions * 10^-5,
                  estimated_transaction_volume = estimated_transaction_volume$estimated_transaction_volume * 10^-6,
                  avg_block_size = avg_block_size$avg_block_size,
                  difficulty = difficulty$difficulty * 10^-12,
                  hash_rate = hash_rate$hash_rate * 10^-6,
                  miners_revenue = miners_revenue$miners_revenue * 10^-7)

# write data to csv-file
write.csv(x = data, file = paste(save_data_path, "data.csv", sep = ""), row.names = FALSE)

#### create moving window dataset from previous prepared dataset ----

# exclude some features
data$miners_revenue = NULL
data$market_cap = NULL
data$estimated_transaction_volume = NULL

# normalize time series with min-max-method
normalize <- function(x) {
  if (min(x) == max(x)) {
    x_norm = x * 0
  } else {
    x_norm = ((x- min(x)) /(max(x)-min(x)))
  }
  return(x_norm)
}

# generate a moving window dataset from original dataset
# m is window size
gen_moving_window <- function(data, m) {
  # get number of features and entries for each feature
  # as well as number of windows
  n_rows = dim(data)[1]
  n_cols = dim(data)[2] - 1 # subtract date column
  n_windows = n_rows - m + 1
  
  data$date = NULL
  
  # generate data.frame for window data with column names
  col_names = names(data)
  new_col_names = c()
  for (col_name in col_names) {
    for (i in seq(-m + 2, 0)) {
      new_col_names = c(new_col_names, paste(col_name, i, sep = ""))
    }
  }
  new_col_names = c(new_col_names, "label")
  new_data = data.frame()
  
  # loop over all windows
  for (i_window in seq(1, n_windows))
  {
    window_data = c()
    # loop over all features
    for (j in seq(1, n_cols)) {
      feat_data = data[seq(i_window, i_window + m - 2), j]
      feat_data = as.vector(t(feat_data))
      feat_data = normalize(feat_data)
      window_data = c(window_data, feat_data)
    }
    
    window_label = data$market_price[i_window + m - 1] > data$market_price[i_window + m - 2]
    
    # add current window data to new generated data
    window = c(window_data, window_label)
    new_data = rbind(new_data, window)
  }
  
  colnames(new_data) = new_col_names
  
  return(new_data)
}

for (m in m_list) {
  # generate window data
  win_data = gen_moving_window(data, m)
  
  # write data to csv-file
  write.csv(x = win_data, 
            file = paste(save_moving_window_path, paste("m=", m, ".csv", sep = ""), sep = ""), 
            row.names = FALSE)
}

