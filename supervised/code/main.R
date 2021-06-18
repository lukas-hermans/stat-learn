#### user input ----

data_path = "../data/data.csv"
figure_path = "figures/"
m = 10

#### load Bitcoin dataset ----

data = read.csv(data_path)
data$date = as.Date(data$date)

#### write function to generate moving window classification data ----

normalize <- function(x) {
  if (min(x) == max(x)) {
    x_norm = x * 0
  } else {
    x_norm = ((x- min(x)) /(max(x)-min(x)))
  }
  return(x_norm)
}

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
    for (i in seq(-m + 1, -1)) {
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

# generate window data
wnd_data = gen_moving_window(data, m)


