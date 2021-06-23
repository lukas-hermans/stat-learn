data_split = function(moving_window_path, frac_train){
  set.seed(1)
  data = read.csv(moving_window_path)
  data$label = as.factor(data$label)
  
  n_tot = dim(data)[1]
  n_train = ceiling(n_tot * frac_train)
  n_test = n_tot - n_train
  
  # shuffle rows of data to get random split
  rows <- sample(n_tot)
  data = data[rows,]
  
  data_train = data[seq(1, n_train),]
  data_test = data[seq(n_train + 1, n_train + n_test),]
  
  return(list(data_train, data_test))
}