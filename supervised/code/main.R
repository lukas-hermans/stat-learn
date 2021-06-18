set.seed(123)

#### user input ----

m_list = c(5, 10, 15, 20)
moving_window_path = "../data/moving_window/"

frac_train = 0.6

#### function for training, validation, and test set split ----

data_split = function(data, frac_train){
  n_tot = dim(data)[1]
  n_train = ceiling(n_tot * frac_train)
  n_val = ceiling((n_tot - n_train) / 2)
  n_test = n_tot - n_train - n_val
  
  # shuffle rows of data to get random split
  rows <- sample(n_tot)
  data = data[rows,]
  
  data_train = data[seq(1, n_train),]
  data_val = data[seq(n_train + 1, n_train + n_val),]
  data_test = data[seq(n_train + n_val + 1, n_train + n_val + n_test),]
  
  return(list(data_train, data_val, data_test))
}

comp_accuracy = function(model, data)
{
  label_pred = predict(model, data, type = "response")
  label_pred = ifelse(label_pred > 0.5, 1, 0)
  label_pred = as.factor(label_pred)
  
  return(sum(label_pred == data$label) / nrow(data))
}

### logistic regression ----
m=20
for (i in seq(1,10)) {
  data = read.csv(paste(moving_window_path, "m=", m, ".csv", sep = ""))
  data$label = as.factor(data$label)
  data = data_split(data, frac_train)
  
  data_train = data[[1]]
  data_val = data[[2]]
  
  model = glm(label ~ ., data = data_train, family = binomial(link = "logit"))
  
  print(comp_accuracy(model, data_train))
  print(comp_accuracy(model, data_val))
}


