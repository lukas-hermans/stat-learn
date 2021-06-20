library(caret)

#### user input ----

set.seed(1)

m_list = seq(5, 100)
moving_window_path = "../data/moving_window/m=100.csv"

frac_train = 0.8
n_folds = 10

#### function for training and test set split ----

data_split = function(data, frac_train){
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

data_up_to_m = function(data, m, m_file){
  drop_num = m_file - m
  
  # collect indices that should be dropped
  # i.e. drop past values
  drop_cols = c()
  for (i in seq(1, drop_num)) {
    drop_cols = c(drop_cols, i, 
                  i + m_file - 1,
                  i + 2*m_file - 2,
                  i + 3*m_file - 3,
                  i + 4*m_file - 4)
  }
  
  return(data[, -drop_cols])
}

#### load data set ----

# load data set
data = read.csv(moving_window_path)
data$label = as.factor(data$label)

# divide into training and test set
data = data_split(data, frac_train)
data_train = data[[1]]
data_test = data[[2]]
label_test = data_test[, length(data_test)]

#### fit different models ----

acc_major = c()
acc_major_test = c()

for (m in m_list) {

  data_train_m = data_up_to_m(data_train, m, max(m_list))
  data_test_m = data_up_to_m(data_test, m, max(m_list))
  
  ### majority predictor
  
  folds <- createFolds(data_train_m[, ncol(data_train_m)], k = n_folds, 
                      list = TRUE, returnTrain = FALSE)
  acc = 0
  for (i in seq(1, n_folds)) {
    # get training and validation labels of current fold
    label_train = data_train_m[-folds[[i]], ncol(data_train_m)]
    label_val = data_train_m[folds[[i]], ncol(data_train_m)]

    # find majority predictor
    ll = data.frame(table(label_train))
    major_pred = ll[which.max(ll$Freq),]$label_train
    
    # compute accuracy
    acc = acc + sum(label_val == major_pred) / length(label_val)
  }
  acc = acc / n_folds
  acc_major = c(acc_major, acc)
  
  # find majority predictor on whole training set
  label_train = data_train_m$label
  ll = data.frame(table(label_train))
  major_pred = ll[which.max(ll$Freq),]$label_train
  
  acc_major_test = c(acc_major_test, sum(label_test == major_pred) / length(label_test))
  
  ### logistic regression
  
}

#### plot results ----

### majority predictor

# plot CV accuracy
ggplot(mapping = aes(m_list, acc_major*100)) + geom_line() + ylim(53.5, 54.5) +
        xlab("m") + ylab("10-fold CV accuracy [%]")

# find best m
m_max = which.max(acc_major_test)
print(m_max)
print(max(acc_major_test))

# retrain to get best predictor for m_max
label_train = data_train$label
ll = data.frame(table(label_train))
major_pred = ll[which.max(ll$Freq),]$label_train

