library(caret)
library(ggplot2)
library(reshape2)
library(regclass)
library(class)
library(base)
library(pROC)

#### user input ----

set.seed(1)

m_list = seq(2, 100)
moving_window_path = "../data/m=100.csv"

frac_train = 0.8
n_folds = 10

K_list = seq(1, 31, 2) # for KNN

#### function for training and test set split ----

data_split = function(moving_window_path, frac_train){
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

data_up_to_m = function(data, m, m_file){
  drop_num = m_file - m
  
  # collect indices that should be dropped
  # i.e. drop past values
  drop_cols = c()
  for (i in seq(1, drop_num)) {
    drop_cols = c(drop_cols, i, 
                  i + m_file - 1,
                  i + 2*m_file - 2,
                  i + 3*m_file - 3)
  }
  
  return(data[, -drop_cols])
}

#### load and plot data set ----

# divide into training and test set
data = data_split(moving_window_path, frac_train)
data_train = data[[1]]
data_test = data[[2]]
label_test = data_test[, length(data_test)]

# create balance plot of training and test set
ggplot(data_train, aes(label)) + geom_bar(aes(fill = label)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, "%")),
                stat = "count",
                position = position_dodge(.1),
                vjust = 1.7) +  
                theme(text = element_text(size=15))
ggplot(data_test, aes(label)) + geom_bar(aes(fill = label)) +
  geom_text(aes(y = ..count.., 
                label = paste0(round(prop.table(..count..),4) * 100, "%")),
            stat = "count",
            position = position_dodge(.1),
            vjust = 1.7) +  
            theme(text = element_text(size=15))


#### fit different models ----

# lists for accuracy storage
acc_major = c()
acc_major_train = c()
acc_major_test = c()
acc_log = c()
acc_log_train = c()
acc_log_test = c()
acc_knn = c()
acc_knn_train = c()
acc_knn_test = c()

K_hist = c()

# folds for CV
folds <- createFolds(data_train[, ncol(data_train)], k = n_folds, 
                     list = TRUE, returnTrain = FALSE)

# loop over all window sizes m in m_list
for (m in m_list) {
  
  print(m)

  data_train_m = data_up_to_m(data_train, m, max(m_list))
  features_train_m = data_train_m[, -ncol(data_train_m)]
  label_train_m = data_train_m$label
  data_test_m = data_up_to_m(data_test, m, max(m_list))
  features_test_m = data_test_m[, -ncol(data_test_m)]
  
  ### majority predictor
  
  acc = 0
  for (i in seq(1, n_folds)) {
    # get training and validation labels of current fold
    label_train = data_train_m[-folds[[i]], ncol(data_train_m)]
    label_val = data_train_m[folds[[i]], ncol(data_train_m)]

    # find majority predictor
    ll = data.frame(table(label_train))
    major_pred = ll[which.max(ll$Freq),]$label_train
    
    # compute accuracy on validation set
    acc = acc + sum(label_val == major_pred) / length(label_val)
  }
  acc = acc / n_folds
  acc_major = c(acc_major, acc)
  
  # find majority predictor on whole training set
  ll = data.frame(table(label_train_m))
  major_pred = ll[which.max(ll$Freq),]$label_train_m
  
  # training accuracy
  acc_major_train = c(acc_major_train, sum(label_train_m == major_pred) / length(label_train_m))
  
  # test accuracy
  acc_major_test = c(acc_major_test, sum(label_test == major_pred) / length(label_test))
  
  ### logistic regression
  
  acc = 0
  for (i in seq(1, n_folds)) {
    # get validation label of current fold
    label_val = data_train_m[folds[[i]], ncol(data_train_m)]
    
    # train logistic model on current fold
    log_pred = glm(label ~ ., data = data_train_m[-folds[[i]], ], family = binomial(link = "logit"))

    # compute accuracy on validation set
    temp_acc = predict(log_pred, data_train_m[folds[[i]], ], type = "response")
    temp_acc = as.factor(ifelse(temp_acc > 0.5, 1, 0))
    temp_acc = sum(temp_acc == label_val) / length(label_val)
  
    acc = acc + temp_acc
  }
  acc = acc / n_folds
  acc_log = c(acc_log, acc)
  
  # train logistic regressor on whole training set
  log_pred = glm(label ~ ., data = data_train_m, family = binomial(link = "logit"))
  
  # training accuracy
  temp_acc_log_train = predict(log_pred, data_train_m, type = "response")
  temp_acc_log_train = as.factor(ifelse(temp_acc_log_train > 0.5, 1, 0))
  acc_log_train = c(acc_log_train, sum(label_train_m == temp_acc_log_train) / length(label_train_m))
  
  # test accuracy
  temp_acc_log_test = predict(log_pred, data_test_m, type = "response")
  temp_acc_log_test = as.factor(ifelse(temp_acc_log_test > 0.5, 1, 0))
  acc_log_test = c(acc_log_test, sum(label_test == temp_acc_log_test) / length(label_test))
  
  ### KNN
  
  acc_K = c()
  for (K in K_list) {
    
    acc_K_temp = 0
    for (i in seq(1, n_folds)) {
      # get training and validation features and labels of current fold
      features_train = data_train_m[-folds[[i]], -ncol(data_train_m)]
      label_train = data_train_m[-folds[[i]], ncol(data_train_m)]
      features_val = data_train_m[folds[[i]], -ncol(data_train_m)]
      label_val = data_train_m[folds[[i]], ncol(data_train_m)]
      
      # train knn model on current fold
      knn_pred = knn(features_train, features_val, label_train, k = K)
      
      # compute accuracy
      acc_K_temp = acc_K_temp + mean(knn_pred == label_val)
    }
    acc_K_temp = acc_K_temp / n_folds
    acc_K = c(acc_K, acc_K_temp)
  }
  
  # find best K value
  K_max = K_list[which.max(acc_K)]
  acc_knn = c(acc_knn, max(acc_K))
  K_hist = c(K_hist, K_max)
  
  # compute training accuracy
  knn_pred = knn(data_train_m[, -ncol(data_train_m)], data_train_m[, -ncol(data_train_m)], data_train_m$label, k = K_max)
  acc_knn_train = c(acc_knn_train, mean(knn_pred == data_train_m$label))
  
  # compute test accuracy
  knn_pred = knn(data_train_m[, -length(data_train_m)], data_test_m[, -length(data_test_m)], data_train_m$label, k = K_max)
  acc_knn_test = c(acc_knn_test, mean(knn_pred == label_test))
}

#### plot results ----

### majority predictor

# plot training & CV accuracy
dat = data.frame(m_list, acc_major_train, acc_major)
dat = melt(dat, id.var = "m_list")
ggplot(dat, mapping = aes(x = m_list, y = value*100, colour = variable)) + geom_line() +
       xlab("m") + ylab("accuracy [%]") +
       coord_cartesian(ylim = c(53.5, 54.5)) +
       scale_color_discrete("type", labels = c("training", "10-fold CV")) +
       scale_x_continuous(breaks = seq(min(m_list), max(m_list), by = 14))

# find best m
m_max = which.max(acc_major) + min(m_list)
print(m_max)
print(acc_major_test[m_max - min(m_list)])

# retrain to get best predictor for m_max
ll = data.frame(table(label_train))
major_pred = ll[which.max(ll$Freq),]$label_train
major_pred = as.factor(major_pred)

# print confusion matrix
table(Predicted = rep(major_pred, length(label_test)), True = label_test)

### logistic regression

# find best m
m_max = which.max(acc_log) + min(m_list) - 1
print(m_max)
print(acc_log_test[m_max - min(m_list)])

# plot training & CV accuracy
dat = data.frame(m_list, acc_log_train, acc_log)
dat = melt(dat, id.var = "m_list")
ggplot(dat, mapping = aes(x = m_list, y = value*100, colour = variable)) + geom_line() +
  xlab("m") + ylab("accuracy [%]") +
  scale_color_discrete("type", labels = c("training", "10-fold CV")) +
  scale_x_continuous(breaks = seq(min(m_list), max(m_list), by = 14)) +
  geom_vline(xintercept = m_max, size = 0.5)

# retrain to get best predictor for m_max
data_train_m_max = data_up_to_m(data_train, m_max, max(m_list))
log_pred = glm(label ~ ., data = data_train_m_max, family = binomial(link = "logit"))
log_prediction = predict(log_pred, data_test, type = "response")
log_prediction = as.factor(ifelse(log_prediction > 0.5, 1, 0))

# print confusion matrix & ROC curve
table(Predicted = log_prediction, True = label_test)
log_prediction = predict(log_pred, data_test, type = "response")
roc_log = roc(label_test ~ log_prediction, print.auc = TRUE)
ggroc(roc_log) + geom_abline(intercept = 1, slope = 1,
                              linetype = "dashed")

# show coefficients & compute VIF
summary(log_pred)
sqrt(VIF(log_pred))

### KNN

# find best m
m_max = which.max(acc_knn) + min(m_list) - 1
print(m_max)
print(acc_knn_test[m_max - min(m_list)])

# plot training & CV accuracy
dat = data.frame(m_list, acc_knn_train, acc_knn)
dat = melt(dat, id.var = "m_list")
ggplot(dat, mapping = aes(x = m_list, y = value*100, colour = variable)) + geom_line() +
  xlab("m") + ylab("accuracy [%]") +
  scale_color_discrete("type", labels = c("training", "10-fold CV")) +
  scale_x_continuous(breaks = seq(min(m_list), max(m_list), by = 14)) +
  geom_vline(xintercept = m_max, size = 0.5)

# retrain to get best predictor for m_max
data_train_m_max = data_up_to_m(data_train, m_max, max(m_list))
acc_K = c()
acc_K_train = c()
for (K in K_list) {
  print(K)
  acc_K_temp = 0
  for (i in seq(1, n_folds)) {
    # get training and validation features and labels of current fold
    features_train = data_train_m_max[-folds[[i]], -ncol(data_train_m_max)]
    label_train = data_train_m_max[-folds[[i]], ncol(data_train_m_max)]
    features_val = data_train_m_max[folds[[i]], -ncol(data_train_m_max)]
    label_val = data_train_m_max[folds[[i]], ncol(data_train_m_max)]
    
    # train knn model on current fold
    knn_pred = knn(features_train, features_val, label_train, k = K)
    
    # compute accuracy
    acc_K_temp = acc_K_temp + mean(knn_pred == label_val)
  }
  acc_K_temp = acc_K_temp / n_folds
  acc_K = c(acc_K, acc_K_temp)
  
  # compute training accuracy for given K
  knn_pred = knn(features_train, features_train, label_train, k = K)
  acc_K_train = c(acc_K_train, mean(knn_pred == label_train))
}
# find best K value
K_max = K_list[which.max(acc_K)] 

# plot accuracy vs. K for m_max
dat = data.frame(K_list, acc_K_train, acc_K)
dat = melt(dat, id.var = "K_list")
ggplot(dat, mapping = aes(x = K_list, y = value*100, colour = variable)) + geom_line() +
  xlab("K") + ylab("accuracy [%]") +
  scale_color_discrete("type", labels = c("training", "10-fold CV")) +
  scale_x_continuous(breaks = seq(min(K_list), max(K_list), by = 4)) +
  geom_vline(xintercept = K_max, size = 0.5)

# print confusion matrix & ROC curve
data_train_m_max = data_up_to_m(data_train, m_max, max(m_list))
data_test_m_max = data_up_to_m(data_test, m_max, max(m_list))
knn_pred = knn(data_train_m_max[, -length(data_train_m_max)], 
               data_test_m_max[, -length(data_test_m_max)],
               data_train_m$label, k = K_max, prob = TRUE)
table(Predicted = knn_pred, True = label_test)
knn_pred = as.numeric(knn_pred)
roc_knn = roc(label_test ~ knn_pred, print.auc = TRUE)
ggroc(roc_knn) + geom_abline(intercept = 1, slope = 1,
                             linetype = "dashed")

# plot K for every m
dat = data.frame(m_list, K_hist)
ggplot(dat, aes(m_list, K_hist)) + geom_line() + 
  xlab("m") + ylab(expression("K"["max"]))
