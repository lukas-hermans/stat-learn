import pandas as pd
import tensorflow as tf
import numpy as np
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
from tensorflow import keras
from tensorflow.keras import layers
import matplotlib.pyplot as plt
import sklearn.metrics

# set random number generator seeds ---
np.random.seed(1)
tf.random.set_seed(1)

# user input ----

moving_window_path = "../data/m=100.csv"
m_list = np.arange(2, 101, 1)

train_frac = 0.8

batch_size = 32
n_epochs = 20

# load & prepare data ----

pandas2ri.activate()

r = robjects.r
r['source']('r_from_python.R')

data_split_r = robjects.globalenv['data_split']

data_r = data_split_r(moving_window_path, train_frac)
data = pandas2ri.rpy2py(data_r)

data_train = np.array(pd.DataFrame(data[0])).astype(np.float32)
features_train = data_train[:, :-1]
label_train = data_train[:, -1]

data_test = np.array(pd.DataFrame(data[1])).astype(np.float32)
features_test = data_test[:, :-1]
label_test = data_test[:, -1]

# same as in R
def data_up_to_m(data, m, m_file):
    drop_num = m_file - m

    # collect indices that should be dropped
    # i.e. drop past values
    drop_cols = []
    for i in range(drop_num):
        drop_cols.append(i)
        drop_cols.append(i + m_file - 1)
        drop_cols.append(i + 2*m_file - 2)
        drop_cols.append(i + 3*m_file - 3)
    
    return np.delete(data, drop_cols, axis = 1)

# neural network ----

# get training and validation indexes w.r.t. training data
ind_train = np.arange(0, int(len(data_train) * 0.75))  
ind_val = np.arange(int(len(data_train) * 0.75), len(data_train))  

acc = []
acc_train = []
n_epochs_list = []

# loop over all window sizes m in m_list
for m in m_list:
    
    print(m)
    
    # get data for current m
    data_train_m = data_up_to_m(data_train, m, max(m_list))
    features_train_m = data_train_m[:, :-2]
    label_train_m = data_train_m[:, -1]
    data_test_m = data_up_to_m(data_test, m, max(m_list))
    features_test_m = data_test_m[:, :-2]
    label_test_m = data_test_m[:, -1]
    
    # one-hot encoding of training and validation labels
    label_train_m_hot = tf.reshape(tf.one_hot(label_train_m[ind_train], 2), shape = [len(label_train_m[ind_train]), 2])
    label_val_m_hot = tf.reshape(tf.one_hot(label_train_m[ind_val], 2), shape = [len(label_train_m[ind_val]), 2])

    # neural network structure
    model = keras.Sequential()
    model.add(layers.InputLayer(input_shape=np.shape(features_train_m)[1]))
    model.add(layers.Dense(20, activation="relu"))
    model.add(layers.Dropout(0.5))
    model.add(layers.Dense(30, activation="relu"))
    model.add(layers.Dropout(0.5))
    model.add(layers.Dense(20, activation="relu"))
    model.add(layers.Dropout(0.5))
    model.add(layers.Dense(10, activation="relu"))
    model.add(layers.Dropout(0.5))
    model.add(layers.Dense(2, activation="softmax"))

    # compile model
    model.compile(optimizer="adam", loss='categorical_crossentropy',
                  metrics=['accuracy'])

    # fit model
    history = model.fit(features_train_m[ind_train, :],
                        label_train_m_hot,
                        shuffle=True,
                        epochs=n_epochs,
                        batch_size=batch_size,
                        validation_data=(features_train_m[ind_val, :], label_val_m_hot),
                        verbose=2)

    n_epochs_best = np.argmax(history.history["val_accuracy"]) + 1
    acc.append(max(history.history["val_accuracy"]))
    acc_train.append(history.history["accuracy"][n_epochs_best - 1])
    n_epochs_list.append(n_epochs_best)

# plot results ----

# plot accuracy
plt.figure(figsize=(5, 3))
plt.plot(m_list, acc_train, label="training", color="salmon")
plt.plot(m_list, acc, label="validation", color="darkturquoise")
#plt.vlines(m_max, , ymax, kwargs)
plt.xlabel(r"$m$")
plt.ylabel("accuracy")
plt.legend()
plt.grid()
plt.tight_layout()

# plot N_{epoch, max} for every m 
plt.figure(figsize=(5, 3))
plt.plot(m_list, n_epochs_list, color="k")
plt.xlabel(r"$m$")
plt.ylabel(r"$N_{epoch, max}$")
plt.grid()
plt.tight_layout()

# find best m (= m_max)
m_max = m_list[np.argmax(acc)]
n_epochs = n_epochs_list[np.argmax(acc)]

# retrain model for best m_max on whole training set
# get data for m_max
data_train_m_max = data_up_to_m(data_train, m_max, max(m_list))
features_train_m_max = data_train_m_max[:, :-2]
label_train_m_max = data_train_m_max[:, -1]

# one-hot encoding of training and validation labels
label_train_m_max_hot = tf.reshape(tf.one_hot(label_train_m_max, 2), shape = [len(label_train_m_max), 2])

# neural network structure
model = keras.Sequential()
model.add(layers.InputLayer(input_shape=np.shape(features_train_m_max)[1]))
model.add(layers.Dense(20, activation="relu"))
model.add(layers.Dropout(0.5))
model.add(layers.Dense(30, activation="relu"))
model.add(layers.Dropout(0.5))
model.add(layers.Dense(20, activation="relu"))
model.add(layers.Dropout(0.5))
model.add(layers.Dense(10, activation="relu"))
model.add(layers.Dropout(0.5))
model.add(layers.Dense(2, activation="softmax"))

# compile model
model.compile(optimizer="adam", loss='categorical_crossentropy',
              metrics=['accuracy'])

# fit model
history = model.fit(features_train_m_max,
                    label_train_m_max_hot,
                    shuffle=True,
                    epochs=n_epochs,
                    batch_size=batch_size,
                    verbose=2)

# compute confusion matrix on test set 
data_test_m_max = data_up_to_m(data_test, m_max, max(m_list))
features_test_m_max = data_test_m_max[:, :-2]
label_test_m_max = data_test_m_max[:, -1]
label_test_m_max_hot = tf.reshape(tf.one_hot(label_test_m_max, 2), shape = [len(label_test_m_max), 2])

y_pred = model.predict(features_test_m_max)
confusion_matrix = sklearn.metrics.multilabel_confusion_matrix(label_test_m_max_hot, np.rint(y_pred))
print(confusion_matrix)

# retrain to plot acc. as a function of N_epoch
n_epochs = 20
# get data for current m
data_train_m_max = data_up_to_m(data_train, m_max, max(m_list))
features_train_m_max = data_train_m_max[:, :-2]
label_train_m_max = data_train_m_max[:, -1]
data_test_m_max = data_up_to_m(data_test, m_max, max(m_list))
features_test_m_max = data_test_m_max[:, :-2]
label_test_m_max = data_test_m_max[:, -1]

# one-hot encoding of training and validation labels
label_train_m_max_hot = tf.reshape(tf.one_hot(label_train_m_max[ind_train], 2), shape = [len(label_train_m_max[ind_train]), 2])
label_val_m_max_hot = tf.reshape(tf.one_hot(label_train_m_max[ind_val], 2), shape = [len(label_train_m_max[ind_val]), 2])

# neural network structure
model = keras.Sequential()
model.add(layers.InputLayer(input_shape=np.shape(features_train_m_max)[1]))
model.add(layers.Dense(20, activation="relu"))
model.add(layers.Dropout(0.5))
model.add(layers.Dense(30, activation="relu"))
model.add(layers.Dropout(0.5))
model.add(layers.Dense(20, activation="relu"))
model.add(layers.Dropout(0.5))
model.add(layers.Dense(10, activation="relu"))
model.add(layers.Dropout(0.5))
model.add(layers.Dense(2, activation="softmax"))

# compile model
model.compile(optimizer="adam", loss='categorical_crossentropy',
              metrics=['accuracy'])

# fit model
history = model.fit(features_train_m_max[ind_train, :],
                    label_train_m_max_hot,
                    shuffle=True,
                    epochs=n_epochs,
                    batch_size=batch_size,
                    validation_data=(features_train_m_max[ind_val, :], label_val_m_max_hot),
                    verbose=2)

plt.figure(figsize=(5, 3))
plt.plot(np.arange(1, n_epochs + 1), history.history["accuracy"], label="training", color="salmon")
plt.plot(np.arange(1, n_epochs + 1), history.history["val_accuracy"], label="validation", color="darkturquoise")
plt.xlabel(r"$N_{epoch}$")
plt.ylabel("accuracy")
plt.legend()
plt.grid()
plt.tight_layout()