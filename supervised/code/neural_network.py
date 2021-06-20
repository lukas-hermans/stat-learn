import pandas as pd
import tensorflow as tf
import numpy as np
from tensorflow import keras
from tensorflow.keras import layers

np.random.seed(1)
tf.random.set_seed(1)

# user input ----

moving_window_path = "../data/moving_window/"
m_list = [5]
train_frac = 0.8


for m in m_list:

    # load data set for window size m
    data = np.array(pd.read_csv(moving_window_path + "m=" + str(m) + ".csv"))
    # np.random.shuffle(data)

    # split data set into training, validation, and test set
    n_train = int(len(data) * train_frac)
    n_val = int(len(data) * (1 - train_frac) / 2)
    n_test = len(data) - n_train - n_val
    data_train = data[:n_train, :]
    data_val = data[n_train:n_train+n_val, :]
    data_test = data[n_train+n_val:n_train+n_val+n_test, :]

    # get features and labels (and apply one-hot encoding)
    features_train = data_train[:, :np.shape(data_train)[1]-1]
    label_train = tf.reshape(tf.one_hot(
        data_train[:, -1], 2), shape=[len(data_train), 2])
    features_val = data_val[:, :np.shape(data_val)[1]-1]
    label_val = tf.reshape(tf.one_hot(
        data_val[:, -1], 2), shape=[len(data_val), 2])
    features_test = data_test[:, :np.shape(data_test)[1]-1]
    label_test = tf.reshape(tf.one_hot(
        data_test[:, -1], 2), shape=[len(data_test), 2])

    # neural network structure
    model = keras.Sequential()
    model.add(layers.InputLayer(input_shape=(np.shape(data)[1]-1)))
    model.add(layers.Dense(128, activation="relu"))
    model.add(layers.Dense(128, activation="relu"))
    model.add(layers.Dense(64, activation="relu"))
    model.add(layers.Dense(2, activation="softmax"))

    # compile model
    model.compile(optimizer="adam", loss='categorical_crossentropy',
                  metrics=['accuracy'])

    # fit model
    history = model.fit(features_train,
                        label_train,
                        shuffle=True,
                        epochs=100,
                        batch_size=32,
                        validation_data=(features_val, label_val),
                        verbose=1)
