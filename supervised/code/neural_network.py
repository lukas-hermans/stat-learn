import pandas as pd
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers

#### user input ----

moving_window_path = "../data/moving_window/"

data = pd.read_csv(moving_window_path + "m=20.csv")

#### neural network ----

model = keras.Sequential(
    [
        layers.Dense(2, activation="relu"),
        layers.Dense(3, activation="relu"),
        layers.Dense(2, activation="softmax"),
    ]
)