#### include packages ----
library(ggplot2)

#### user input ----

data_path = "../data/data.csv"

#### load Bitcoin dataset ----

data = read.csv(data_path)
head(data)
