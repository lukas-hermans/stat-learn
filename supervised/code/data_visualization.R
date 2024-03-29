# ---- data_visualization

#### include packages ----
library(ggplot2)
library(gridExtra)
library(corrplot)

#### user input ----

data_path = "../data/data.csv"
figure_path = "figures/"

lag_max = 300 # for autocorrelation plot

#### load Bitcoin dataset ----

data = read.csv(data_path)
data$date = as.Date(data$date)

#### visualize dataset ----

# plot time series in one large panel
text = theme(text = element_text(size=6))
plot_market_price = ggplot(data, mapping = aes(date, market_price, group = 1)) +
                    geom_line() + text
plot_market_cap = ggplot(data, mapping = aes(date, market_cap, group = 1)) + 
                  geom_line() + ylab("market cap [10¹¹ USD]") + text
plot_n_transactions = ggplot(data, mapping = aes(date, n_transactions, group = 1)) + 
                      geom_line() + ylab("# transactions [10⁵]") + text
plot_estimated_transaction_volume  = ggplot(data, mapping = aes(date, estimated_transaction_volume , group = 1)) + 
                                     geom_line() + ylab("transaction vol. [10⁶ BTC]") + text
plot_avg_block_size = ggplot(data, mapping = aes(date, avg_block_size, group = 1)) + 
                      geom_line() + ylab("avg. block size [MB]") + text
plot_difficulty = ggplot(data, mapping = aes(date, difficulty, group = 1)) + 
                  geom_line() + ylab("difficulty [rel.]") + text
plot_hash_rate = ggplot(data, mapping = aes(date, hash_rate, group = 1)) + 
                 geom_line() + ylab("hash rate [EH/s]") + text
plot_miners_revenue = ggplot(data, mapping = aes(date, miners_revenue, group = 1)) + 
  geom_line() + ylab("miner's revenue [10⁷ USD]") + text

grid.arrange(plot_market_price, 
             plot_market_cap,
             plot_n_transactions,
             plot_estimated_transaction_volume,
             plot_avg_block_size,
             plot_difficulty,
             plot_hash_rate,
             plot_miners_revenue,
             ncol=2)

# plot correlation matrix for all variables
cor_data = data
cor_data$date = NULL
colnames(cor_data) = c("market price",
                  "market cap",
                  "# transactions",
                  "transaction vol.",
                  "avg. block size",
                  "difficulty",
                  "hash rate",
                  "miner's revenue")
cor = cor(cor_data)
corrplot(cor, type="upper", 
         addCoef.col = "black",
         col=colorRampPalette(c("blue","white","red"))(200),
         tl.col="black", tl.srt=45,
         diag=FALSE)

# plot autocorrelation of Bitcoin market price
autocor = acf(data$market_price, plot = FALSE, lag.max = lag_max)
ggplot(mapping = aes(autocor$lag, autocor$acf)) + geom_line() +
  xlab("lag [days]") + ylab("autocorrelation")

# exclude some features
data$miners_revenue = NULL
data$market_cap = NULL
data$estimated_transaction_volume = NULL
data$difficulty = NULL

# plot all vs. all
data$date = NULL
ggpairs(data, columnLabels = c("market price [10⁴ USD]", "# transactions [10⁵]", 
                               "avg. block size [MB]", "hash rate [EH/s]")) 
