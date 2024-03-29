library(ggplot2)
library(ggwordcloud)
library(dendextend)
library(gridExtra)
library(qdapRegex)
library(textclean)
library(tm)
library(urltools)
library(stringr)
library(dplyr, quietly = TRUE)
library(RColorBrewer)

#### user input ----

file_path = "../data/twitter_musk.csv"
write_path = "../data/tweets_clean.csv"
years = seq(2010, 2020)

urls_min_freq = 6
ats_min_freq = 75
tweets_min_freq = 150

rem_spars = 0.995

K_list = seq(1, 20)
K_best = 8
words_per_cluster = 10

h_clusters = 15

#### load dataset ----

# load raw data
raw_data = read.csv(file_path, colClasses=c("urls" = "character",
                                            "photos" = "character",
                                            "video" = "character",
                                            "tweet" = "character"))
print(dim(raw_data))

# extract relevant information
tweets = raw_data$tweet
dates = as.Date(raw_data$date)
hours = raw_data$hour
urls = raw_data$urls
photos = raw_data$photos
videos = raw_data$video
nlikes = raw_data$nlikes
nreplies = raw_data$nreplies
nretweets = raw_data$nretweets
replies = raw_data$reply_to

#### first insights ----

# plot tweets per year
dates_year = substring(dates, 1, 4)
ntweets_year = c()
for (i in years) {
  ntweets_year = c(ntweets_year, sum(dates_year == i))
}
ntweets_year = data.frame(year = years, ntweets_year)
ggplot(ntweets_year, mapping = aes(years, ntweets_year)) + geom_bar(stat = "identity") +
       xlab("year") + ylab("# Tweets") +
       scale_x_continuous(breaks = round(years, 1))

# plot number of tweets per hour of day in PT (California)
hours = hours - 7
convert_hours = c(23, 22, 21, 20, 19, 18, 17)
for (i in seq(1, 7)) {
  hours[hours == -i] =  convert_hours[i]
}

ggplot(mapping = aes(x = hours)) + geom_bar() +
  xlab("hour of day (PDT)") + ylab("# Tweets") + scale_x_continuous(breaks = round(seq(0, 23), 1))

# plot average number of likes, replies, and retweets per tweet per year
nlikes_year = c()
nlikes_year_std = c()
nreplies_year = c()
nreplies_year_std = c()
nretweets_year = c()
nretweets_year_std = c()
for (i in years) {
  nlikes_year = c(nlikes_year, mean(nlikes[dates_year == i]))
  nlikes_year_std = c(nlikes_year_std, sd(nlikes[dates_year == i]) / sqrt(sum(dates_year == i)))
  nreplies_year = c(nreplies_year, mean(nreplies[dates_year == i]))
  nreplies_year_std = c(nreplies_year_std, sd(nreplies[dates_year == i]) / sqrt(sum(dates_year == i)))
  nretweets_year = c(nretweets_year, mean(nretweets[dates_year == i]))
  nretweets_year_std = c(nretweets_year_std, sd(nretweets[dates_year == i]) / sqrt(sum(dates_year == i)))
}
plot_nlikes_year = ggplot(mapping = aes(years, nlikes_year)) + geom_line() +
  geom_ribbon(mapping = aes(ymin = nlikes_year - nlikes_year_std, ymax = nlikes_year + nlikes_year_std), alpha=0.3) +
  xlab("year") + ylab("avg. # likes") +
  scale_x_continuous(breaks = round(years, 1))
plot_nreplies_year = ggplot(mapping = aes(years, nreplies_year)) + geom_line() +
  geom_ribbon(mapping = aes(ymin = nreplies_year - nreplies_year_std, ymax = nreplies_year + nreplies_year_std), alpha=0.3) +
  xlab("year") + ylab("avg. # replies") +
  scale_x_continuous(breaks = round(years, 1))
plot_nretweets_year = ggplot(mapping = aes(years, nretweets_year)) + geom_line() +
  geom_ribbon(mapping = aes(ymin = nretweets_year - nretweets_year_std, ymax = nretweets_year + nretweets_year_std), alpha=0.3) +
  xlab("year") + ylab("avg. # Retweets") +
  scale_x_continuous(breaks = round(years, 1))
grid.arrange(plot_nlikes_year, 
             plot_nreplies_year,
             plot_nretweets_year,
             ncol=1)

# compute fraction of tweets that contains urls, photos, and videos
urls_frac = mean(urls != "[]")
photos_frac = mean(photos != "[]")
videos_frac = mean(videos == 1)
print(urls_frac)
print(photos_frac)
print(videos_frac)

# compute fraction of tweets that are replies to other Twitter users
replies_frac = mean(replies != "[]")
print(replies_frac)#

#### analysis of mentioned urls ----

# drop tweets with no urls
drop_ind = which(urls == "[]")
urls_remain = urls[-drop_ind]

urls_remain = sub(".", "", urls_remain) # drop first "["
urls_remain = substr(urls_remain, 1, nchar(urls_remain) - 1) # drop last "]"

# consider tweets with one than one url
urls_remain = strsplit(urls_remain, split = ", ")
urls_remain = unlist(urls_remain)

# get domain names
urls_remain = domain(urls_remain)
urls_remain = data.frame(sort(table(urls_remain), decreasing = TRUE))
urls_remain = urls_remain[urls_remain$Freq > urls_min_freq, ]

# plot domain bar plot for most common urls
ggplot(urls_remain, aes(urls_remain, Freq)) + geom_bar(stat = "identity") +
  xlab("domain name") + ylab("frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  scale_y_continuous(breaks = seq(0, round(max(urls_remain$Freq) + 10, 10), 10))

#### analysis @ (ats, referals to other Twitter users) ----

ats = unlist(str_extract_all(tweets, "(?<=^|\\s)@[^\\s]+")) # get users
at_users = ats

at_users = data.frame(sort(table(at_users), decreasing = TRUE))
at_users = at_users[at_users$Freq > ats_min_freq, ]

# plot @user bar plot for most common users
ggplot(at_users, aes(at_users, Freq)) + geom_bar(stat = "identity") +
  xlab("@user") + ylab("frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  scale_y_continuous(breaks = seq(0, round(max(at_users$Freq) + 25, 25), 25))

#### clean tweets ----

tweets_clean = rm_url(tweets) # remove urls from tweet texts
tweets_clean = gsub("@\\w+", "", tweets_clean) # remove @user
tweets_clean = tolower(tweets_clean) # transform to lower case
tweets_clean = gsub("’", "'", tweets_clean)
tweets_clean = gsub("“", "\"", tweets_clean)
tweets_clean = gsub("”", "\"", tweets_clean)
tweets_clean = gsub("…", "", tweets_clean)
tweets_clean = gsub("—", "", tweets_clean)
tweets_clean = gsub("amp", "", tweets_clean) # comes from usage of "&" sign
tweets_clean = removeWords(tweets_clean, stopwords("en")) # remove some standard expressions
tweets_clean = removeWords(tweets_clean, stopwords("SMART")) # remove some standard expressions
tweets_clean = gsub("[[:punct:][:blank:]]+", " ", tweets_clean, perl = TRUE) # remove punctuation
tweets_clean = gsub("\\b\\d+\\b", "", tweets_clean) # remove numbers
tweets_clean = str_replace(gsub("\\s+", " ", str_trim(tweets_clean)), "B", "b") # remove trailing spaces and multiple spaces

write.csv(data.frame(tweets_clean), write_path)

#### plot barplot of most common words ----

# get table of all words ever tweeted
tweets_clean_table = strsplit(tweets_clean, " ")
tweets_clean_table = unlist(tweets_clean_table)
tweets_clean_table = data.frame(sort(table(tweets_clean_table), decreasing = TRUE))
print(dim(tweets_clean_table))
tweets_clean_table = tweets_clean_table[tweets_clean_table$Freq > tweets_min_freq, ]

# plot bar plot for most common words
ggplot(tweets_clean_table, aes(tweets_clean_table, Freq)) + geom_bar(stat = "identity") +
  xlab("word") + ylab("frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  scale_y_continuous(breaks = seq(0, round(max(tweets_clean_table$Freq) + 100, 100), 100))

#### convert tweets into document-term matrix ----

tweets_corpus = Corpus(VectorSource(tweets_clean))
tweets_matrix = DocumentTermMatrix(tweets_corpus)
tweets_matrix = removeSparseTerms(tweets_matrix, rem_spars)
tweets_matrix = as.matrix(tweets_matrix)

#### K-means clustering ----

# find optimal number of clusters (using elbow method)
tot_wss = c()
for (K in K_list) {
  set.seed(1)
  K_means_fit = kmeans(tweets_matrix, K)
  tot_wss = c(tot_wss, K_means_fit$tot.withinss)
}
ggplot(mapping = aes(K_list, tot_wss)) + geom_line() + 
  xlab("K") + ylab("G") +
  geom_vline(xintercept = K_best, color = "red", linetype = "dashed", size = 1)

# perform K-means clustering for best K
set.seed(1)
K_means_fit = kmeans(tweets_matrix, K_best)
print(K_means_fit$size)

# plot histogram for each cluster
plist = lapply(seq(1, K_best), function(i) {
  tweets_matrix_sub = tweets_matrix[K_means_fit$cluster == i, ]
  tweets_matrix_sub = sort(colSums(tweets_matrix_sub), decreasing = TRUE)
  tweets_matrix_sub = tweets_matrix_sub[seq(1, words_per_cluster)]
  names = names(tweets_matrix_sub)
  count = unname(tweets_matrix_sub)
  tweets_matrix_sub = data.frame(word = names, count)
  a = ifelse(max(count) > 500, 150, 50)
  ggplot(tweets_matrix_sub) + geom_bar(mapping = aes(reorder(names, -count), count), stat = "identity") +
    xlab("word") + ylab("frequency") +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    scale_y_continuous(breaks = seq(0, round(max(count) + a, a), a)) +
    ggtitle(paste("cluster ", i))
})
do.call(grid.arrange, c(plist, ncol = 2))

#### hierarchical clustering ----

# create color vector
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))[-4]

# plot dendogram
dist_matrix = dist(t(tweets_matrix), method = "binary")
h_fit = hclust(dist_matrix, method = "ward.D2")
h_fit = as.dendrogram(h_fit)
h_fit = color_labels(h_fit, k = h_clusters, col = col_vector)
h_fit = color_branches(h_fit, k = h_clusters, col = col_vector)
plot(h_fit)
groups = rect.dendrogram(h_fit, h_clusters)

# plot wordcloud of each cluster in dendogram
plist = lapply(seq(1, h_clusters), function(i) {
  words = groups[[i]]
  words = data.frame(words = names(words))
  words <- words %>%
    mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))
  ggplot(words, aes(label = words, angle = angle, size=0.5)) +
    geom_text_wordcloud(colour = col_vector[i]) +
    ggtitle(paste("cluster ", i))
})
grid.arrange(grobs = plist, ncol = 3)
