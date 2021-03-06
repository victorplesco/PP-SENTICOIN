splitter.unique <- function() {
  require(dplyr);
  
  # Loading tweets and assigning duplicates to most popular cryptocurrency;
  tweets = read.csv("./SENTICOIN/data/raw/twitter/@portfolio/csv/tweets.csv") %>%
    mutate(merged = paste0(tweet_id, "__", text));
  tweets = left_join(tweets, {tweets %>% group_by(coin) %>% summarize(popularity = n()) %>% ungroup()}, by = "coin") %>%
    select(merged, tweet_id, coin, text, created_at, popularity);
  
  # Ordering by "tweet_id + text" and "popularity" to delete duplicates of "tweet_id" and "text" tuple
  # and assign the tweet to the cryptocurrency having the most number of tweets, i.e., most popular.
  tweets = tweets[order(tweets$merged, -abs(tweets$popularity)),];
  tweets = tweets[!duplicated(tweets$merged),] %>% select(-merged);
  
  # Ordering by "text" and "popularity" to delete duplicates of "text" and assign the 
  # tweet to the cryptocurrency having the most number of tweets, i.e., most popular.
  tweets = tweets[order(tweets$text, -abs(tweets$popularity)),];
  tweets = tweets[!duplicated(tweets$text),] %>% select(-popularity);
  
  tweets$id = as.numeric(rownames(tweets)); tweets = tweets[, c("id", "tweet_id", "coin", "text", "created_at")]; 
  
  write.csv(tweets, "./SENTICOIN/data/processed/models/regression/loader/corpus/origin/splitter.unique_tweets.csv", row.names = FALSE);
};

splitter.get_data <- function() {
  tweets = read.csv("./SENTICOIN/data/processed/models/regression/loader/corpus/origin/splitter.unique_tweets.csv");
  return(tweets);
};

splitter <- function(coins = NULL, lag.hours = 1, test.split = 0.2, val.split = 0.1, poles = NULL, stratify.by_date = TRUE) {
  require(dplyr); require(lubridate); require(caTools);
  
  # Checker for train, test and validation split proportion;
  if(!isTRUE(test.split + val.split < 1)) {stop("Test and validation split sum to one or more!");};
  
  tweets = splitter.get_data();
  
  # Loading prices;
  prices = data.frame(); 
  if(length(coins) != 0) { # Merging prices for specified cryptocurrencies;
    if(all(coins %in% gsub("\\..*", "", list.files("./SENTICOIN/data/raw/crypto/@portfolio/")))) {
      
      # Subset tweets for specified cryptocurrencies;
      tweets = tweets[which(tweets$coin %in% coins),];
      for(i in coins) {temp = read.csv(paste0("./SENTICOIN/data/raw/crypto/@portfolio/", i, ".csv"));
        temp = temp %>% mutate(coin = i) %>% select(coin, time, open, close); 
        prices = rbind(prices, temp); rm(temp);
      };
      
    } else {stop("Listed coin(s) are wrong or doesn't have prices!")};
  } else { # Merging all prices;
    
    for(i in list.files("./SENTICOIN/data/raw/crypto/@portfolio/")) {
        temp = read.csv(paste0("./SENTICOIN/data/raw/crypto/@portfolio/", i));
        temp = temp %>% mutate(coin = gsub("\\..*", "", i)) %>% select(coin, time, open, close); 
        prices = rbind(prices, temp); rm(temp);
    }; rm(i);
  };
  
  # Labeling tweets with prices of a lag specified by "lag.hours";
  tweets$created_at = strptime(tweets$created_at, format = "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC");
  tweets$floor_time = as.character(floor_date(tweets$created_at, unit = "hour")); tweets$ceiling_time = ceiling_date(tweets$created_at, unit = "hour");
  tweets$lag_time   = as.character(format(as.POSIXct(tweets$created_at, format = "%Y-%m-%dT%H:%M:%S.000Z", tz = "UTC") + 60 * 60 * lag.hours, format = "%Y-%m-%d %H:00:00"));
  
  tweets = left_join(tweets, {colnames(prices)[which(colnames(prices) %in% c("open", "close"))] = c("open_floor", "close_floor"); prices}, 
                     by = c("coin" = "coin", "floor_time" = "time"));
  tweets = left_join(tweets, {colnames(prices)[which(colnames(prices) %in% "close_floor")] = c("close_lag"); prices[, c("coin", "time", "close_lag")]}, 
                     by = c("coin" = "coin", "lag_time" = "time"));
  tweets$predicted_price = as.numeric(difftime(tweets$ceiling_time, tweets$created_at, units = "mins")/60) * 
    (tweets$close_floor - tweets$open_floor) + tweets$open_floor;
  
  n.initial = nrow(tweets); tweets = na.omit(tweets);
  tweets$y = (tweets$close_lag - tweets$predicted_price)/tweets$predicted_price; 
  
  if(!is.null(poles)) {
    tweets$y[which(tweets$y_poled <= poles[1])] = poles[1];
    tweets$y[which(tweets$y_poled >= poles[2])] = poles[2];
    tweets = tweets %>% select(id, coin, text, created_at, tweet_id, y); 
  };  
  
  print(paste0("Lost ", n.initial - nrow(tweets), " tweets due to lag!")); rm(n.initial);
  
  # Defining thresholds for longitudinal splitting (by days) into train, test and validation set;
  split = strptime(c(as.character(min(tweets$created_at)), as.character(max(tweets$created_at))), format = "%Y-%m-%d %H:%M:%S", tz = "UTC");
  # Define the last timestamp, starting from the last day and counting in a decreasing order, for the validation set;
  val.min  = split[2] - as.numeric(difftime(split[2], split[1], units = "hours")) * val.split * 60 * 60;
  
  if(stratify.by_date == FALSE) {
    # Define the last timestamp, starting from the last day of the validation set and counting in a decreasing order, for the test tweets;
    test.min = val.min - as.numeric(difftime(split[2], split[1], units = "hours")) * test.split * 60 * 60; # The remaining are train set;
    train = tweets[which(tweets$created_at  < test.min & tweets$created_at >= split[1]), c("id", "y")];
    test  = tweets[which(tweets$created_at  < val.min  & tweets$created_at > test.min),  c("id", "y")];
    val   = tweets[which(tweets$created_at <= split[2] & tweets$created_at > val.min),   c("id", "y")];
    
    return(list(`train` = train, `test` = test, `val` = val));
  } else {
    split.rows = sample.split(as.factor(strptime(tweets$created_at[which(tweets$created_at < val.min & tweets$created_at >= split[1])], 
                                                 format = "%Y-%m-%d", tz = "UTC")),
                              SplitRatio = (1 - test.split));
    train = tweets[split.rows, c("id", "y"), drop = FALSE];
    test  = tweets[!split.rows, c("id", "y"), drop = FALSE];
    val   = tweets[which(tweets$created_at <= split[2] & tweets$created_at > val.min), c("id", "y"), drop = FALSE];
    
    return(list(`train` = train, `test` = test, `val` = val));
  };
}; 

# # lag.hours = 1, test.split = 0.2, val.split = 0.1, stratify.by_date = TRUE
# split <- splitter(coins = NULL, lag.hours = 1, test.split = 0.2, val.split = 0.1, stratify.by_date = TRUE);
# write.csv(split$train, "./SENTICOIN/data/processed/models/regression/loader/splits/lag_1/train/labels/labels.csv", row.names = FALSE);
# write.csv(split$test,  "./SENTICOIN/data/processed/models/regression/loader/splits/lag_1/test/labels/labels.csv", row.names = FALSE);
# write.csv(split$val,   "./SENTICOIN/data/processed/models/regression/loader/splits/lag_1/val/labels/labels.csv", row.names = FALSE);
# 
# # lag.hours = 6, test.split = 0.2, val.split = 0.1, stratify.by_date = TRUE
# split <- splitter(coins = NULL, lag.hours = 6, test.split = 0.2, val.split = 0.1, stratify.by_date = TRUE);
# write.csv(split$train, "./SENTICOIN/data/processed/models/regression/loader/splits/lag_6/train/labels/labels.csv", row.names = FALSE);
# write.csv(split$test,  "./SENTICOIN/data/processed/models/regression/loader/splits/lag_6/test/labels/labels.csv", row.names = FALSE);
# write.csv(split$val,   "./SENTICOIN/data/processed/models/regression/loader/splits/lag_6/val/labels/labels.csv", row.names = FALSE);
# 
# # lag.hours = 30, test.split = 0.2, val.split = 0.1, stratify.by_date = TRUE
# split <- splitter(coins = NULL, lag.hours = 30, test.split = 0.2, val.split = 0.1, stratify.by_date = TRUE);
# write.csv(split$train, "./SENTICOIN/data/processed/models/regression/loader/splits/lag_30/train/labels/labels.csv", row.names = FALSE);
# write.csv(split$test,  "./SENTICOIN/data/processed/models/regression/loader/splits/lag_30/test/labels/labels.csv", row.names = FALSE);
# write.csv(split$val,   "./SENTICOIN/data/processed/models/regression/loader/splits/lag_30/val/labels/labels.csv", row.names = FALSE);

# Testing "poles" as feature;
# require(ggplot2);
# test <- splitter(coins = NULL, lag.hours = 6, test.split = 0.2, val.split = 0.1, poles = c(-0.1, 0.1));
# ggplot(data = test) +
#   geom_histogram(aes(x = y_poled, y = ..density..), col = "tomato3", fill = "white", bins = 500) + 
#   geom_density(aes(x = y, y = ..density..), col = "dodgerblue",) +
#   theme_void();

# Testing popularity duplicate exclusion ("ADA", "ETH"):
# test = read.csv("./SENTICOIN/data/raw/twitter/@portfolio/csv/tweets.csv") %>% filter(coin == "ADA" | coin == "ETH") %>%
#   mutate(merged = paste0(tweet_id, "__", text));
# test = left_join(test, {test %>% group_by(coin) %>% summarize(popularity = n()) %>% ungroup()}, by = "coin") %>%
#   select(merged, tweet_id, coin, text, created_at, popularity);
# summary(as.factor(test$coin));
# #    ADA    ETH 
# # 168824 298610 
# test = test[order(test$merged, -abs(test$popularity)),];
# test = test[!duplicated(test$merged),] %>% select(-merged);
# summary(as.factor(test$coin)); rm(test);
# #   ADA    ETH 
# # 66516 298610

# Testing popularity duplicate exclusion ("all"):
# test = read.csv("./SENTICOIN/data/raw/twitter/@portfolio/csv/tweets.csv");
# summary(as.factor(test$coin)); rm(test);
# #    ADA   AVAX   DOGE    DOT    ETH    LTC    SOL    UNI    XRP 
# # 168824  46382  78571  52585 298610  33776 227243  19927  55430
# test = splitter.get_data();
# summary(as.factor(test$coin)); rm(test);
# #   ADA   AVAX   DOGE    DOT    ETH    LTC    SOL    UNI    XRP 
# # 59918  18049  35976  22158 291569  11856 183511   5695  20651

# Showcase of popularity duplicate exclusion;
# test <- data.frame(id = c(1, 2, 3, 3, 4, 4), 
#                    text = c("a", "a", "b", "c", "d", "d"),
#                    popularity = c(100, 200, 100, 200, 100, 200)) %>% 
#   mutate(merged = paste0(text, "_", id)); test;
# #  id text popularity merged (Initial dataset);
# #   1    a        100    a_1
# #   2    a        200    a_2
# #   3    b        100    b_3
# #   3    c        200    c_3
# #   4    d        100    e_4
# #   4    d        200    e_4
# test <- test[order(test$merged, -abs(test$popularity)),]; test; 
# #  id text popularity merged (Ordered by "tweet_id + text"); 
# #   1    a        100    a_1
# #   2    a        200    a_2
# #   3    b        100    b_3
# #   3    c        200    c_3
# #   4    d        200    e_4 # Changed position!
# #   4    d        100    e_4
# test <- test[!duplicated(test$merged),]; test;
# #  id text popularity merged # (Removing "tweet_id + text" duplicates based on popularity);
# #   1    a        100    a_1
# #   2    a        200    a_2
# #   3    b        100    b_3
# #   3    c        200    c_3
# #   4    d        200    e_4
# test <- test[order(test$text, -abs(test$popularity)),]; test; 
# #  id text popularity merged (Ordered by "text");
# #   2    a        200    a_2 # Changed position!
# #   1    a        100    a_1
# #   3    b        100    b_3
# #   3    c        200    c_3
# #   4    d        200    e_4
# test <- test[!duplicated(test$text),]; test;
# #  id text popularity merged (Removing "text" duplicates based on popularity);
# #   2    a        200    a_2
# #   3    b        100    b_3
# #   3    c        200    c_3
# #   4    d        200    e_4 # Final dataset!