v2_aggregate_chunks <- function(path) {
  
  files = list.files(path);
  twitter_data = list(`data` = list(), 
                      `includes` = list(`tweets` = list(),
                                        `places` = list(),
                                        `users`  = list(), 
                                        `media`  = list(), 
                                        `polls`  = list()),
                      `errors` = list());
  
  for(i in files) {
    parsed.content = readRDS(paste0(path, i));
    
    twitter_data$data = append(twitter_data$data, parsed.content$data);
    twitter_data$includes$tweets = unique(append(twitter_data$includes$tweets, parsed.content$includes$tweets));
    twitter_data$includes$places = unique(append(twitter_data$includes$places, parsed.content$includes$places));
    twitter_data$includes$users  = unique(append(twitter_data$includes$users,  parsed.content$includes$users));
    twitter_data$includes$media  = unique(append(twitter_data$includes$media,  parsed.content$includes$media));
    twitter_data$includes$polls  = unique(append(twitter_data$includes$polls,  parsed.content$includes$polls));
    
    twitter_data$errors = append(twitter_data$errors, parsed.content$errors);
  }; return(twitter_data);
};
