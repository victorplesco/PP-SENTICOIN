#' @author Victor Plesco
#' 
#' 
#' @title supp_make.request
#' @description 
#' 
#' @param token object of class v2_create_token(). Bearer token associated with a user-created APP (requires a developer account).
#' @param v2.endpoint string \{"/2/tweets/search/all" or "/2/tweets/search/recent"\}. Twitter API v2 Search Endpoint.
#' @param param_list list (e.g. max_results = 10, ect.). List of a request's parameters.
#' @param safe.dir string (e.g. "~/home/..."). Default=NULL. Path to an existing directory (serving as a data backup). If NULL, considers the home directory.
#' 
#' @return A response's parsed content (with attached errors if encountered).
#' 
#' @export
supp_make.request <- function(token, v2.endpoint, param_list, safe.dir = NULL) {
  
  safe.dir = supp_make.safedir(safe.dir = safe.dir); 
  twitter_data = list(`data` = list(), 
                      `includes` = list(`tweets` = list(),
                                        `places` = list(),
                                         `users` = list(), 
                                         `media` = list(), 
                                         `polls` = list()),
                      `errors` = list());
  exe.data = list(`requests` = 0, `tweets` = 0, `daily_counts` = list(), `daytime_reached` = NULL,
                  `previous_token` = NULL, `current_token` = NULL, `next_token` = NULL);
  
  # The following repeat statement will handle pagination;
  repeat {
  
    response_list = supp_eval.response(token = token, v2.endpoint = v2.endpoint, param_list = param_list);
    if(as.numeric(response_list[1]) != 0) { # If successful response;
      
      saveRDS(response_list$parsed.content[1:2], paste0(safe.dir, "content/", format(Sys.Date(), format = "%y%m%d"), "_", format(Sys.time(), format = "%H%M%S"), ".RDS")); 
      
      # Appending newly downloaded data;
      twitter_data$data = append(twitter_data$data, response_list$parsed.content$data); 
      # Appending newly downloaded expansions (checked for uniqueness);
      twitter_data$includes$tweets = unique(append(twitter_data$includes$tweets, response_list$parsed.content$includes$tweets));
      twitter_data$includes$places = unique(append(twitter_data$includes$places, response_list$parsed.content$includes$places));
      twitter_data$includes$users  = unique(append(twitter_data$includes$users,  response_list$parsed.content$includes$users));
      twitter_data$includes$media  = unique(append(twitter_data$includes$media,  response_list$parsed.content$includes$media));
      twitter_data$includes$polls  = unique(append(twitter_data$includes$polls,  response_list$parsed.content$includes$polls));
      # Appending partial errors;
      if(as.numeric(response_list[1]) == 2) {twitter_data$errors = append(twitter_data$errors, response_list$parsed.content$errors);};

      # Updating the execution summary and printing updates;
      exe.data = supp_exe.monitoring(exe.data = exe.data, api.data = response_list$parsed.content, par.data = param_list, safe.dir = safe.dir); 
      
      ifelse(is.null(response_list$parsed.content$meta$next_token), # If next_token is NULL;
             return(twitter_data), # Returns the so far downloaded data;
             {param_list$next_token = response_list$parsed.content$meta$next_token;}); # Updates next_token;
      
    } else {twitter_data$errors = append(twitter_data$errors, response_list$parsed.content$errors); return(twitter_data);}};
};