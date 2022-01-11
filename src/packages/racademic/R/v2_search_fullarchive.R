#' @author Victor Plesco
#' 
#' 
#' @title v2_search_fullarchive
#' @description Get tweets data on statuses identified via search query.
#' 
#' @param token object of class v2_create_token(). Bearer token associated with a user-created APP (requires a developer account).
#' @param product_track string \{"academic" or "standard"\}. Type of product track. 
#' @param safe.dir string (e.g. "~/home/..."). Default=NULL. Path to an existing directory (serving as a data backup). If NULL, considers 
#' the home directory.
#' @param query string (e.g. "btc -is:retweet"). One query for matching Tweets.
#' @param start_time string \{"\%Y-\%m-\%dT\%H:\%M:\%SZ"\} (ISO 8601/RFC 3339). The oldest UTC timestamp from which the Tweets will be
#' provided. Timestamp is in second granularity and is inclusive (for example, 12:00:01 includes the first second of the minute).
#' @param end_time string \{"\%Y-\%m-\%dT\%H:\%M:\%SZ"\} (ISO 8601/RFC 3339).
#' @param max_results integer [10, 500]. The maximum number of search results to be returned by a request. A number between 10 an the 
#' system limit (currently 500). By default, a request response will return 10 results.
#' @param next_token string. This parameter is used to get the next "page" of results. The value used with the parameter is pulled directly
#' from the response provided by the API, and should not be modified.
#' @param since_id string. Returns results with a Tweet ID greater than the specified ID. The ID specified is exclusive and responses
#' will not include it. If included with the same request as a `start_time` parameter, only `since_id` will be used.
#' @param until_id string. Returns results with a Tweet ID less than (that is, older than) the specified ID. Used with `since_id`. The ID
#' specified is exclusive and responses will not include it.
#' @param expansions vector of strings (e.g. "expansion_\{1\}", ...). Specifies a subset of expansions to be returned (accepts "all"). 
#' To request all but some expansions, append "-" in front of each expansion in the input vector (e.g. "-expansion_\{1\}", ...). 
#' @param _.fields vector of strings (e.g. "field_\{1\}", ...). Specifies a subset of fields to be returned (accepts "all"). To 
#' request all but some fields, append "-" in front of each field in the input vector (e.g. "-field_\{1\}", ...). 
#' 
#' @return A list of tweets and their expansions (with attached errors if encountered).
#' 
#' @export
v2_search_fullarchive <- function(token, safe.dir = NULL,
                                  query, start_time = NULL, end_time = NULL, max_results = 10, next_token = NULL, since_id = NULL, until_id = NULL, 
                                  expansions = NULL, tweet.fields = NULL, user.fields = NULL, media.fields = NULL, place.fields = NULL, poll.fields = NULL) {
  
  param_list = supp_eval.request(
    v2.endpoint = "/2/tweets/search/all", product_track = "academic", query = query, 
    start_time = start_time, end_time = end_time, max_results = max_results, next_token = next_token, since_id = since_id, until_id = until_id, expansions = expansions,
    tweet.fields = tweet.fields, user.fields = user.fields, media.fields = media.fields, place.fields = place.fields, poll.fields = poll.fields);

  twitter_data = supp_make.request(token = token, v2.endpoint = "/2/tweets/search/all", param_list = param_list, safe.dir = safe.dir);

  return(twitter_data);
}; 
