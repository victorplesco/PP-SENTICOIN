#' @author Victor Plesco
#' 
#' 
#' @title supp_get.expansions
#' 
#' @description Returns a customized set of expansions for a Twitter payload. 
#'
#' @param payload string \{"tweet" or "user"\}. Type of Twitter payload.
#' @param expansions vector of strings (e.g. "expansion_\{1\}", ...). Expansions to be returned (accepts "all"). To request all but some 
#' expansions, append "-" in front of each expansion in the input vector (e.g. "-expansion_\{1\}", ...). 
#' 
#' @seealso \url{https://developer.twitter.com/en/docs/twitter-api/expansions}
#' 
#' @return A comma separated string of expansions (e.g. "author_id,geo.place_id").
#' 
#' @export
supp_get.expansions <- function(payload, expansions) {
  
  payload_switch = switch(payload,
                         
    tweet = c(
      "author_id"                      ,
      "in_reply_to_user_id"            ,
      "entities.mentions.username"     ,
      "referenced_tweets.id.author_id" ,
      "referenced_tweets.id"           ,
      "attachments.media_keys"         ,
      "attachments.poll_ids"           ,
      "geo.place_id"
    ),
    
    user = c(
      "pinned_tweet_id"
    ),
    
    stop("The provided payload is wrong or is not handled by racademic!")
  );

  if(identical(expansions, "all")) { # Case 1: expansion = c("all");
    return(paste(payload_switch, collapse = ","));
  } else {
  
    # If mismatch;
    if(anyNA(match(sub("^-", "", expansions), payload_switch))) { 
      
      ifelse(any(sub("^-", "", expansions) == "all"), # Case 2: expansions = c("all", "expansion_{1}", ...);
             stop("The request to \"all\" is a standalone!"),
             stop("The following expansions do not belong to payload", payload, # Case 3: expansions = c("wrong_expansion_{1}", "expansion_{1}", ...);
                  " [", paste0(setdiff(sub("^-", "", expansions), payload_switch), collapse = ", "), "]!"));
    
      } else {
      
      # If no mismatch;
      ifelse(any(duplicated(sub("^-", "", expansions))), # Case 4: expansions = c("expansion_{1}", "expansion_{1}", ...);
             stop("Duplicated expansions [", paste(sub("^-", "", expansions)[which(duplicated(sub("^-", "", expansions)))], collapse = ", "), "] have been requested!"),
             ifelse(any(grepl("-", expansions) == any(!grepl("-", expansions))), # Case 5: expansions = c("-expansion_{1}", "expansion_{2}", ...);
                    stop("The request to \"-\" is a standalone!"),
                    ifelse(any(grepl("-", expansions)), # Case 6: expansions = c("-expansion_{1}", "-expansion_{2}", ...);
                           return(paste(grep(paste(sub("^-", "", expansions), collapse = "|"), payload_switch, invert = TRUE, value = TRUE), collapse = ",")),
                           return(paste(expansions, collapse = ","))))); # Case 7: expansions = c("expansion_{1}", "expansion_{2}", ...);
    };
  };
};