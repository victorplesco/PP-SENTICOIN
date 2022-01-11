#' @author Victor Plesco
#' 
#' 
#' @title supp_get.fields
#' @description Returns a customized set of fields of a Twitter object. 
#'
#' @param object string \{"tweet", "user", "media", "poll", "place"\}. Type of Twitter object.
#' @param fields vector of strings (e.g. "field_\{1\}", ...). Fields to be returned (accepts "all"). To request all but some fields, 
#' append "-" in front of each field in the input vector (e.g. "-field_\{1\}", ...). 
#'  
#' @seealso \url{https://developer.twitter.com/en/docs/twitter-api/fields}
#' 
#' @return A comma separated string of fields (e.g. "author_id,geo.place_id").
#' 
#' @export
supp_get.fields <- function(object, fields) {
  
  object_switch = switch(object,
                             
    tweet = c(
      "id"                  ,
      "text"                ,
      "attachments"         ,
      "author_id"           ,
      "context_annotations" ,
      "conversation_id"     ,
      "created_at"          ,
      "entities"            ,
      "geo"                 ,
      "in_reply_to_user_id" ,
      "lang"                ,
      "possibly_sensitive"  ,
      "public_metrics"      ,
      "referenced_tweets"   ,
      "reply_settings"      ,
      "source"              ,
      "withheld"            
    ),
    
    user = c(
      "id"                  ,
      "name"                ,
      "username"            ,
      "created_at"          ,
      "description"         ,
      "entities"            ,
      "location"            ,
      "pinned_tweet_id"     ,
      "profile_image_url"   ,
      "protected"           ,
      "public_metrics"      ,
      "url"                 ,
      "verified"            ,
      "withheld"            
    ),
    
    media = c(
      "media_key"           ,
      "type"                ,
      "duration_ms"         ,
      "height"              ,
      "preview_image_url"   ,
      "public_metrics"      ,
      "width"               
    ),
    
    poll = c(
      "id"                  ,
      "options"             ,
      "duration_minutes"    ,
      "end_datetime"        ,
      "voting_status"       
    ),
    
    place = c(
      "full_name"           ,
      "id"                  ,
      "contained_within"    ,
      "country"             ,
      "country_code"        ,
      "geo"                 ,
      "name"                ,
      "place_type"          
    ),
    
    stop("The provided object is wrong or is not handled by racademic!")
  );  
  
  if(identical(fields, "all")) { # Case 1: fields = c("all");
    return(paste(object_switch, collapse = ","));
  } else {
    
    # If mismatch;
    if(anyNA(match(sub("^-", "", fields), object_switch))) { 
      
      ifelse(any(sub("^-", "", fields) == "all"), # Case 2: fields = c("all", "field_{1}", ...);
             stop("The request to \"all\" is a standalone!"),
             ifelse(any(grepl("^non_public_metrics$|^organic_metrics$|^promoted_metrics$", sub("^-", "", fields))),
                    stop("The following fields [", paste(grep("non_public_metrics|organic_metrics|promoted_metrics", sub("^-", "", fields), value = TRUE), collapse = ", "),
                         "] require user authentication are not handled by racademic!"),
                    stop("The following fields do not belong to object ", object, # Case 3: fields = c("wrong_field_{1}", "field_{1}", ...);
                         " [", paste0(setdiff(sub("^-", "", fields), object_switch), collapse = ", "), "]!")));
      
    } else {

      ifelse(any(duplicated(sub("^-", "", fields))), # Case 4: fields = c("field_{1}", "field_{1}", ...);
             stop("Duplicated fields [", paste(sub("^-", "", fields)[which(duplicated(sub("^-", "", fields)))], collapse = ", "), "] have been requested!"),
             ifelse(any(grepl("-", fields) == any(!grepl("-", fields))), # Case 5: fields = c("-field_{1}", "field_{2}", ...);
                    stop("The request to \"-\" is a standalone!"),
                           ifelse(any(grepl("-", fields)), # Case 6: fields = c("-field_{1}", "-field_{2}", ...);
                                  return(paste(grep(paste(sub("^-", "", fields), collapse = "|"), object_switch, invert = TRUE, value = TRUE), collapse = ",")),
                                  return(paste(fields, collapse = ","))))); # Case 7: fields = c("field_{1}", "field_{2}", ...);)
    };
  };
};