#' @author Victor Plesco
#' 
#' 
#' @title supp_eval.response
#' @description Makes a request to a Twitter API v2 search endpoint and switches the return based on the encountered errors.
#' 
#' @param token object of class v2_create_token(). Bearer token associated with a user-created APP (requires a developer account).
#' @param v2.endpoint string \{"/2/tweets/search/all" or "/2/tweets/search/recent"\}. Twitter API v2 Search Endpoint.
#' @param param_list list (e.g. max_results = 10, ect.). List of a request's parameters.
#' 
#' @return A list composed of a response's metadata and its parsed content (with attached errors if encountered).
#' 
#' @export
supp_eval.response <- function(token, v2.endpoint, param_list) {
  
  safe.guardian = 0;
  repeat { # Ensures continuability in case of server breakdown;
    
    safe.guardian = safe.guardian + 1; if(safe.guardian > 10) {
      
      cat("\nSomething went wrong. Aborting program to avoid being rate limited!\n\n");
      return(list(0, `response` = response, `parsed.content` = parsed.content));
      
    };
    
    response = httr::GET(url = paste0("https://api.twitter.com", v2.endpoint), 
                         httr::add_headers(.headers = token), query = param_list); 
    
    # Parsing response to list format (ensures readability of errors);
    parsed.content = httr::content(response, as = "parsed");

    # Switch Legend:
    # 200 = OK {The request was successful!}
    # 429 = Too Many Requests {Returned when a request cannot be served due to the App's rate limit having been exhausted.}
    # 500 = Internal Server Error {Something is broken. Usually a temporary error.}
    # 502 = Bad Gateway {Twitter is down, or being upgraded.}
    # 503 = Service Unavailable {The Twitter servers are up, but overloaded with requests.}
    # 504 = Gateway Timeout {The Twitter servers are up, but the request couldn't be serviced due to some failure.}
    
    switch(as.character(response$status_code),
           
           `429` = 
             {
               cat("\n\nWarning message:\n  Program has stopped due to error in response. Recovering rate limits!\n",
                   "\n Title:", as.character(parsed.content$title), 
                   "\n Status Code:", as.character(response$status_code), 
                   "\n Details:", as.character(parsed.content$detail),
                   "\n For more details please visit:", as.character(parsed.content$type), "\n\n");
               
               # Handling rate limits;
               supp_eval.ratelimit(request.remaining = as.numeric(response$headers$`x-rate-limit-remaining`),
                                   request.reset = as.numeric(response$headers$`x-rate-limit-reset`));
             },
           
           `500` = 
             {
               cat("\n\nWarning message:\n  Program has stopped due to error in response. Trying again in 10s!\n",
                   "\n Title:", as.character(parsed.content$title), 
                   "\n Status Code:", as.character(response$status_code), 
                   "\n Details:", as.character(parsed.content$detail),
                   "\n For more details please visit:", as.character(parsed.content$type), "\n\n");
               Sys.sleep(10);
             },
           
           `502` = 
             {
               cat("\n\nWarning message:\n  Program has stopped due to error in response. Trying again in 10s!\n",
                   "\n Title:", as.character(parsed.content$title), 
                   "\n Status Code:", as.character(response$status_code), 
                   "\n Details:", as.character(parsed.content$detail),
                   "\n For more details please visit:", as.character(parsed.content$type), "\n\n");
               Sys.sleep(10);
             },
           
           `503` = 
             {
               cat("\n\nWarning message:\n  Program has stopped due to error in response. Trying again in 10s!\n",
                   "\n Title:", as.character(parsed.content$title), 
                   "\n Status Code:", as.character(response$status_code), 
                   "\n Details:", as.character(parsed.content$detail),
                   "\n For more details please visit:", as.character(parsed.content$type), "\n\n");
               Sys.sleep(10);
             },
           
           `200` = 
             {
               
               # Handling rate limits;
               supp_eval.ratelimit(request.remaining = as.numeric(response$headers$`x-rate-limit-remaining`),
                                   request.reset = as.numeric(response$headers$`x-rate-limit-reset`));
               
               if("errors" %in% names(parsed.content)) {
                 
                 if(any(lapply(parsed.content$errors, "[[", "title") %in% c("Forbidden", "Not Found Error", "Authorization Error") == FALSE)) {
                   cat("\n\nWarning message:\n  Program has stopped due to partial errors in response. Returning errors!\n\n");
                   return(list(0, `response` = response, `parsed.content` = parsed.content));
                   
                 } else {
                   cat("\n\nWarning message:\n  Program has encountered partial errors in response. Attaching to payload!\n\n");
                   return(list(2, `response` = response, `parsed.content` = parsed.content));
                 };
                 
               } else {return(list(1, `response` = response, `parsed.content` = parsed.content));};
             },
           
           { # For the remaining cases (not evaluated yet);
             cat("\n\nWarning message:\n  Program has stopped due to error in response. Returning errors!\n",
                 "\n Title:", as.character(parsed.content$title), 
                 "\n Status Code:", as.character(response$status_code), 
                 "\n Details:", as.character(parsed.content$detail),
                 "\n For more details please visit:", as.character(parsed.content$type), "\n\n");
             return(list(0, `response` = response, `parsed.content` = parsed.content));
           }
          );
  };
};