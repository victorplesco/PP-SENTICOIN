#' @author Victor Plesco
#' 
#' 
#' @title supp_eval.request
#' @description Given a set of a request's parameters, validates their values based on a pre-defined set of conditions.
#' 
#' @param v2.endpoint string \{"/2/tweets/search/all" or "/2/tweets/search/recent"\}. Twitter API v2 Search Endpoint.
#' @param product_track string \{"academic" or "standard"\}. Type of product track.
#' @param ... mix (e.g. max_results = 10, query = "btc -is:retweet", ect.). Set of a request's parameters.
#' 
#' @return A list of a request's validated parameters.
#' 
#' @export
supp_eval.request <- function(v2.endpoint, product_track, ...) {
  
  param_input = list(...); param_conditions = supp_get.conditions(v2.endpoint = v2.endpoint, product_track = product_track);
  
  # query;
  if(!is.null(param_input$query)) {
    if(nchar(param_input$query) > param_conditions$length.query) {
      stop("query length {", nchar(param_input$query), "} bigger than product track permited [", param_conditions$length.query, "]!");
    } else {param_input$query = param_input$query;}; 
  };

  # *_time;
  if(!is.null(param_input$start_time)) {
      if(!is.null(param_input$end_time)) {
        ifelse(is.na(strptime(param_input$start_time, format = "%Y-%m-%dT%H:%M:%SZ")), # Checking format of start_time;
               stop("Wrong format of start_time [", param_input$start_time, "] should receive %Y-%m-%dT%H:%M:%SZ!"),
               ifelse(is.na(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ")), # Checking format of end_time;
                      stop("Wrong format of end_time [", param_input$end_time, "] should receive %Y-%m-%dT%H:%M:%SZ!"),
                      ifelse(with(lapply(param_input[c("start_time", "end_time")], function(x) strptime(x, format = "%Y-%m-%dT%H:%M:%SZ")), start_time > end_time),
                             stop("start_time [", param_input$start_time, "] must be prior to the end_time [", param_input$end_time, "]!"), # start_time and end_time dependency (x < y);
                             ifelse(as.POSIXlt(param_input$end_time, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ") > as.POSIXlt(Sys.time(), tz = "UTC"), # end_time and request time (UTC) dependency (x < y);
                                    stop("end_time [", param_input$end_time, "] must be prior to the request time [", paste0(gsub(" ", "T", as.character(as.POSIXlt(Sys.time(), tz = "UTC"))), "Z]!")), 
                                    {param_input[c("start_time", "end_time")] = param_input[c("start_time", "end_time")]})))); # Validation passed;
    } else {stop("No end_time provided!");}; # Not clear what happens with the end_time if start_time is specified and the endpoint is "recent";
  }
  else {
    if(!is.null(param_input$end_time)) {
      ifelse(is.na(strptime(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ")), # Checking format of end_time;
             stop("Wrong format of end_time [", param_input$end_time, "] should receive %Y-%m-%dT%H:%M:%SZ!"),
             ifelse(as.POSIXlt(param_input$end_time, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ") > as.POSIXlt(Sys.time(), tz = "UTC"), # end_time and request time (UTC) dependency (x < y);
                    stop("end_time [", param_input$end_time, "] must be prior to the request time [", paste0(gsub(" ", "T", as.character(as.POSIXlt(Sys.time(), tz = "UTC"))), "Z]!")), 
                    # Validation passed, setting start_time to default: start_time = end_time - 7gg|30gg (based on the endpoint);
                    {cat(paste("\nWarning message:\n  start_time not provided, setting to default:\n\n    end_time:", gsub("T|Z", " ", param_input$end_time), 
                               "\n  start_time:", param_conditions$default.start_time(as.POSIXlt(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ")), "(default)\n\n"));
                     param_input$start_time = as.character(param_conditions$default.start_time(as.POSIXlt(param_input$end_time, format = "%Y-%m-%dT%H:%M:%SZ")));}));
      
    } else {
      cat(paste("\nWarning message:\n  start_time and end_time not provided, setting to default:\n\n    ", 
                "end_time:", as.character(as.POSIXlt(param_conditions$default.end_time, tz = "UTC")), "(default)\n  ",
                "start_time:", as.character(as.POSIXlt(param_conditions$default.start_time(as.POSIXlt(param_conditions$default.end_time, tz = "UTC")))), "(default)\n\n"));
      # Setting end_time to default: Sys.time() - 30 (converted to UTC Timezone). The default is already present in param_conditions and need only to be converted;
      param_input$end_time   = paste0(gsub(" ", "T", as.character(as.POSIXlt(param_conditions$default.end_time, tz = "UTC"))), "Z"); 
      # Setting start_time to default: start_time = end_time - 7gg|30gg (based on the endpoint). The default is already present in param_conditions as a function;
      param_input$start_time = paste0(gsub(" ", "T", as.character(param_conditions$default.start_time(as.POSIXlt(param_conditions$default.end_time, tz = "UTC")))), "Z");
    };
  };

  # max_results;
  if(!is.null(param_input$max_results)) {
    if(param_input$max_results < param_conditions$min.max_results | param_input$max_results > param_conditions$max.max_results) {
      stop("max_results {", param_input$max_results, "} must be in range [", param_conditions$min.max_results, ", ", param_conditions$max.max_results, "]!");
    } else {param_input$max_results = as.character(param_input$max_results);}; 
  };
  
  # next_token;
  if(!is.null(param_input$next_token)) {
    param_input$next_token = param_input$next_token;
  };
  
  # since_id;
  if(!is.null(param_input$since_id)) {
    param_input$since_id = param_input$since_id;
  };
  
  # until_id;
  if(!is.null(param_input$until_id)) {
    param_input$until_id = param_input$until_id;
  };
  
  # *.fields;
  if(!is.null(param_input$tweet.fields)) {param_input$tweet.fields = supp_get.fields(object = "tweet", fields = param_input$tweet.fields);};
  if(!is.null(param_input$user.fields))  {param_input$user.fields  = supp_get.fields(object = "user",  fields = param_input$user.fields);};
  if(!is.null(param_input$media.fields)) {param_input$media.fields = supp_get.fields(object = "media", fields = param_input$media.fields);};
  if(!is.null(param_input$place.fields)) {param_input$place.fields = supp_get.fields(object = "place", fields = param_input$place.fields);};
  if(!is.null(param_input$poll.fields))  {param_input$poll.fields  = supp_get.fields(object = "poll",  fields = param_input$poll.fields);};
  
  # expansions;
  if(!is.null(param_input$expansions)) {
    param_input$expansions = supp_get.expansions(payload = param_conditions$payload, expansions = param_input$expansions);
  }; 
  
  return(param_input);
}; 