#' @author Victor Plesco
#' 
#' 
#' @title supp_get.conditions
#' @description Returns a request's conditions based on its endpoint and the type of product track used.
#'  
#' @param v2.endpoint string \{"/2/tweets/search/all" or "/2/tweets/search/recent"\}. Twitter API v2 Search Endpoint.
#' @param product_track string \{"academic" or "standard"\}. Type of product track. 
#' 
#' @return A list of conditions.
#' 
#' @export
supp_get.conditions <- function(v2.endpoint, product_track) {
  
  switch(v2.endpoint,
   
   # Search Full-archive;
   `/2/tweets/search/all` = list (
         max.max_results       = 500,
         min.max_results       = 10,
         default.end_time      = Sys.time() - 30, # System time minus 30 seconds;
         default.start_time    = {function(end_time) {end_time - (60 * 60 * 24 * 30)}}, # end_time minus 30 days;
         length.query          = 1024,
         payload               = "tweet"
       ),
   
   # Search Recent;
   `/2/tweets/search/recent` = list (
         max.max_results       = 100,
         min.max_results       = 10,
         default.end_time      = Sys.time() - 30, # System time minus 30 seconds;
         default.start_time    = {function(end_time) {end_time - (60 * 60 * 24 * 7)}}, # end_time minus 7 days;
         length.query          = switch(product_track, academic = 1024, standard = 512,
                                        stop("The provided product track is wrong or is not handled by racademic!")),
         payload               = "tweet"
       ),
   
   stop("The provided endpoint is wrong or is not handled by racademic!")
   );
}; 