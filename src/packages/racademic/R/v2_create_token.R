#' @author: Victor Plesco
#' 
#' 
#' @title v2_create_token
#' @description Creates an OAuth 2.0 Bearer authentication "token".
#' 
#' @param token string. Bearer token associated with a user-created APP (requires a developer account).
#' 
#' @return An object of class racademic.
#' 
#' @export
v2_create_token <- function(token) {
  return(c(`Authorization` = sprintf('Bearer %s', token)));
};