#' @author Victor Plesco
#' 
#' 
#' @title supp_make.safedir
#' @description Creates a "/racademic_data_<Sys.Date>_<Sys.Time>" directory with two sub-folders: "content" and "metadata". 
#' 
#' @param safe.dir string (e.g. "~/home/..."). Default=NULL. Path to an existing directory (serving as a data backup). If NULL, considers the home directory.
#' 
#' @return Path to the newly created directory.
#' 
#' @export
supp_make.safedir <- function(safe.dir = NULL) {
  
  data.dir = paste0("racademic_data_", format(Sys.Date(), format = "%y%m%d"), "_", format(Sys.time(), format = "%H%M%S")); # Create data folder;
  if(is.null(safe.dir)) { # If no safe.dir specified, uses home directory as base;
    
    safe.dir = "~/";
    while(data.dir %in% list.files(safe.dir)) { # Ensures no conflicts among directory names;
      data.dir = paste0("racademic_data_", format(Sys.Date(), format = "%y%m%d"), "_", format(Sys.time(), format = "%H%M%S")); Sys.sleep(0.1);
    }; dir.create(paste0(safe.dir, data.dir)); dir.create(paste0(safe.dir, data.dir, "/content/")); dir.create(paste0(safe.dir, data.dir, "/metadata/"));
    
  } else {
    
    if(dir.exists(safe.dir)) { 
      safe.dir = ifelse(identical(substring(safe.dir, nchar(safe.dir)), "/"), safe.dir, paste0(safe.dir, "/"));
      while(data.dir %in% list.files(safe.dir)) { # Ensures no conflicts among directory names;
        data.dir = paste0("racademic_data_", format(Sys.Date(), format = "%y%m%d"), "_", format(Sys.time(), format = "%H%M%S")); Sys.sleep(0.1);
      }; dir.create(paste0(safe.dir, data.dir)); dir.create(paste0(safe.dir, data.dir, "/content/")); dir.create(paste0(safe.dir, data.dir, "/metadata/"));
    } else {stop("The specified safe.dir doesn't exist. Provide an existing one!");};
    
  };
  return(paste0(safe.dir, data.dir, "/"));
};