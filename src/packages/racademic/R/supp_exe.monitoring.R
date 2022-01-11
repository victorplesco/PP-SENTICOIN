#' @author Victor Plesco
#' 
#' 
#' @title supp_exe.monitoring
#' @description Updates the values of the execution parameters and prints a short process summary.
#' 
#' @param exe.data list (e.g. requests = 10, tweets = 150, ect.). List of execution parameters.
#' @param api.data object of class response(). Information captured from a request.
#' @param param_list list (e.g. max_results = 10, ect.). List of a request's parameters.
#' @param safe.dir string (e.g. "~/home/..."). Path to an existing directory (serving as a data backup).
#' 
#' @return An updated set of execution parameters.
#' 
#' @export
supp_exe.monitoring <- function(exe.data, api.data, par.data, safe.dir) {
 
  # requests;
  exe.data$requests = exe.data$requests + 1; 
  
  # tweets;
  exe.data$tweets = exe.data$tweets + api.data$meta$result_count;

  # daily_counts;
  counts = as.list(table(as.character(strptime(lapply(api.data$data, "[[", "created_at"), format = "%Y-%m-%d")))); 
  if(all(names(counts) %in% names(exe.data$daily_counts))) {
    
    exe.data$daily_counts[which(names(exe.data$daily_counts) %in% names(counts))] = 
      mapply("+", exe.data$daily_counts[names(exe.data$daily_counts) %in% names(counts)], counts);
    
  } else {
    
    exe.data$daily_counts[which(names(exe.data$daily_counts) %in% names(counts))] = 
      mapply("+", exe.data$daily_counts[names(exe.data$daily_counts) %in% names(counts)], counts[which(names(counts) %in% names(exe.data$daily_counts))]);
    exe.data$daily_counts = append(exe.data$daily_counts, counts[order(setdiff(names(counts), names(exe.data$daily_counts)), decreasing = TRUE)], after = length(exe.data$daily_counts));
    
  };
  
  # daytime_reached;
  exe.data$daytime_reached = as.character(min(strptime(lapply(api.data$data, "[[", "created_at"), format = "%Y-%m-%dT%H:%M:%S.000Z")));
  
  # *_token;
  exe.data$previous_token = exe.data$current_token;
  exe.data$current_token  = exe.data$next_token;
  exe.data$next_token     = api.data$meta$next_token;
  
  
  cat("\n Reached:", as.character(exe.data$daytime_reached),
      "\n     End:", as.character(strptime(par.data$start_time, format = "%Y-%m-%dT%H:%M:%SZ")),
      "\n\n Previous Token:", as.character(exe.data$previous_token),
      "\n  Current Token:", as.character(exe.data$current_token),
      "\n     Next Token:", as.character(exe.data$next_token),
      "\n\n Date", strrep(" ", 5), "Counts");
  for(i in 1:length(exe.data$daily_counts)) {cat("\n", names(exe.data$daily_counts[i]), " ", as.character(exe.data$daily_counts[i]))};
  cat("\n\n Requests:", as.character(exe.data$requests), "Total:", as.character(exe.data$tweets), "\n"); 

  saveRDS(exe.data, paste0(safe.dir, "metadata/", "exe.summary.RDS"));
  
  return(exe.data);
};