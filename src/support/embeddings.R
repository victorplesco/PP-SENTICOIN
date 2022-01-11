embeddings.unify <- function(path, superpath, chunks = NULL, read_feather = TRUE) {
  require(feather);

  embeddings = data.frame(); ram.flag = 0; overall.flag = 0; super.batch_name = 0; 
  for(batch in list.files(path)) {
    
    print(paste0("BATCH Nr. ", overall.flag));

    if(read_feather) {
      embeddings = rbind(embeddings, read_feather(paste0(path, batch)));
    } else {embeddings = rbind(embeddings, read.csv(paste0(path, batch)));};
    
    if(!is.null(chunks)) {
      if(ram.flag == chunks) {
        write_feather(embeddings, paste0(superpath, "BATCH_", super.batch_name, ".feather"));
        embeddings = data.frame(); ram.flag = 0; super.batch_name = super.batch_name + 1;
        if(overall.flag == length(list.files(path))) {return = 0;};
      };
    }; ram.flag = ram.flag + 1; overall.flag = overall.flag + 1;
    
    if(overall.flag == length(list.files(path))) {
      write_feather(embeddings, paste0(superpath, "BATCH_", super.batch_name, ".feather"));
    super.batch_name = super.batch_name + 1;};
  };
};

embeddings.label <- function() {
  require(feather); require(dplyr);
  embeddings <- read_feather("./SENTICOIN/data/processed/models/regression/loader/vectors/BERT/total/BATCH_0.feather");
  
  for(i in c("lag_1/", "lag_6/", "lag_30/")) {
    for(j in c("train/", "test/", "val/")) {
      write_feather(inner_join(read.csv(paste0("./SENTICOIN/data/processed/models/regression/loader/splits/" , i, j, "labels/labels.csv")), 
                              embeddings, by = "id"),
                    paste0("./SENTICOIN/data/processed/models/regression/loader/splits/", i, j, "total/embeddings.feather"));
      print(paste0(i, j, " Done!"));
    };
  };
};

embeddings.batches <- function(batch_length) {
  require(feather); require(dplyr);
  
  for(i in c("lag_1/")) {#  "lag_6/", "lag_30/"
    for(j in c("train/", "test/", "val/")) {
      
      to_batch = read_feather(paste0("./SENTICOIN/data/processed/models/regression/loader/splits/" , i, j, "total/embeddings.feather"));
      to_batch = to_batch[sample(nrow(to_batch)),];
      
      batch_name = 0; start = 1; 
      while(nrow(to_batch) - start >= batch_length) {
        
        cat("BATCH Nr: ", batch_name, "\n");
        write_feather(to_batch[start:(start + batch_length - 1),], paste0("./SENTICOIN/data/processed/models/regression/loader/splits/" , i, j, "batches/BATCH_", batch_name, ".feather"));
        start = start + batch_length; batch_name = batch_name + 1;
      };
      
      write_feather(to_batch[start:nrow(to_batch),], paste0("./SENTICOIN/data/processed/models/regression/loader/splits/" , i, j, "batches/BATCH_", batch_name, ".feather"));
      print(paste0(i, j, " Done!"));
    };
  };
};

embeddings.batches(batch_length = 64);

# require(feather); require(dplyr);
# a <- read_feather("./SENTICOIN/data/processed/models/regression/loader/splits/lag_1/val/backup/total/embeddings.feather")
# b <- a[, c("y", "id")]; b <- b[order(b$y),]; b <- b[c(c(1:round(nrow(b) * 0.10)), c((nrow(b) - round(nrow(b) * 0.10)):nrow(b))),];
# c <- left_join(b, a[, -which(colnames(a) == "y")], by = "id");
# write_feather(c, "./SENTICOIN/data/processed/models/regression/loader/splits/lag_1/val/total/embeddings.feather");

# embeddings.label()
# embeddings.batches(batch_length = 64)

# # Merging to chunks of 200 files;
# path       = "./SENTICOIN/data/processed/models/regression/loader/vectors/BERT/mini_batches/";
# superpath  = "./SENTICOIN/data/processed/models/regression/loader/vectors/BERT/super_batches/";
# embeddings = embeddings.unify(path, superpath, chunks = 200, read_feather = FALSE);
# 
# # Merging to total;
# path       = "./SENTICOIN/data/processed/models/regression/loader/vectors/BERT/super_batches/";
# superpath  = "./SENTICOIN/data/processed/models/regression/loader/vectors/BERT/total/";
# embeddings = embeddings.unify(path, superpath);

# # Labeling embeddings based on lag;
# embeddings.label();
