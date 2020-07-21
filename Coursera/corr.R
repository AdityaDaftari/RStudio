# ---
# title: "corr"
# author: "Aditya Daftari"
# date: "7/21/2020"
# ---

getRawData <- function(directory, id = 1:332){
  
  directory_path = paste0("./",directory)
  file_names <- paste0(directory,"/",formatC(id, width=3, flag = 0),".csv")
  # file_names <- paste0(directory_path,"/",list.files(path = directory_path, pattern = ".csv"))
  # print(file_names)
  raw_data <- lapply(file_names, read.csv)
}

complete <- function(directory, id = 1:332, raw_data = NULL){
  
  if (is.null(raw_data)){
    raw_data <- getRawData(directory,id)
  }
  output_date_frame = data.frame()
  for(i in seq_along(raw_data)){
    
    data_frame_i <- raw_data[[i]]$ID[!is.na(raw_data[[i]]$sulfate) & !is.na(raw_data[[i]]$nitrate)]
    id <- raw_data[[i]]$ID[1]
    n_obs  <- length(data_frame_i)
    output_date_frame <- rbind(output_date_frame,list("id"=id, "nobs"=n_obs))
  }
  output_date_frame
}

corr <- function(directory, threshold = 0){
  
  raw_data <- getRawData(directory)
  complete_cases <- complete(directory,raw_data = raw_data)
  complete_cases_over_threshold <- raw_data[complete_cases$id[complete_cases$nobs > threshold]]
  no_of_frames <- length(complete_cases_over_threshold)
  correlations <- numeric(length = no_of_frames)
  
  i <- 1
  for(data_frame_i in complete_cases_over_threshold){
    
    data_frame_i <- data_frame_i[(!is.na(data_frame_i$sulfate) & !is.na(data_frame_i$nitrate)),]
    correlations[i] <- cor(data_frame_i$sulfate, data_frame_i$nitrate)
    i <- i+1
    
  }
  correlations
}