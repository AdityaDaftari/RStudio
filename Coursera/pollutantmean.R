# ---
# title: "pollutantmean"
# author: "Aditya Daftari"
# date: "7/21/2020"
# ---

getRawData <- function(directory, id){
  
  directory_path = paste0("./",directory)
  file_names <- paste0(directory,"/",formatC(id, width=3, flag = 0),".csv")
  # file_names <- paste0(directory_path,"/",list.files(path = directory_path, pattern = ".csv"))
  # print(file_names)
  raw_data <- lapply(file_names, read.csv)
}

pollutantmean <- function(directory, pollutant, id = 1:332){
  
  raw_data <- getRawData(directory,id)
  pollutant_reading_sum <- 0
  pollutant_reading_count <- 0
  for(i in seq_along(raw_data))
  {
    temp <- raw_data[[i]][[pollutant]][!is.na(raw_data[[i]][[pollutant]])]
    pollutant_reading_sum <- pollutant_reading_sum + sum(temp)
    pollutant_reading_count <- pollutant_reading_count + length(temp)
  }
  pollutantmean <- pollutant_reading_sum / pollutant_reading_count
}
