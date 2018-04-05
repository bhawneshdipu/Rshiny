## GetRawData.R ##

function.getRawData <- function(name = "all",daterange="none",channel="all") {
  if (file.exists(paste0("dipu.raw_data", ".Rda"))) {
    load(paste0("dipu.raw_data", ".Rda"))
    return(dipu.raw_data)
    
  } else{
    file_list <-
      list.files(pattern = "(SC)(.)*(.csv)$",
                 recursive = TRUE,
                 full.names = TRUE)
    raw_data <-
      do.call("rbind", lapply(file_list, function(x) {
        message(x)
        read_csv(x)
      }))
    dipu.raw_data <- raw_data
    save(dipu.raw_data, file = paste0("dipu.raw_data", ".Rda"))
    return(dipu.raw_data)
    
  }
}
