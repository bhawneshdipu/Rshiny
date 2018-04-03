## GetMyData.R ##

function.getMyData<-function(name="all"){
  
  
  folder <- "./"
  setwd("./")
  
  if(file.exists(paste0(name,"_",".Rda"))){
    local({
      load(paste0(name,"_",".Rda"))
      return (paste0(name,"_"))
    })
  }
  tmpname<-name
  if(name=="all"){
    tmpname<-""
  }
  
  file_pattern=paste0(tmpname,"(.)*(.csv$)")
  #file_list <- list.files(path=folder, pattern=file_pattern,recursive = TRUE,full.names = TRUE) # create list of all .csv files in folder
  file_list <- list.files(path=folder, pattern=file_pattern,recursive = TRUE,full.names = TRUE)
  # read in each .csv file in file_list and rbind them into a data frame called data1
  raw_data <- 
    do.call("rbind", 
            lapply(file_list, 
                   function(x){
                     message(x)
                     read_csv(x)
                   } 
                  )
            )
  
  #data <- data[is.finite(rowSums(data)),]
  save(raw_data,file=paste0(name,"_",".Rda"))
  return(raw_data)
}
