## GetMyData.R ##

function.getMyData<-function(name="all"){
  folder <- "./"
  if(name=="all"){
    name=""
  }
  file_pattern=paste0(name,"(.)*(.csv$)")
  file_list <- list.files(path=folder, pattern=file_pattern) # create list of all .csv files in folder
  # read in each .csv file in file_list and rbind them into a data frame called data1 
  data <- 
    do.call("rbind", 
            lapply(file_list, 
                   function(x) 
                     read_csv(paste(folder, x, sep=''))))
  
  
  data
  }

