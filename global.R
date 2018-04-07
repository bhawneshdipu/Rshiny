#1. GetMyData.R -> filename ==> data1
#2. Preprocessing -> data,channel ==> data1
#3. Aggregation ->data,channel,filename,aggregater ==> pac
source("Lib.R")
source("GetMyData.R")
source("Preprocessing.R")
source("Aggregation.R")
h2o.init()
##browser()
file_list <- list.files( pattern="(SC)(.)*(.csv)$",recursive = TRUE,full.names = TRUE)
raw_data <-do.call("rbind", lapply(file_list, function(x) {
  message(x)
  read_csv(x)
}))
##browser()
dipu.raw_data<-raw_data
##browser()
save(dipu.raw_data,file=paste0("dipu.raw_data",".Rda"))
dipu.pre_data<-function.MyPreprocessing(dipu.raw_data)

##browser()