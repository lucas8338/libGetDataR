#' split a timeseries data (data.frame) in to a x/y to fit a model
split.x_y_timeseries_split<- function(data,input_size,output_size){
  sequence_size<- input_size+output_size
  if (nrow(data)%%sequence_size!=0){
    data<-data[((nrow(data)%%sequence_size)+1):nrow(data),]
  }
  data.x<-list()
  data.y<-list()
  for (i in 1:(nrow(data)-sequence_size)+1){
    visible.data<-data[i:(sequence_size+(i-1)),]
    data.x<-append(data.x,list(as.data.frame(visible.data[1:input_size,])))
    data.y<-append(data.y,list(as.data.frame(visible.data[(input_size+1):nrow(visible.data),])))

  }
  list(data.x,data.y)
}