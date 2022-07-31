transf.diff<- function(data){
  nd<- c()
  for (i in 1:(length(data)-1)){
    nd<-append(nd,data[i+1]-data[i])
  }
  nd<- append(nd,NA,after=0)
  nd
}