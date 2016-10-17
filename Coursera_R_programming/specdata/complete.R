complete<-function(directory,id=1:332){
  files_full<-list.files(directory,full.names = T)
  dat<-data.frame()
  for(i in id){
    table_i<-read.csv(files_full[i])
    nobs_i<-sum(complete.cases(table_i))
    tmp<-data.frame(i,nobs_i)
    dat<-rbind(dat,tmp)
  }
  colnames(dat)<-c("id","nobs")
  dat
}