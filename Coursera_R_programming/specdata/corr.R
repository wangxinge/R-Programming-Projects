corr<-function(directory,threshold=0){
  files_full<-list.files(directory,full.names = T)
  dat<-vector(mode = "numeric",length=0)
  for(i in 1:length(files_full)){
    table_i<-read.csv(files_full[i])
    csum<-sum((!is.na(table_i$sulfate)) & (!is.na(table_i$nitrate)))
    if(csum > threshold){
      tmp<-table_i[which(!is.na(table_i$sulfate)),]
      subtable_i<-tmp[which(!is.na(tmp$nitrate)),]
      dat<-c(dat,cor(subtable_i$sulfate,subtable_i$nitrate))
    }
  }
  dat
}