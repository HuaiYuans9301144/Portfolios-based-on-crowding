setwd("C:\\Users\\user\\Desktop")
data<-read.csv("stockdata1.csv")
data[,6]<-data$法人持股./data$週轉率._月 #新增新變數actratio
colnames(data)[6]<-"Actratio"
data$年月<-as.Date(data$年月)
library(lubridate)
library(tidyverse)
NumberOfStock<- data%>%group_by(年月)%>%summarise(n=n())
NumberOfStock[,2]    #檢測是否所有股票的日期數量皆相同
weightReturn<- matrix(nrow=40,ncol=789)  #新增一個加權報酬的矩陣
return<- matrix(nrow=40,ncol=789)    #原始報酬的矩陣

# k<-1 
# i<-1
# j<-1
# while (k<41&&i<(length(data$報酬率._月)+1)) {
#   
#     if(j>789){
#       k=k+1
#       j=1
#     }
#     return[k,j]<-data[i,5] 
#     i=i+1
#     j=j+1
#   
# }

i<-1
j<-789    
k<-1                          #總共有789支股票 4季*10年=40期
while(k<41){
  return[k,]<-data[i:j,5]   #將報酬矩陣計算為789支股票*40期
  i=i+789
  j=j+789
  k=k+1
}
return= return/100
weightReturn[1,]<-return[1,] #加權報酬第一期為原始報酬
x<-2
 while(x<41){
  for(j in c(1:789)){
    weightReturn[x,j]<- (1+return[(x-1),j])*return[x,j] #將加權報酬定義為Wi,t=(1+Ri,t-1)*Ri,t
  }                        
   x=x+1
 }


actratio<- matrix(nrow=40,ncol=789) #Actratio的矩陣
i<-1
j<-789         
k<-1

while(k<41){
  actratio[k,]<-data[i:j,6]     #將資料移入Actratio的矩陣 總共有789支股票40期
  i=i+789
  j=j+789
  k=k+1
}
exhibit<-data.frame(cbind(weightReturn[40,],actratio[40,])) #抽出2021/12/30的加權報酬以及擁擠度
colnames(exhibit)<-c("adjustReturn","Actratio")      
library(dplyr)
exhibit<-exhibit[order(exhibit$Actratio),]  #將資料依據擁擠度大小排序
i<-79
j<-2
result<-matrix(nrow = 10,ncol = 3)  #result用以儲存10組(依據擁擠度大小分組)的平均報酬
                                    #報酬的標準差 以及 擁擠度的平均
group1<-exhibit[1:78,]             #因總共有789支股票 令第一組為78支 其餘為79支
result[1,1]<-mean(group1[,2])
result[1,2]<-mean(group1[,1])
result[1,3]<-sd(group1[,1])
while (i<790) {
  result[j,2]<-mean(exhibit[i:(i+78),1]) #將報酬平均放入result
  result[j,1]<-mean(exhibit[i:(i+78),2]) #將擁擠度平均丟入
  result[j,3]<-sd(exhibit[i:(i+78),1])   #將報酬標準差丟入
  i=i+79
  j=j+1
}
table<- t(result)             #將表格轉置變為橫向
colnames(table)<-c(1:10)        
rownames(table)<-c("Actratio","RET(Mean in%)","RET(STD dev.)")  #建立table
table<-round(table,3)
knitr::kable(as.data.frame(table))
