
#########################三個專家間情緒評分一致性############################

setwd("C:/Users/0963625209/Desktop/碩論yo/情緒一致性")
data <- read.csv("yo.csv")
data$consistency <- NA

#選取180位資料做比對
data = data[c(511:690),]

#三個人
for (i in 1:nrow(data)){
  if (data[i,4] == data[i,19] & data[i,4]== data[i,34]){
    data[i,49] <- 1
  }else if((data[i,4]!=data[i,19] & data[i,4]!=data[i,34] & data[i,19]!=data[i,34])){
    data[i,49] <- 0
  }else{
    data[i,49] <- 0.5
  }
}

for (i in 1:nrow(data)){
  for (j in 5:18){
    data[i,j+(3*15)] = round((data[i,j] + data[i,j+15] + data[i,j+2*15])/3,3)
  }
}

for (i in 1:nrow(data)){
  total = 0
  n = 0
  for (j in 50:63){
    if (data[i,j] != 0.000){
      total = total + data[i,j]
      n = n+1
    }
  }
  if (n == 0){
    data[i,64] <- 1
  }else{
    data[i,64] <- total/n
  }
}
mean(data[,49]) #專家間情緒類別一致性
mean(data[,64]) #專家間AU一致性

#write.csv(data,file="C:/Users/0963625209/Desktop/碩論yo/情緒一致性/consistency_3.csv",row.names = FALSE)

###################################讀取系統資料###########################################

directory="D:/祐瑋資料/情緒/analysis/video/v"
#讀檔
v17_2 <- read.csv("D:/祐瑋資料/情緒/analysis/video/v17_2.csv")
video_list <- list()    
for (i in 1:60){ 
  if (i == 17){
    video_list[[i]]=v17_2
  }else{
    video_list[[i]]=read.csv(sprintf("%s%s.csv",directory,i))
  }
  for (x in 1:nrow(video_list[[i]])){
    if ((video_list[[i]][x,7] < 0.1) & (video_list[[i]][x,8] < 0.1) & (video_list[[i]][x,9] < 0.1) & (video_list[[i]][x,10] < 0.1) & (video_list[[i]][x,11] < 0.1) & (video_list[[i]][x,12] < 0.1)){
      video_list[[i]][x,13] <- 'Normal'
    }
  }
  video_list[[i]] <- video_list[[i]][,c(1,13:27)] #刪掉多餘的欄位
}

directory2="D:/祐瑋資料/情緒/analysis/practical/p"
do_list_1 <- list()
do_list_2 <- list()
do_list_3 <- list()
for (i in 1:60){
  do_list_1[[i]]=read.csv(sprintf("%s%s%s%s.csv",directory2, i, c("_"), 1))
  do_list_2[[i]]=read.csv(sprintf("%s%s%s%s.csv",directory2, i, c("_"), 2))
  do_list_3[[i]]=read.csv(sprintf("%s%s%s%s.csv",directory2, i, c("_"), 3))
  
  for (x in 1:nrow(do_list_1[[i]])){
    if ((do_list_1[[i]][x,7] < 0.1) & (do_list_1[[i]][x,8] < 0.1) & (do_list_1[[i]][x,9] < 0.1) & (do_list_1[[i]][x,10] < 0.1) & (do_list_1[[i]][x,11] < 0.1) & (do_list_1[[i]][x,12] < 0.1)){
      do_list_1[[i]][x,13] <- 'Normal'
    }
  }
  
  for (x in 1:nrow(do_list_2[[i]])){
    if ((do_list_2[[i]][x,7] < 0.1) & (do_list_2[[i]][x,8] < 0.1) & (do_list_2[[i]][x,9] < 0.1) & (do_list_2[[i]][x,10] < 0.1) & (do_list_2[[i]][x,11] < 0.1) & (do_list_2[[i]][x,12] < 0.1)){
      do_list_2[[i]][x,13] <- 'Normal'
    }
  }
  
  for (x in 1:nrow(do_list_3[[i]])){
    if ((do_list_3[[i]][x,7] < 0.1) & (do_list_3[[i]][x,8] < 0.1) & (do_list_3[[i]][x,9] < 0.1) & (do_list_3[[i]][x,10] < 0.1) & (do_list_3[[i]][x,11] < 0.1) & (do_list_3[[i]][x,12] < 0.1)){
      do_list_3[[i]][x,13] <- 'Normal'
    }
  }
  
  do_list_1[[i]] <- do_list_1[[i]][,c(1,13:27)]
  do_list_2[[i]] <- do_list_2[[i]][,c(1,13:27)]
  do_list_3[[i]] <- do_list_3[[i]][,c(1,13:27)]
  
}


for (i in 1:60){ #把秒數四捨五入
  video_list[[i]]$Time..sec. <- round(video_list[[i]]$Time..sec.)
  do_list_1[[i]]$Time..sec. <- round(do_list_1[[i]]$Time..sec.)
  do_list_2[[i]]$Time..sec. <- round(do_list_2[[i]]$Time..sec.)
  do_list_3[[i]]$Time..sec. <- round(do_list_3[[i]]$Time..sec.)
}

#########################尋找對應時間點的系統資料############################

coding <- read.csv("C:/Users/0963625209/Desktop/碩論yo/情緒一致性/情緒人工編碼一致性.csv")
coding <- coding[,-c(19,20)] #刪掉多餘欄位

syscoding <- as.data.frame(matrix(nrow = 0, ncol = 16))

for (i in 1:60){   #60位受試者
  for (j in 1:18){ #影片學習18筆
    for (a in 1:nrow(video_list[[i]])){
      if (video_list[[i]][a,1] == coding[j+(i-1)*30,3]){ #秒數相同的資料，如果沒有該列會顯示NA
        syscoding[j+(i-1)*30,] <- video_list[[i]][a,]
      }
    }
    if (is.na(syscoding[j+(i-1)*30,1])){ #找不到該時間點的資料，就往下再找一筆
      for (b in 1:nrow(video_list[[i]])){
        if (video_list[[i]][b,1] == coding[j+(i-1)*30,3]+1){
          syscoding[j+(i-1)*30,] <- video_list[[i]][b,]
        }
      }
    }
  }
  for (x in 19:22){ #實作學習一4筆
    for (a in 1:nrow(do_list_1[[i]])){
      if (do_list_1[[i]][a,1] == coding[x+(i-1)*30,3]){
        syscoding[x+(i-1)*30,] <- do_list_1[[i]][a,]
      }
    }
    if (is.na(syscoding[x+(i-1)*30,1])){
      for (b in 1:nrow(do_list_1[[i]])){
        if (do_list_1[[i]][b,1] == coding[x+(i-1)*30,3]+1){
          syscoding[x+(i-1)*30,] <- do_list_1[[i]][b,]
        }
      }
    }
  }
  for (y in 23:26){ #實作學習二4筆
    for (a in 1:nrow(do_list_2[[i]])){
      if (do_list_2[[i]][a,1] == coding[y+(i-1)*30,3]){
        syscoding[y+(i-1)*30,] <- do_list_2[[i]][a,]
      }
    }
    if (is.na(syscoding[y+(i-1)*30,1])){
      for (b in 1:nrow(do_list_2[[i]])){
        if (do_list_2[[i]][b,1] == coding[y+(i-1)*30,3]+1){
          syscoding[y+(i-1)*30,] <- do_list_2[[i]][b,]
        }
      }
    }
  }
  for (z in 27:30){ #實作學習三4筆
    for (a in 1:nrow(do_list_3[[i]])){
      if (do_list_3[[i]][a,1] == coding[z+(i-1)*30,3]){
        syscoding[z+(i-1)*30,] <- do_list_3[[i]][a,]
      }
    }
    if (is.na(syscoding[z+(i-1)*30,1])){
      for (b in 1:nrow(do_list_3[[i]])){
        if (do_list_3[[i]][b,1] == coding[z+(i-1)*30,3]+1){
          syscoding[z+(i-1)*30,] <- do_list_3[[i]][b,]
        }
      }
    }
  }
}

syscoding$index <- 1:1800

#檢查遺失值
which(is.na(syscoding$V1))

#########################專家與系統情緒評分一致性############################

colnames(syscoding) <- c('Time','系統Expression','系統AU1','系統AU2','系統AU4','系統AU6','系統AU7','系統AU9','系統AU10','系統AU12','系統AU14','系統AU15','系統AU17','系統AU23','系統AU24','系統AU25','index')

coding_all <- cbind(coding,syscoding[,c(2:16)])

coding_all$consistency_express <- NA

#情緒類別比對
for (i in 1:nrow(coding_all)){
  if (coding_all[i,4] == coding_all[i,19]){
    coding_all[i,34] <- 1
  }else{
    coding_all[i,34] <- 0
  }
}
#系統與專家情緒類別一致結果

mean(coding_all$consistency_express) #一致性分數

samelist <- c()
for (i in 1:nrow(coding_all)){
  if (coding_all[i,34] == 1){
    samelist <- c(samelist,coding_all[i,19])
  }
}

table(samelist)        #一致時系統的情緒類別數量
table(coding_all[,4])  #專家的情緒類別總數
table(coding_all[,19]) #系統的情緒類別總數

consistable <- as.data.frame(matrix(nrow = 7, ncol = 4))
colnames(consistable) <- c("Emotion","Artificial","System","Intersection")
consistable$Emotion <- c("Anger","Disgust","Fear","Happiness","Normal","Sadness","Surprise")

for (i in 1:7){
  consistable[i,2] <- table(coding_all[,19])[i] #專家編碼數量
  consistable[i,3] <- table(coding_all[,4])[i]  #系統編碼數量
  consistable[i,4] <- table(samelist)[i]        #交集編碼數量
}

#write.csv(consistable,file="C:/Users/0963625209/Desktop/碩論yo/情緒一致性/consistable.csv",row.names = FALSE)

#AU相關性

library(car)
library(sandwich)
library(RcmdrMisc)
cor <- coding_all[,c(5:18,20:33)]
rpb <- rcorr.adjust(cor,type = 'pearson')
rpb

library(apaTables)
apa.cor.table(cor,filename = "C:/Users/0963625209/Desktop/碩論yo/分析/rpb.doc",show.conf.interval = FALSE)
