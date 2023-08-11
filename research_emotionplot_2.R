
#-------------------------------PART1_讀檔與前處理------------------------------------------

#系統資料
directory="D:/祐瑋資料/情緒/analysis/video/v"
#讀第17位受試者資料，因為實驗操作有誤，第17位受試者有兩個資料需要合併，中間補8個遺漏值(8秒)，沒有特別需要處理的資料可忽略
v17_1 <- read.csv("D:/祐瑋資料/情緒/analysis/video/v17_1.csv")
v17_2 <- read.csv("D:/祐瑋資料/情緒/analysis/video/v17_2.csv")

x2 <- runif(390,-2.0,-1.0)
x2 <- NA
natwo <- as.data.frame(array(x2,c(8,39)))
names(natwo) <- c(colnames(v17_1))
v17 <- rbind(v17_1, natwo, v17_2) 
#拉時間軸
v17$Time..sec. <- c(0:2702)

#讀影片檔
video_list <- list()    
for (i in 1:60){ 
  if (i == 17){   #沒有特別需要處理的資料可忽略
    video_list[[i]]=v17
  }else{
    video_list[[i]]=read.csv(sprintf("%s%s.csv",directory,i))
  }
  video_list[[i]] <- video_list[[i]][,-c(28:39)] #刪掉多餘的欄位
  video_list[[i]] <- video_list[[i]][-c(1:11),] #刪掉開頭11s
  video_list[[i]]$Time..sec. <- c(0:(nrow(video_list[[i]])-1)) #從0秒開始所以多一秒
}

#影片pitch>20改na，unknown改na
for (i in 1:60){
  skip1 <- c()
  for (j in 1:nrow(video_list[[i]])){
    if (is.na(video_list[[i]][j,13]==TRUE)){
      next
    }
    if ((video_list[[i]][j,3] > 20)|(video_list[[i]][j,13])=="Unknown"){
      skip1 <- c(skip1, j) #把資料位置記下來
    }
  }
  if (is.null(skip1)==FALSE){
    video_list[[i]][skip1, 7:12] <- NA #改NA
  }
}  
#計算每位受試者資料數與遺失值數，可略
totalrecord <- as.data.frame(matrix(nrow = 60, ncol = 3))
totalrecord$V1 <- 1:60
for (i in 1:60){
  totalrecord[i,2] <- nrow(video_list[[i]])
  totalrecord[i,3] <- sum(is.na(video_list[[i]][,7]))
}
#write.csv(totalrecord,file="C:/Users/0963625209/Desktop/碩論yo/分析/question_two_totaldata.csv",row.names = FALSE)

#-------------------------------PART2_資料數不等的前處理-----------------------------------

#拉0.1為單位的百分比時間軸，可以改1，依據你的資料數作調整
for (i in 1:60){
  for (j in 1:nrow(video_list[[i]])){
    video_list[[i]][j,1] <- round((video_list[[i]][j,1]/(nrow(video_list[[i]])-1)*100), 1) #從0秒開始所以多一要扣掉
    #video_list[[i]][j,1] <- round((video_list[[i]][j,1]/(nrow(video_list[[i]])-1)*100), 0) #改成以1為單位範例
  }
}

##############相同時間單位取平均，再依序放入六個情緒的空資料框###################

#建六個空的情緒資料框
#如果有多情緒類別情緒空資料框還要再加一個
anger_60 <- as.data.frame(matrix(nrow = 0, ncol = 60))
disgust_60 <- as.data.frame(matrix(nrow = 0, ncol = 60))
fear_60 <- as.data.frame(matrix(nrow = 0, ncol = 60))
happiness_60 <- as.data.frame(matrix(nrow = 0, ncol = 60))
sadness_60 <- as.data.frame(matrix(nrow = 0, ncol = 60))
surprise_60 <- as.data.frame(matrix(nrow = 0, ncol = 60))

transmean <- video_list[[1]][1,7:12]
transmean <- transmean[-c(nrow(transmean)),] #轉換用資料框

#emo_list放包含所有csv檔的list, x1放情緒的第一欄位位置, y2放情緒的最後一個欄位位置, z1放期望資料長度
#如果有多情緒類別，有出現surprise_60下面每個的地方就還要再加
mean_emoprocessing <- function(emo_list,x1,y1,z1){ 
  for (a in 1:length(emo_list)){ #六十個受試者
    transmean <- emo_list[[a]][1,x1:y1] #建一個相同時間單位取平均的資料框，第一個先放進去
    for (b in 2:nrow(emo_list[[a]])){
      #如果現在的秒數與前一個秒數相同的話就放入transmean，沒有的話就先將transmean取平均再把新的時間點放入
      if ((emo_list[[a]][b,1]==emo_list[[a]][b-1,1]) & (b!= nrow(emo_list[[a]]))){ #秒數相同又不是最後一筆資料
        transmean <- rbind(transmean, emo_list[[a]][b,x1:y1])} 
      
      else if ((emo_list[[a]][b,1]==emo_list[[a]][b-1,1]) & (b==nrow(emo_list[[a]]))){ #秒數相同且是最後一筆資料
        transmean <- rbind(transmean, emo_list[[a]][b,x1:y1]) 
        skip2 <- c()
        for (i in 1:nrow(transmean)){
          if (is.na(transmean[i,1])){
            skip2 <- c(skip2,i)}}
        if (is.null(skip2)==TRUE){ #沒有NA的情況，直接取平均，原本是0.1-100但要變位置所以*10，秒數從0開始便位置所以要加1
          anger_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[1], 2, mean)
          disgust_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[2], 2, mean)
          fear_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[3], 2, mean)
          happiness_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[4], 2, mean)
          sadness_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[5], 2, mean)
          surprise_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[6], 2, mean)}
        else{
          transmean[-skip2, 1:(y1-x1+1)]
          if (is.na(transmean[1,1])){ #全部NA的情況，直接放入資料框中
            anger_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- NA
            disgust_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- NA
            fear_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- NA
            happiness_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- NA
            sadness_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- NA
            surprise_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- NA}
          else{ #排除NA剩下的數值
            anger_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[1], 2, mean)
            disgust_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[2], 2, mean)
            fear_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[3], 2, mean)
            happiness_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[4], 2, mean)
            sadness_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[5], 2, mean)
            surprise_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- apply(transmean[6], 2, mean)}
        }
      }
      else if ((emo_list[[a]][b,1]!=emo_list[[a]][b-1,1]) & (b==nrow(emo_list[[a]]))){
        #秒數不同且是最後一筆資料
        skip2 <- c()
        for (i in 1:nrow(transmean)){
          if (is.na(transmean[i,1])){
            skip2 <- c(skip2,i)}}
        if (is.null(skip2)==TRUE){
          anger_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[1], 2, mean)
          disgust_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[2], 2, mean)
          fear_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[3], 2, mean)
          happiness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[4], 2, mean)
          sadness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[5], 2, mean)
          surprise_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[6], 2, mean)}
        else{
          transmean[-skip2, 1:(y1-x1+1)]
          if (is.na(transmean[1,1])){
            anger_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA
            disgust_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA
            fear_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA
            happiness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA
            sadness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA
            surprise_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA}
          else{
            anger_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[1], 2, mean)
            disgust_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[2], 2, mean)
            fear_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[3], 2, mean)
            happiness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[4], 2, mean)
            sadness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[5], 2, mean)
            surprise_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[6], 2, mean)}
        }
        
        anger_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- emo_list[[a]][b,7]
        disgust_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- emo_list[[a]][b,8]
        fear_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- emo_list[[a]][b,9]
        happiness_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- emo_list[[a]][b,10]
        sadness_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- emo_list[[a]][b,11]
        surprise_60[emo_list[[a]][b,1]*(z1/100)+1,a] <- emo_list[[a]][b,12]
      }
      
      else{#秒數不同且不是最後一筆資料
        
        skip2 <- c()
        for (i in 1:nrow(transmean)){
          if (is.na(transmean[i,1])){
            skip2 <- c(skip2,i)}}
        if (is.null(skip2)==TRUE){
          anger_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[1], 2, mean)
          disgust_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[2], 2, mean)
          fear_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[3], 2, mean)
          happiness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[4], 2, mean)
          sadness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[5], 2, mean)
          surprise_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[6], 2, mean)}
        else{
          transmean[-skip2, 1:(y1-x1+1)]
          if (is.na(transmean[1,1])){
            anger_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA
            disgust_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA
            fear_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA
            happiness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA
            sadness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA
            surprise_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- NA}
          else{
            anger_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[1], 2, mean)
            disgust_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[2], 2, mean)
            fear_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[3], 2, mean)
            happiness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[4], 2, mean)
            sadness_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[5], 2, mean)
            surprise_60[emo_list[[a]][b-1,1]*(z1/100)+1,a] <- apply(transmean[6], 2, mean)}
        }
        
        transmean <- transmean[-c(nrow(transmean)),]
        transmean <- emo_list[[a]][b,x1:y1]
      }
    }
  }
  emotion_all <- list(anger_60,disgust_60,fear_60,happiness_60,sadness_60,surprise_60)
  return(emotion_all)
}

emotion_60 <- mean_emoprocessing(video_list,7,12,1000)

#---------------------------------PART3_所有受試者取平均-------------------------------------------

#60人每0.1平均
sum=0
n=0
transmean <- transmean[-c(1,2),]
emotion_mean <- transmean
for (i in 1:6){
  for (j in 1:nrow(emotion_60[[i]])){
    for (k in 1:ncol(emotion_60[[i]])){
      if (is.na(emotion_60[[i]][j,k])==FALSE){
        sum = sum + emotion_60[[i]][j,k]
        n = n + 1
        }
    }
    emotion_mean[j,i] <- sum/n
    sum=0
    n=0
  }
}
time_percentage <- 0:1000/10
emotion_mean$Percentage_of_videotime <- time_percentage

#---------------------------------PART4_畫情緒趨勢圖與原始資料變化圖-------------------------------------------

library(reshape2)
library(ggplot2)
library(ggpubr)
#變更資料形狀
emotion_plot <- melt(emotion_mean, id="Percentage_of_videotime")
colnames(emotion_plot) <- c("Video_time(%)","Emotion","Average_of_intensity")

#趨勢圖的函式
smooth_plot <- function(a){
  ggplot(a, aes(x = `Video_time(%)`, y = Average_of_intensity, group = Emotion, color = Emotion)) +
    scale_color_manual(values=c("#FF0033","#009E73","#999999","#FF9900","#0066FF","#9900FF"))+
    geom_smooth(size=2)+
    xlab("Video time(%)")+
    ylab("Average_of_intensity")+
    scale_x_continuous(limits = c(-5,100.0), breaks = seq(0,100.0,5.0))+
    scale_y_continuous(limits = c(0,0.15), breaks = seq(0,0.15,0.025))
}

#原始曲線圖函式
raw_plot <- function(b){
  ggplot(b, aes(x = `Video_time(%)`, y = Average_of_intensity, group = Emotion, color = Emotion)) +
    geom_line(size=1)+
    scale_color_manual(values=c("#FF0033","#009E73","#999999","#FF9900","#0066FF","#9900FF"))+
    xlab("Video time(%)")+
    ylab("Average_of_intensity")+
    scale_x_continuous(limits = c(0,100.0), breaks = seq(0,100.0,5.0))+
    scale_y_continuous(limits = c(0,0.15), breaks = seq(0,0.15,0.025))
}

smooth_plot(emotion_plot)
raw_plot(emotion_plot)

#------------------------PART5_高低學習表現分組後前面3、4部分再做一次--------------------------------------

#以學習表現的平均值做分組
totalscore <- read.csv("C:/Users/0963625209/Desktop/碩論yo/分析/totalscore.csv")
totalscore$score <- apply(totalscore[c(6,17)], 1, sum)
totalscore <- totalscore[-c(47),c(1,18)] #去掉作弊仔

totalscore$group <- NA
for (i in 1:nrow(totalscore)){
  if (totalscore[i,2] < mean(totalscore$score)){
    totalscore[i,3] <- "low"
  }else{
    totalscore[i,3] <- "high"
  }
}
mean(totalscore$score)  #學習表現平均值
table(totalscore$group) #各組數量

##############合併進emotion_60裡再分開成高低學習表現兩組###################

totalscore <- as.data.frame(t(totalscore))
colnames(totalscore) <- 1:60
for (i in 1:6){
  colnames(emotion_60[[i]]) <- 1:60
  emotion_60[[i]] <- rbind(emotion_60[[i]],totalscore)
}
lowlist <- list()
highlist <- list()
for (i in 1:6){
  lowlist[[i]] <- as.data.frame(t(emotion_60[[i]]))
  highlist[[i]] <- as.data.frame(t(emotion_60[[i]]))
  lowlist[[i]] <- subset(lowlist[[i]],lowlist[[i]]$group=="low")
  highlist[[i]] <- subset(highlist[[i]],highlist[[i]]$group=="high")
  lowlist[[i]] <- lowlist[[i]][,-c(1002,1003,1004)]
  highlist[[i]] <- highlist[[i]][,-c(1002,1003,1004)]
}

######################計算不同組別的情緒平均值#########################

lowlist_mean <- transmean
highlist_mean <- transmean
emotionlist_meanfunc <- function(emotionlist,emotionlist_mean){
  sum=0
  n=0
  for (i in 1:6){
    for (j in 1:ncol(emotionlist[[i]])){
      for (k in 1:nrow(emotionlist[[i]])){
        if (is.na(emotionlist[[i]][k,j])==FALSE){
          sum = sum + round(as.numeric(emotionlist[[i]][k,j]),3)
          n = n + 1
        }
      }
      emotionlist_mean[j,i] <- sum/n
      sum=0
      n=0
    }
  }
  return(emotionlist_mean)
}
lowlist_mean <- emotionlist_meanfunc(lowlist,lowlist_mean)
highlist_mean <- emotionlist_meanfunc(highlist,highlist_mean)

time_percentage <- 0:1000/10
lowlist_mean$Percentage_of_videotime <- time_percentage
highlist_mean$Percentage_of_videotime <- time_percentage

lowlist_mean_plot <- melt(lowlist_mean, id="Percentage_of_videotime")
colnames(lowlist_mean_plot) <- c("Video_time(%)","Emotion","Average_of_intensity")
highlist_mean_plot <- melt(highlist_mean, id="Percentage_of_videotime")
colnames(highlist_mean_plot) <- c("Video_time(%)","Emotion","Average_of_intensity")

#畫趨勢圖
smooth_plot(highlist_mean_plot)
smooth_plot(lowlist_mean_plot)

#畫原始曲線圖
raw_plot(highlist_mean_plot)
raw_plot(lowlist_mean_plot)

#-------------------------PART6_課室目標分組後前面3、4部分再做一次-------------------------------------------

#######################以課室目標做分組####################################

totalscore <- read.csv("C:/Users/0963625209/Desktop/碩論yo/分析/totalscore.csv")
totalscore <- totalscore[-c(47),c(1,2)]
for(i in 1:length(totalscore[,2])){
  if(totalscore[i,2]=="A"){totalscore[i,2] <- "MA"}
  if(totalscore[i,2]=="B"){totalscore[i,2] <- "PA"}
}

##############合併進emotion_60裡再分開成精熟與表現兩組###################
totalscore <- as.data.frame(t(totalscore))
colnames(totalscore) <- 1:60
for (i in 1:6){
  colnames(emotion_60[[i]]) <- 1:60
  emotion_60[[i]] <- rbind(emotion_60[[i]],totalscore)
}
malist <- list()
palist <- list()
for (i in 1:6){
  malist[[i]] <- as.data.frame(t(emotion_60[[i]]))
  palist[[i]] <- as.data.frame(t(emotion_60[[i]]))
  malist[[i]] <- subset(malist[[i]],malist[[i]]$組別=="MA")
  palist[[i]] <- subset(palist[[i]],palist[[i]]$組別=="PA")
  malist[[i]] <- malist[[i]][,c(1:1001)]
  palist[[i]] <- palist[[i]][,c(1:1001)]
}

######################計算不同組別的情緒平均值#########################

malist_mean <- transmean
palist_mean <- transmean
malist_mean <- emotionlist_meanfunc(malist,malist_mean)
palist_mean <- emotionlist_meanfunc(palist,palist_mean)

time_percentage <- 0:1000/10
malist_mean$Percentage_of_videotime <- time_percentage
palist_mean$Percentage_of_videotime <- time_percentage

malist_mean_plot <- melt(malist_mean, id="Percentage_of_videotime")
colnames(malist_mean_plot) <- c("Video_time(%)","Emotion","Average_of_intensity")
palist_mean_plot <- melt(palist_mean, id="Percentage_of_videotime")
colnames(palist_mean_plot) <- c("Video_time(%)","Emotion","Average_of_intensity")

#畫趨勢圖
smooth_plot(malist_mean_plot)
smooth_plot(palist_mean_plot)
#畫原始曲線圖
raw_plot(malist_mean_plot)
raw_plot(palist_mean_plot)

#-----------------PART7_課室目標與高低學習表現分四組後前面3、4部分再做一次-------------------------------------------

#######################以課室目標與學習表現做分組####################################

totalscore <- read.csv("C:/Users/0963625209/Desktop/碩論yo/分析/totalscore.csv")
totalscore$score <- apply(totalscore[c(6,17)], 1, sum)
totalscore <- totalscore[-c(47),c(1,2,18)]
for(i in 1:length(totalscore[,2])){
  if(totalscore[i,2]=="A"){totalscore[i,2] <- "MA"}
  if(totalscore[i,2]=="B"){totalscore[i,2] <- "PA"}
}
totalscore$group <- NA
for (i in 1:nrow(totalscore)){
  if (totalscore[i,3] < mean(totalscore$score)){
    totalscore[i,4] <- "low"
  }else{
    totalscore[i,4] <- "high"
  }
}

######合併進emotion_60裡再分開成精熟高學習表現、精熟低學習表現、表現高學習表現、表現低學習表現四組########

totalscore <- as.data.frame(t(totalscore))
colnames(totalscore) <- 1:60
for (i in 1:6){
  colnames(emotion_60[[i]]) <- 1:60
  emotion_60[[i]] <- rbind(emotion_60[[i]],totalscore)
}
mahighlist <- list()
malowlist <- list()
pahighlist <- list()
palowlist <- list()
for (i in 1:6){
  mahighlist[[i]] <- as.data.frame(t(emotion_60[[i]]))
  pahighlist[[i]] <- as.data.frame(t(emotion_60[[i]]))
  malowlist[[i]] <- as.data.frame(t(emotion_60[[i]]))
  palowlist[[i]] <- as.data.frame(t(emotion_60[[i]]))
  mahighlist[[i]] <- subset(mahighlist[[i]],mahighlist[[i]]$組別=="MA" & mahighlist[[i]]$group=="high")
  pahighlist[[i]] <- subset(pahighlist[[i]],pahighlist[[i]]$組別=="PA"& pahighlist[[i]]$group=="high")
  malowlist[[i]] <- subset(malowlist[[i]],malowlist[[i]]$組別=="MA" & malowlist[[i]]$group=="low")
  palowlist[[i]] <- subset(palowlist[[i]],palowlist[[i]]$組別=="PA"& palowlist[[i]]$group=="low")
  mahighlist[[i]] <- mahighlist[[i]][,c(1:1001)]
  pahighlist[[i]] <- pahighlist[[i]][,c(1:1001)]
  malowlist[[i]] <- malowlist[[i]][,c(1:1001)]
  palowlist[[i]] <- palowlist[[i]][,c(1:1001)]
}

######################計算不同組別的情緒平均值#########################

mahighlist_mean <- transmean
pahighlist_mean <- transmean
malowlist_mean <- transmean
palowlist_mean <- transmean

mahighlist_mean <- emotionlist_meanfunc(mahighlist,mahighlist_mean)
pahighlist_mean <- emotionlist_meanfunc(pahighlist,pahighlist_mean)
malowlist_mean <- emotionlist_meanfunc(malowlist,malowlist_mean)
palowlist_mean <- emotionlist_meanfunc(palowlist,palowlist_mean)

time_percentage <- 0:1000/10
mahighlist_mean$Percentage_of_videotime <- time_percentage
pahighlist_mean$Percentage_of_videotime <- time_percentage
malowlist_mean$Percentage_of_videotime <- time_percentage
palowlist_mean$Percentage_of_videotime <- time_percentage

mahighlist_mean_plot <- melt(mahighlist_mean, id="Percentage_of_videotime")
colnames(mahighlist_mean_plot) <- c("Video_time(%)","Emotion","Average_of_intensity")
pahighlist_mean_plot <- melt(pahighlist_mean, id="Percentage_of_videotime")
colnames(pahighlist_mean_plot) <- c("Video_time(%)","Emotion","Average_of_intensity")
malowlist_mean_plot <- melt(malowlist_mean, id="Percentage_of_videotime")
colnames(malowlist_mean_plot) <- c("Video_time(%)","Emotion","Average_of_intensity")
palowlist_mean_plot <- melt(palowlist_mean, id="Percentage_of_videotime")
colnames(palowlist_mean_plot) <- c("Video_time(%)","Emotion","Average_of_intensity")

#畫趨勢圖
smooth_plot(mahighlist_mean_plot)
smooth_plot(pahighlist_mean_plot)
smooth_plot(malowlist_mean_plot)
smooth_plot(palowlist_mean_plot)
#畫原始曲線圖
raw_plot(mahighlist_mean_plot)
raw_plot(pahighlist_mean_plot)
raw_plot(malowlist_mean_plot)
raw_plot(palowlist_mean_plot)

#描述性統計
emotionlist_stat <- transmean
stat_sys <- function(x){
  for (i in 1:6){
    emotionlist_stat[1,i] <- round(mean(x[,i]),3)
    emotionlist_stat[2,i] <- round(sd(x[,i]),3)
  }
  return(emotionlist_stat)
}

stat_sys(mahighlist_mean)
stat_sys(pahighlist_mean)
stat_sys(malowlist_mean)
stat_sys(palowlist_mean)


