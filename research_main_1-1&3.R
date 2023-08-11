
#-------------------------------PART1_讀檔與前處理------------------------------------------

#設路徑
setwd("C:/Users/0963625209/Desktop/碩論yo/分析")

##########問卷資料#############
#實驗前準備有運算思維與程式經驗
#學前量表有課室目標與自我效能
#totalscore有知識評量分數與實作評量分數跟課室目標組別
totalscore <- read.csv("totalscore.csv")
#knowledge_test <- read.csv("程式語言知識評量 (回覆) - 表單回應 1.csv",fileEncoding = "UTF-8")
beforelearn <- read.csv("程式語言學前量表 (回覆) - 表單回應 1.csv",fileEncoding = "UTF-8")
prepare1 <- read.csv("實驗前準備 (回覆) - 表單回應 1.csv",fileEncoding = "UTF-8")
prepare2 <- read.csv("程式學習實驗前準備 (回覆).csv")
doemotion <- read.csv("實作情緒量表 (回覆) - 表單回應 1.csv",fileEncoding = "UTF-8")
videoemotion <- read.csv("影片情緒量表 (回覆) - 表單回應 1.csv",fileEncoding = "UTF-8")

#第一筆跟預試放一起，取出來再合併。
#knowledge_test_1 <- read.csv("程式語言知識評量_預試 (回覆) - 表單回應 1.csv",fileEncoding = "UTF-8")
#knowledge_test_1 <- knowledge_test_1[-c(1:2),]
videoemotion_1 <- read.csv("影片情緒量表_預試 (回覆) - 表單回應 1.csv",fileEncoding = "UTF-8")
videoemotion_1 <- videoemotion_1[-c(1:2),]
doemotion_1 <- read.csv("實作情緒量表_預試 (回覆) - 表單回應 1.csv",fileEncoding = "UTF-8")
doemotion_1 <- doemotion_1[-c(1:2),]
beforelearn_1 <- read.csv("程式語言學前量表_預試 (回覆) - 表單回應 1.csv",fileEncoding = "UTF-8")
beforelearn_1 <- beforelearn_1[-c(1:2),]

beforelearn <- rbind(beforelearn_1,beforelearn)
doemotion <- rbind(doemotion_1,doemotion)
videoemotion <- rbind(videoemotion_1,videoemotion)
#knowledge_test <- rbind(knowledge_test_1,knowledge_test)
prepare1 <- prepare1[,-c(4:10)] #去掉成就目標的部份
prepare <- rbind(prepare1,prepare2)

#############1-1處理實驗前準備資料檔，整理運算思維與程式經驗###############

#刪掉沒來的
prepare <- prepare [-c(6,13,51,54,55),]
prepare$experience <- apply(prepare[4:9], 1, sum) #每個人各題的程式經驗分數總和
#對運算思維答案
for (j in 10:17){
  for (i in 1:length(prepare[,j])){
    if (prepare[i,j]=="D."){
      prepare[i,j] <- as.integer(1)
      as.integer(prepare[i,j])
    }else if (prepare[i,j]=="D. Kim"){
      prepare[i,j] <- as.integer(2)
      as.integer(prepare[i,j])
    }else if (prepare[i,j]=="B. 85元"){
      prepare[i,j] <- as.integer(3)
      as.integer(prepare[i,j])
    }else if (prepare[i,j]=="B. 2條"){
      prepare[i,j] <- as.integer(2)
      as.integer(prepare[i,j])
    }else if (prepare[i,j]=="A. 4(8(C)) U) 8(C) 4(D)"){
      prepare[i,j] <- as.integer(3)
      as.integer(prepare[i,j])
    }else if (prepare[i,j]=="A."){
      prepare[i,j] <- as.integer(1)
      as.integer(prepare[i,j])
    }else if (prepare[i,j]=="D. 假設文章中都沒有出現$，先將所有11取代成$，再將所有1取代成11，最後將所有$取代成1"){
      prepare[i,j] <- as.integer(1)
      as.integer(prepare[i,j])
    }else if (prepare[i,j]=="D. 7"){
      prepare[i,j] <- as.integer(2)
      as.integer(prepare[i,j])
    }else{
      prepare[i,j] <- as.integer(0)
      as.integer(prepare[i,j])
    }
  }
}

prepare[,10:17] = as.data.frame(lapply(prepare[,10:17],as.numeric)) #轉資料型態為數值
prepare$ct <- apply(prepare[10:17], 1, sum) #運算思維分數總和
#運算思維與程式經驗標準化
prepare$ct <- scale(prepare$ct)
prepare$experience <- scale(prepare$experience)

prepare$實驗編號 <- c(111052201,111050701,111052002,111051101,111051001,111050902,111050601,111050903,111051201,111051002,
                  111050901,111051301,111052102,111051401,111051501,111051601,111051701,111051602,111051702,111051801,
                  111051901,111052001,111052101,111060101,111061501,111052301,111052401,111052701,111060102,111053001,
                  111052603,111052801,111060703,111061201,111052402,111052502,111052601,111052602,111052501,111052901,
                  111060202,111053101,111060701,111052802,111052902,111053002,111053102,111060201,111060702,111061101,
                  111061801,111061602,111061202,111061301,111061401,111061601,111061702,111061701,111061802,111061901,
                  111062101)
ability <- prepare[,18:20]

###############1-2整理各變項成一個資料檔model######################

#整理自我效能分數
beforelearn$efficacy <- apply(beforelearn[12:19], 1, sum)
self_efficacy <- beforelearn[,c(2,20)]
#整理學習表現分數
totalscore$score <- apply(totalscore[c(6,17)], 1, sum)

model <- totalscore[,c(1,2,18)]
model <- merge(model,self_efficacy, by="實驗編號")
model <- merge(model,ability, by="實驗編號")
#整理課室目標精熟、表現的分數
beforelearn$c_performance <- apply(beforelearn[,3:6], 1, sum)
beforelearn$c_master <- apply(beforelearn[,7:11], 1, sum)

classroom <- beforelearn[,c(2,21,22)]
model <- merge(model,classroom, by="實驗編號")
model <- model[-c(47),] #刪掉作弊者

for(i in 1:length(model[,2])){
  if(model[i,2]=="A"){model[i,2] <- "MA"}
  if(model[i,2]=="B"){model[i,2] <- "PA"}
}

#-------------------------------PART2_獨立樣本t檢定------------------------------------------

A = subset(model,model$組別=="MA")
B = subset(model,model$組別=="PA")

#檢測操弄是否成功
library(psych)
library(car)
library(reshape2)
library(dplyr)
describe(A[,-c(1)])
describe(B[,-c(1)])
model$組別 = as.factor(model$組別)
#leveneTest(model$c_master,model$組別)
#leveneTest(model$c_performance,model$組別)
#t.test(model$c_master~model$組別,data = model, var.equal = TRUE, mu=0)
#t.test(model$c_performance~model$組別,data = model, var.equal = FALSE, mu=0)
var.test(A$c_master,B$c_master)
var.test(A$c_performance,B$c_performance)
t.test(A$c_master, B$c_master, var.equal = TRUE)
t.test(A$c_performance,B$c_performance, var.equal = FALSE)

#-------------------------------PART3_預測學習表現------------------------------------------


#設類別變項0.5與-0.5較準確
CMC = contrasts(model$組別)
CMC[1:2] = c(-0.5, 0.5)
CMC
#預測學習表現
model$efficacy_sd <- scale(model$efficacy)
model_score <- lm(score ~ ct + experience + (efficacy_sd*組別),contrasts = list(組別 = CMC), data=model)
vif(model_score) #檢查共線性
summary(model_score)

#實作
#model_practical <- lm(實作分數 ~ ct + experience + (efficacy_sd*組別),contrasts = list(組別 = CMC), data=model)
#vif(model_practical)
#summary(model_practical)

#知識
#model_knowledge <- lm(知識分數 ~ ct + experience + (efficacy_sd*組別),contrasts = list(組別 = CMC), data=model)
#vif(model_knowledge)
#summary(model_knowledge)

#-------------------------------PART4_預測學業情緒問卷------------------------------------------

###############4-1整理情緒問卷的各變項進資料檔model#######################################

#整理出影片與實作學習的無聊、樂趣與困惑
videoemotion$video_boring <- apply(videoemotion[3:7], 1, sum)
videoemotion$video_enjoyment <- apply(videoemotion[8:12], 1, sum)
videoemotion$video_confused <- apply(videoemotion[13:17], 1, sum)
doemotion$practical_boring <- apply(doemotion[3:7], 1, sum)
doemotion$practical_enjoyment <- apply(doemotion[8:12], 1, sum)
doemotion$practical_confused <- apply(doemotion[13:17], 1, sum)

videoemotion <- videoemotion[,c(2,18,19,20)]
#修正一些填錯號碼的人
videoemotion[37,1] <- 111053001
videoemotion <- videoemotion[-c(47),]

doemotion <- doemotion[,c(2,18,19,20)]
doemotion[42,1] <- 111060102
doemotion <- doemotion[-c(47),]

model <- merge(model,videoemotion, by="實驗編號")
model <- merge(model,doemotion, by="實驗編號", all.x=TRUE, all.y=TRUE)

##################4-2整理成可分析的情緒model###############################

modelset <- model[,-c(4,7,8)]
model_emotion <- melt(modelset, id.vars = c("實驗編號","efficacy_sd","ct","experience","score","組別"))
colnames(model_emotion) <- c("id","efficacy_sd","ct","experience","score","classgroup","step","emotion_score")

#分成無聊、樂趣、困惑三種
model_boring = subset(model_emotion,model_emotion$step=="video_boring" | model_emotion$step=="practical_boring")
model_boring$step <- factor(model_boring$step, labels = c("video_boring", "practical_boring"))
model_enjoyment = subset(model_emotion,model_emotion$step=="video_enjoyment" | model_emotion$step=="practical_enjoyment")
model_enjoyment$step <- factor(model_enjoyment$step, labels = c("video_enjoyment", "practical_enjoyment"))
model_confused = subset(model_emotion,model_emotion$step=="video_confused" | model_emotion$step=="practical_confused")
model_confused$step <- factor(model_confused$step, labels = c("video_confused", "practical_confused"))


CBC = contrasts(model_boring$step)
CBC[1:2] = c(-0.5, 0.5)
CBC

CEC = contrasts(model_enjoyment$step)
CEC[1:2] = c(-0.5, 0.5)
CEC

CCC = contrasts(model_confused$step)
CCC[1:2] = c(-0.5, 0.5)
CCC

#####################4-3預測情緒問卷結果##################################

library(emmeans)
library(lme4)
library(lmerTest)
library(ggplot2)

describe(model_boring)
lmm_boring <- lmer(emotion_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
            contrasts = list(classgroup = CMC, step = CBC),
            data = model_boring, REML = T)
summary(lmm_boring)
vif(lmm_boring) 

describe(model_enjoyment)
lmm_enjoyment <- lmer(emotion_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                   contrasts = list(classgroup = CMC, step = CEC),
                   data = model_enjoyment, REML = T)
summary(lmm_enjoyment)
vif(lmm_enjoyment)

describe(model_confused)
lmm_confused <- lmer(emotion_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                      contrasts = list(classgroup = CMC, step = CCC),
                      data = model_confused, REML = T)
summary(lmm_confused)
vif(lmm_confused)

#無聊交互作用
mean_boring <- emmeans(lmm_boring, pairwise ~ classgroup:step, adjust="none")
mean_boring
mean_boring_fixstep <- emmeans(lmm_boring, pairwise ~ classgroup|step, adjust="none")
mean_boring_classgroup <- emmeans(lmm_boring, pairwise ~ step|classgroup, adjust="none")

mean_boring_fixstep <- as.data.frame(mean_boring_fixstep$emmean)
names(mean_boring_fixstep)[3] <- "boring_mean"

ggplot(data = mean_boring_fixstep, aes(x = step, y = boring_mean, color = classgroup))+
  geom_point(position = position_dodge(0.1))+
  geom_errorbar(aes(x = step, ymin = lower.CL, ymax = upper.CL, color = classgroup),
                position = position_dodge(0.1), width = 0.1, size=2)+
  geom_line(aes(group = classgroup), position = position_dodge(0.1),size=2)+
  ggtitle("Interaction of goal structure and step in boring emotion")+
  theme(axis.text = element_text(size = 20), #y-axis label size
        axis.title = element_text(size = 20), #x-axis label size
        axis.title.x = element_text(size = 20), # x-axis title
        axis.title.y = element_text(size = 20), # y-axis title
        plot.title = element_text(size = 20))

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(reghelper)

#樂趣交互作用

enjoyment_step_efficacy = plot_model(lmm_enjoyment, type = "emm", terms = c("efficacy_sd", "step"),axis.title = "enjoyment_score", 
                                 title = "Interaction of learning step and efficacy in enjoyment emotion",dot.size=10)
enjoyment_step_efficacy

lmm_enjoyment_2 <- lmer(emotion_score ~ ct + experience + step*efficacy_sd + (1|id),
                      #contrasts = list(classgroup = CMC, step = CEC),
                      data = model_enjoyment, REML = T)
summary(lmm_enjoyment_2)

simple_slopes(lmm_enjoyment_2)

#困惑交互作用

confused_class_step = plot_model(lmm_confused, type = "emm", terms = c("efficacy_sd", "step"),axis.title = "confused_score", 
                        title = "Interaction of learning step and efficacy in confused emotion",dot.size=10)
confused_class_step

lmm_confused_2 <- lmer(emotion_score ~ ct + experience + step*efficacy_sd + (1|id),
                        #contrasts = list(classgroup = CMC, step = CCC),
                        data = model_confused, REML = T)
summary(lmm_confused_2)

simple_slopes(lmm_confused_2)



#-------------------------------PART5_預測系統情緒數值---------------------------------------------------

##################5-1讀取影片與實作學習之系統資料##########################

directory="D:/祐瑋資料/情緒/analysis/video/v"
#讀檔
v17_1 <- read.csv("D:/祐瑋資料/情緒/analysis/video/v17_1.csv")
v17_2 <- read.csv("D:/祐瑋資料/情緒/analysis/video/v17_2.csv")
v17 <- rbind(v17_1,v17_2) 
#讀影片檔
video_list <- list()    
for (i in 1:60){ 
  if (i == 17){
    video_list[[i]]=v17
  }else{
    video_list[[i]]=read.csv(sprintf("%s%s.csv",directory,i))
  }
  video_list[[i]] <- video_list[[i]][,-c(28:39)] #去掉au的部份
  video_list[[i]] <- video_list[[i]][-c(1:11),] #去掉開頭的11秒
}
#讀實作檔
directory2="D:/祐瑋資料/情緒/analysis/practical/p"
do_list <- list()
do_list_tiny <- list()
for (i in 1:60){
  for (j in 1:3){
    do_list_tiny[[j]]=read.csv(sprintf("%s%s%s%s.csv",directory2, i, c("_"), j))
  }
  do_list[[i]] <- rbind(do_list_tiny[[1]],do_list_tiny[[2]],do_list_tiny[[3]])
  do_list[[i]] <- do_list[[i]][,-c(28:39)]
  do_list_tiny <- list()
}
######################5-2計算去除多少資料，可略至0.1部份########################
rowcount <- as.data.frame(matrix(nrow = 0, ncol = 20))
for (i in 1:60){
  rowcount[i,1] <- nrow(video_list[[i]])
  rowcount[i,2] <- nrow(do_list[[i]])
}
#影片六種情緒皆 <0.05
for (i in 1:60){
  skip2 <- c()
  for (x in 1:nrow(video_list[[i]])){
    if ((video_list[[i]][x,7] < 0.05) & (video_list[[i]][x,8] < 0.05) & (video_list[[i]][x,9] < 0.05) & (video_list[[i]][x,10] < 0.05) & (video_list[[i]][x,11] < 0.05) & (video_list[[i]][x,12] < 0.05)){
      skip2 <- c(skip2, x)
    }
  }
  if (is.null(skip2)==FALSE){
    video_list[[i]] <- video_list[[i]][-skip2,]
  }
  rowcount[i,3] <- nrow(video_list[[i]])
}  

#實作六種情緒皆 <0.05
for (i in 1:60){
  do_skip2 <- c()
  for (x in 1:nrow(do_list[[i]])){
    if ((do_list[[i]][x,7] < 0.05) & (do_list[[i]][x,8] < 0.05) & (do_list[[i]][x,9] < 0.05) & (do_list[[i]][x,10] < 0.05) & (do_list[[i]][x,11] < 0.05) & (do_list[[i]][x,12] < 0.05)){
      do_skip2 <- c(do_skip2, x)
    }
  }
  if (is.null(do_skip2)==FALSE){
    do_list[[i]] <- do_list[[i]][-do_skip2,]
  }
  rowcount[i,4] <- nrow(do_list[[i]])
}  

#重讀影片檔
video_list <- list()    
for (i in 1:60){ 
  if (i == 17){
    video_list[[i]]=v17
  }else{
    video_list[[i]]=read.csv(sprintf("%s%s.csv",directory,i))
  }
  video_list[[i]] <- video_list[[i]][,-c(28:39)]
  video_list[[i]] <- video_list[[i]][-c(1:11),]
}
#重讀實作檔
do_list <- list()
do_list_tiny <- list()
for (i in 1:60){
  for (j in 1:3){
    do_list_tiny[[j]]=read.csv(sprintf("%s%s%s%s.csv",directory2, i, c("_"), j))
  }
  do_list[[i]] <- rbind(do_list_tiny[[1]],do_list_tiny[[2]],do_list_tiny[[3]])
  do_list[[i]] <- do_list[[i]][,-c(28:39)]
  do_list_tiny <- list()
}

#影片六種情緒皆 <0.15
for (i in 1:60){
  skip2 <- c()
  for (x in 1:nrow(video_list[[i]])){
    if ((video_list[[i]][x,7] < 0.15) & (video_list[[i]][x,8] < 0.15) & (video_list[[i]][x,9] < 0.15) & (video_list[[i]][x,10] < 0.15) & (video_list[[i]][x,11] < 0.15) & (video_list[[i]][x,12] < 0.15)){
      skip2 <- c(skip2, x)
    }
  }
  if (is.null(skip2)==FALSE){
    video_list[[i]] <- video_list[[i]][-skip2,]
  }
  rowcount[i,5] <- nrow(video_list[[i]])
}  
#實作六種情緒皆 <0.15
for (i in 1:60){
  do_skip2 <- c()
  for (x in 1:nrow(do_list[[i]])){
    if ((do_list[[i]][x,7] < 0.15) & (do_list[[i]][x,8] < 0.15) & (do_list[[i]][x,9] < 0.15) & (do_list[[i]][x,10] < 0.15) & (do_list[[i]][x,11] < 0.15) & (do_list[[i]][x,12] < 0.15)){
      do_skip2 <- c(do_skip2, x)
    }
  }
  if (is.null(do_skip2)==FALSE){
    do_list[[i]] <- do_list[[i]][-do_skip2,]
  }
  rowcount[i,6] <- nrow(do_list[[i]])
}  
colnames(rowcount) <- c("video_all","do_all","video_emotion<.05","do_emotion<.05","video_emotion<.15","do_emotion<.15")

#重讀影片檔
video_list <- list()    
for (i in 1:60){ 
  if (i == 17){
    video_list[[i]]=v17
  }else{
    video_list[[i]]=read.csv(sprintf("%s%s.csv",directory,i))
  }
  video_list[[i]] <- video_list[[i]][,-c(28:39)]
  video_list[[i]] <- video_list[[i]][-c(1:11),]
}
#重讀實作檔
do_list <- list()
do_list_tiny <- list()
for (i in 1:60){
  for (j in 1:3){
    do_list_tiny[[j]]=read.csv(sprintf("%s%s%s%s.csv",directory2, i, c("_"), j))
  }
  do_list[[i]] <- rbind(do_list_tiny[[1]],do_list_tiny[[2]],do_list_tiny[[3]])
  do_list[[i]] <- do_list[[i]][,-c(28:39)]
  do_list_tiny <- list()
}

#影片去掉pitch>20, 六種情緒皆 <0.1
for (i in 1:60){
  skip2 <- c()
  for (x in 1:nrow(video_list[[i]])){
    if ((video_list[[i]][x,7] < 0.1) & (video_list[[i]][x,8] < 0.1) & (video_list[[i]][x,9] < 0.1) & (video_list[[i]][x,10] < 0.1) & (video_list[[i]][x,11] < 0.1) & (video_list[[i]][x,12] < 0.1)){
      skip2 <- c(skip2, x)
    }
  }
  if (is.null(skip2)==FALSE){
    video_list[[i]] <- video_list[[i]][-skip2,]
  }
  rowcount[i,7] <- nrow(video_list[[i]]) #刪除皆小於0.1剩餘的比例
  
  skip1 <- c()
  for (j in 1:length(video_list[[i]]$Pitch)){
    
    if (video_list[[i]][j,3] > 20){
      skip1 <- c(skip1, j) 
    }
  }
  if (is.null(skip1)==FALSE){
    video_list[[i]] <- video_list[[i]][-skip1,]
  }
  rowcount[i,8] <- nrow(video_list[[i]]) #刪除皆小於0.1與頭過低剩餘的比例
}  

#實作去掉pitch>20, 六種情緒皆 <0.1
for (i in 1:60){
  do_skip2 <- c()
  for (x in 1:nrow(do_list[[i]])){
    if ((do_list[[i]][x,7] < 0.1) & (do_list[[i]][x,8] < 0.1) & (do_list[[i]][x,9] < 0.1) & (do_list[[i]][x,10] < 0.1) & (do_list[[i]][x,11] < 0.1) & (do_list[[i]][x,12] < 0.1)){
      do_skip2 <- c(do_skip2, x)
    }
  }
  if (is.null(do_skip2)==FALSE){
    do_list[[i]] <- do_list[[i]][-do_skip2,]
  }
  rowcount[i,9] <- nrow(do_list[[i]])
  do_skip1 <- c()
  for (j in 1:length(do_list[[i]]$Pitch)){
    
    if (do_list[[i]][j,3] > 20){
      do_skip1 <- c(do_skip1, j) 
    }
  }
  if (is.null(do_skip1)==FALSE){
    do_list[[i]] <- do_list[[i]][-do_skip1,]
  }
  rowcount[i,10] <- nrow(do_list[[i]])
}  
colnames(rowcount) <- c("video_all","do_all","video_emotion<.05","do_emotion<.05","video_emotion<.15","do_emotion<.15","video_emotion<.1","video_pitch<20","do_emotion<.1","do_pitch<20")

######################5-3計算不同標準剩餘的資料比例#############################

#0.05比例
rowcount <- rowcount[,1:10]
rowcount$video_percentage_0.05 <- round((rowcount[,3]/rowcount[,1])*100,2)
rowcount$do_percentage_0.05 <- round((rowcount[,4]/rowcount[,2])*100,2)
rowcount$percentage_0.05 <- round(((rowcount[,3]+rowcount[,4])/(rowcount[,1]+rowcount[,2]))*100,2)
#0.1比例
rowcount$video_percentage_0.1 <- round((rowcount[,7]/rowcount[,1])*100,2)
rowcount$do_percentage_0.1 <- round((rowcount[,9]/rowcount[,2])*100,2)
rowcount$percentage_0.1 <- round(((rowcount[,7]+rowcount[,9])/(rowcount[,1]+rowcount[,2]))*100,2)
#0.15比例
rowcount$video_percentage_0.15 <- round((rowcount[,5]/rowcount[,1])*100,2)
rowcount$do_percentage_0.15 <- round((rowcount[,6]/rowcount[,2])*100,2)
rowcount$percentage_0.15 <- round(((rowcount[,5]+rowcount[,6])/(rowcount[,1]+rowcount[,2]))*100,2)

#0.1+pitch<20
rowcount$video_percentage_20 <- round((rowcount[,8]/rowcount[,1])*100,2)
rowcount$do_percentage_20 <- round((rowcount[,10]/rowcount[,2])*100,2)
rowcount$percentage_20 <- round(((rowcount[,8]+rowcount[,10])/(rowcount[,1]+rowcount[,2]))*100,2)

rowcount_result <- rowcount[,11:22]
#write.csv(rowcount_result,"C:/Users/0963625209/Desktop/碩論yo/分析/rowcount_result.csv",row.names = FALSE)

########################5-4計算情緒時間比例####################################

#取影片每個情緒比例
sys_percentage <- as.data.frame(matrix(nrow = 0, ncol = 6))

for (i in 1:60){
  happiness=0
  sadness=0
  surprise=0
  fear=0
  disgust=0
  anger=0
  for (j in 1:nrow(video_list[[i]])){
    if (video_list[[i]][j,13]=="Happiness"){
      happiness=happiness+1}
    if (video_list[[i]][j,13]=="Sadness"){
      sadness=sadness+1}
    if (video_list[[i]][j,13]=="Surprise"){
      surprise=surprise+1}
    if (video_list[[i]][j,13]=="Fear"){
      fear=fear+1}
    if (video_list[[i]][j,13]=="Disgust"){
      disgust=disgust+1}
    if (video_list[[i]][j,13]=="Anger"){
      anger=anger+1}
  }
  sys_percentage <- rbind(sys_percentage,c(happiness/nrow(video_list[[i]]),sadness/nrow(video_list[[i]]),surprise/nrow(video_list[[i]]),
           fear/nrow(video_list[[i]]),disgust/nrow(video_list[[i]]),anger/nrow(video_list[[i]])))
  
}
colnames(sys_percentage) <- c("video_happiness_%","video_sadness_%","video_surprise_%","video_fear_%","video_disgust_%","video_anger_%")

model <- cbind(model,sys_percentage)
#取實作每個情緒比例
dosys_percentage <- as.data.frame(matrix(nrow = 0, ncol = 6))

for (i in 1:60){
  happiness=0
  sadness=0
  surprise=0
  fear=0
  disgust=0
  anger=0
  for (j in 1:nrow(do_list[[i]])){
    if (do_list[[i]][j,13]=="Happiness"){
      happiness=happiness+1}
    if (do_list[[i]][j,13]=="Sadness"){
      sadness=sadness+1}
    if (do_list[[i]][j,13]=="Surprise"){
      surprise=surprise+1}
    if (do_list[[i]][j,13]=="Fear"){
      fear=fear+1}
    if (do_list[[i]][j,13]=="Disgust"){
      disgust=disgust+1}
    if (do_list[[i]][j,13]=="Anger"){
      anger=anger+1}
  }
  dosys_percentage <- rbind(dosys_percentage,c(happiness/nrow(do_list[[i]]),sadness/nrow(do_list[[i]]),surprise/nrow(do_list[[i]]),
                     fear/nrow(do_list[[i]]),disgust/nrow(do_list[[i]]),anger/nrow(do_list[[i]])))
}
colnames(dosys_percentage) <- c("practical_happiness_%","practical_sadness_%","practical_surprise_%","practical_fear_%","practical_disgust_%","practical_anger_%")

model <- cbind(model,dosys_percentage)

##########################5-5計算情緒平均信心值#####################################

#取影片每個情緒平均強度
sys_mean <- as.data.frame(matrix(nrow = 0, ncol = 6))
for (i in 1:60){
  sys_mean <- rbind(sys_mean,apply(video_list[[i]][,7:12], 2, mean))
}
colnames(sys_mean) <- c("video_anger_mean","video_disgust_mean","video_fear_mean","video_happiness_mean","video_sadness_mean","video_surprise_mean")
model <- cbind(model,sys_mean)
#取實作每個情緒平均強度
dosys_mean <- as.data.frame(matrix(nrow = 0, ncol = 6))
for (i in 1:60){
  dosys_mean <- rbind(dosys_mean,apply(do_list[[i]][,7:12], 2, mean))
}
colnames(dosys_mean) <- c("practical_anger_mean","practical_disgust_mean","practical_fear_mean","practical_happiness_mean","practical_sadness_mean","practical_surprise_mean")
model <- cbind(model,dosys_mean)
#write.csv(model,"C:/Users/0963625209/Desktop/碩論yo/分析/allvariables.csv",row.names = FALSE)
##########################5-6系統情緒時間比例建模#####################################

sys_model_percentage <- model[,c(1,2,5,6,9,16,17,18,19,20,21,22,23,24,25,26,27)]
sys_emotion_percentage <- melt(sys_model_percentage, id.vars = c("實驗編號","efficacy_sd","ct","experience","組別"))
colnames(sys_emotion_percentage) <- c("id","efficacy_sd","ct","experience","classgroup","step","sys_percentage_score")

sys_percentage_happiness = subset(sys_emotion_percentage,sys_emotion_percentage$step=="video_happiness_%" | sys_emotion_percentage$step=="practical_happiness_%")
sys_percentage_happiness$step <- factor(sys_percentage_happiness$step, labels = c("video_happiness_%", "practical_happiness_%"))

sys_percentage_sadness = subset(sys_emotion_percentage,sys_emotion_percentage$step=="video_sadness_%" | sys_emotion_percentage$step=="practical_sadness_%")
sys_percentage_sadness$step <- factor(sys_percentage_sadness$step, labels = c("video_sadness_%", "practical_sadness_%"))

sys_percentage_surprise = subset(sys_emotion_percentage,sys_emotion_percentage$step=="video_surprise_%" | sys_emotion_percentage$step=="practical_surprise_%")
sys_percentage_surprise$step <- factor(sys_percentage_surprise$step, labels = c("video_surprise_%", "practical_surprise_%"))

sys_percentage_fear = subset(sys_emotion_percentage,sys_emotion_percentage$step=="video_fear_%" | sys_emotion_percentage$step=="practical_fear_%")
sys_percentage_fear$step <- factor(sys_percentage_fear$step, labels = c("video_fear_%", "practical_fear_%"))

sys_percentage_disgust = subset(sys_emotion_percentage,sys_emotion_percentage$step=="video_disgust_%" | sys_emotion_percentage$step=="practical_disgust_%")
sys_percentage_disgust$step <- factor(sys_percentage_disgust$step, labels = c("video_disgust_%", "practical_disgust_%"))

sys_percentage_anger = subset(sys_emotion_percentage,sys_emotion_percentage$step=="video_anger_%" | sys_emotion_percentage$step=="practical_anger_%")
sys_percentage_anger$step <- factor(sys_percentage_anger$step, labels = c("video_anger_%", "practical_anger_%"))


CHAC = contrasts(sys_percentage_happiness$step)
CHAC[1:2] = c(-0.5, 0.5)
CHAC

CSAC = contrasts(sys_percentage_sadness$step)
CSAC[1:2] = c(-0.5, 0.5)
CSAC

CSUC = contrasts(sys_percentage_surprise$step)
CSUC[1:2] = c(-0.5, 0.5)
CSUC

CFEC = contrasts(sys_percentage_fear$step)
CFEC[1:2] = c(-0.5, 0.5)
CFEC

CDIC = contrasts(sys_percentage_disgust$step)
CDIC[1:2] = c(-0.5, 0.5)
CDIC

CANC = contrasts(sys_percentage_anger$step)
CANC[1:2] = c(-0.5, 0.5)
CANC

##########################5-7預測系統情緒時間比例結果#####################################

describe(sys_percentage_happiness)
lmm_happiness_percentage <- lmer(sys_percentage_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                   contrasts = list(classgroup = CMC, step = CHAC),
                   data = sys_percentage_happiness, REML = T)
summary(lmm_happiness_percentage)
vif(lmm_happiness_percentage)

describe(sys_percentage_sadness)
lmm_sadness_percentage <- lmer(sys_percentage_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                      contrasts = list(classgroup = CMC, step = CSAC),
                      data = sys_percentage_sadness, REML = T)
summary(lmm_sadness_percentage)
vif(lmm_sadness_percentage)

describe(sys_percentage_surprise)
lmm_surprise_percentage <- lmer(sys_percentage_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                    contrasts = list(classgroup = CMC, step = CSUC),
                    data = sys_percentage_surprise, REML = T)
summary(lmm_surprise_percentage)
vif(lmm_surprise_percentage)

describe(sys_percentage_fear)
lmm_fear_percentage <- lmer(sys_percentage_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                     contrasts = list(classgroup = CMC, step = CFEC),
                     data = sys_percentage_fear, REML = T)
summary(lmm_fear_percentage)
vif(lmm_fear_percentage)

describe(sys_percentage_disgust)
lmm_disgust_percentage <- lmer(sys_percentage_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                 contrasts = list(classgroup = CMC, step = CDIC),
                 data = sys_percentage_disgust, REML = T)
summary(lmm_disgust_percentage)
vif(lmm_disgust_percentage)

describe(sys_percentage_anger)
lmm_anger_percentage <- lmer(sys_percentage_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                    contrasts = list(classgroup = CMC, step = CANC),
                    data = sys_percentage_anger, REML = T)
summary(lmm_anger_percentage)
vif(lmm_anger_percentage)

##########################5-8系統情緒平均強度建模#####################################

sys_model_mean <- model[,c(1,2,5,6,9,28,29,30,31,32,33,34,35,36,37,38,39)]
sys_emotion_mean <- melt(sys_model_mean, id.vars = c("實驗編號","efficacy_sd","ct","experience","組別"))
colnames(sys_emotion_mean) <- c("id","efficacy_sd","ct","experience","classgroup","step","sys_mean_score")

sys_mean_happiness = subset(sys_emotion_mean,sys_emotion_mean$step=="video_happiness_mean" | sys_emotion_mean$step=="practical_happiness_mean")
sys_mean_happiness$step <- factor(sys_mean_happiness$step, labels = c("video_happiness_mean", "practical_happiness_mean"))

sys_mean_sadness = subset(sys_emotion_mean,sys_emotion_mean$step=="video_sadness_mean" | sys_emotion_mean$step=="practical_sadness_mean")
sys_mean_sadness$step <- factor(sys_mean_sadness$step, labels = c("video_sadness_mean", "practical_sadness_mean"))

sys_mean_surprise = subset(sys_emotion_mean,sys_emotion_mean$step=="video_surprise_mean" | sys_emotion_mean$step=="practical_surprise_mean")
sys_mean_surprise$step <- factor(sys_mean_surprise$step, labels = c("video_surprise_mean", "practical_surprise_mean"))

sys_mean_fear = subset(sys_emotion_mean,sys_emotion_mean$step=="video_fear_mean" | sys_emotion_mean$step=="practical_fear_mean")
sys_mean_fear$step <- factor(sys_mean_fear$step, labels = c("video_fear_mean", "practical_fear_mean"))

sys_mean_disgust = subset(sys_emotion_mean,sys_emotion_mean$step=="video_disgust_mean" | sys_emotion_mean$step=="practical_disgust_mean")
sys_mean_disgust$step <- factor(sys_mean_disgust$step, labels = c("video_disgust_mean", "practical_disgust_mean"))

sys_mean_anger = subset(sys_emotion_mean,sys_emotion_mean$step=="video_anger_mean" | sys_emotion_mean$step=="practical_anger_mean")
sys_mean_anger$step <- factor(sys_mean_anger$step, labels = c("video_anger_mean", "practical_anger_mean"))


CHAMC = contrasts(sys_mean_happiness$step)
CHAMC[1:2] = c(-0.5, 0.5)
CHAMC

CSAMC = contrasts(sys_mean_sadness$step)
CSAMC[1:2] = c(-0.5, 0.5)
CSAMC

CSUMC = contrasts(sys_mean_surprise$step)
CSUMC[1:2] = c(-0.5, 0.5)
CSUMC

CFEMC = contrasts(sys_mean_fear$step)
CFEMC[1:2] = c(-0.5, 0.5)
CFEMC

CDIMC = contrasts(sys_mean_disgust$step)
CDIMC[1:2] = c(-0.5, 0.5)
CDIMC

CANMC = contrasts(sys_mean_anger$step)
CANMC[1:2] = c(-0.5, 0.5)
CANMC

##########################5-9預測系統情緒平均強度結果#####################################

describe(sys_mean_happiness)
lmm_happiness_mean <- lmer(sys_mean_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                                 contrasts = list(classgroup = CMC, step = CHAMC),
                                 data = sys_mean_happiness, REML = T)
summary(lmm_happiness_mean)
vif(lmm_happiness_mean)

describe(sys_mean_sadness)
lmm_sadness_mean <- lmer(sys_mean_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                               contrasts = list(classgroup = CMC, step = CSAMC),
                               data = sys_mean_sadness, REML = T)
summary(lmm_sadness_mean)
vif(lmm_sadness_mean)

describe(sys_mean_surprise)
lmm_surprise_mean <- lmer(sys_mean_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                                contrasts = list(classgroup = CMC, step = CSUMC),
                                data = sys_mean_surprise, REML = T)
summary(lmm_surprise_mean)
vif(lmm_surprise_mean)

describe(sys_mean_fear)
lmm_fear_mean <- lmer(sys_mean_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                            contrasts = list(classgroup = CMC, step = CFEMC),
                            data = sys_mean_fear, REML = T)
summary(lmm_fear_mean)
vif(lmm_fear_mean)

describe(sys_mean_disgust)
lmm_disgust_mean <- lmer(sys_mean_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                               contrasts = list(classgroup = CMC, step = CDIMC),
                               data = sys_mean_disgust, REML = T)
summary(lmm_disgust_mean)
vif(lmm_disgust_mean)

describe(sys_mean_anger)
lmm_anger_mean <- lmer(sys_mean_score ~ ct + experience + classgroup*step*efficacy_sd + (1|id),
                             contrasts = list(classgroup = CMC, step = CANMC),
                             data = sys_mean_anger, REML = T)
summary(lmm_anger_mean)
vif(lmm_anger_mean)

########################5-10系統情緒時間比例交互作用事後比較############################

#happiness
happiness_class_efficacy = plot_model(lmm_happiness_percentage, type = "emm", terms = c("efficacy_sd", "classgroup"),axis.title = "happiness_score", 
                                  title = "Interaction of Classroom goal struture groups and efficacy in happiness emotion percentage",dot.size=10)
happiness_class_efficacy

lmm_happiness_percentage_2 <- lmer(sys_percentage_score ~ ct + experience + 
                                     classgroup*efficacy_sd + (1|id),
                                 #contrasts = list(classgroup = CMC, step = CHAC),
                                 data = sys_percentage_happiness, REML = T)
simple_slopes(lmm_happiness_percentage_2)

#anger
anger_class_efficacy = plot_model(lmm_anger_percentage, type = "emm", terms = c("efficacy_sd", "classgroup"),axis.title = "anger_score", 
                                      title = "Interaction of Classroom goal struture groups and efficacy in anger emotion percentage",dot.size=10)
anger_class_efficacy
lmm_anger_percentage_2 <- lmer(sys_percentage_score ~ ct + experience + 
                                 classgroup*efficacy_sd + (1|id),
                             #contrasts = list(classgroup = CMC, step = CANC),
                             data = sys_percentage_anger, REML = T)
simple_slopes(lmm_anger_percentage_2)

########################5-11系統情緒平均信心值交互作用事後比較############################

#happiness
happiness_classefficacy_mean = plot_model(lmm_happiness_mean, type = "emm", terms = c("efficacy_sd", "classgroup"),axis.title = "happiness_score", 
                                      title = "Interaction of Classroom goal struture groups and efficacy in mean of happiness emotion",dot.size=10)
happiness_classefficacy_mean

happiness_classefficacy_mean_2 <- lmer(sys_mean_score ~ ct + experience + 
                                     classgroup*efficacy_sd + (1|id),
                                   #contrasts = list(classgroup = CMC, step = CHAMC),
                                   data = sys_mean_happiness, REML = T)

summary(happiness_classefficacy_mean_2)

simple_slopes(happiness_classefficacy_mean_2)

#anger
anger_classefficacy_mean = plot_model(lmm_anger_mean, type = "emm", terms = c("efficacy_sd", "classgroup"),axis.title = "anger_score", 
                                  title = "Interaction of Classroom goal struture groups and efficacy in mean of anger emotion",dot.size=10)
anger_classefficacy_mean
lmm_anger_mean_2 <- lmer(sys_mean_score ~ ct + experience + 
                                 classgroup*efficacy_sd + (1|id),
                               #contrasts = list(classgroup = CMC, step = CANMC),
                               data = sys_mean_anger, REML = T)
simple_slopes(lmm_anger_mean_2)

#-------------------------------PART6_問卷與系統相關---------------------------------------------------

video_percentage_cor <- model[,c(10,11,12,16,17,18,19,20,21)]
do_percentage_cor <- model[,c(13,14,15,22,23,24,25,26,27)]
#video_mean_cor <- model[,c(10,11,12,28,29,30,31,32,33)]
#do_mean_cor <- model[,c(13,14,15,34,35,36,37,38,39)]
video_mean_cor <- model[,c(10,11,12,31,32,33,30,29,28)] #換欄位位置
do_mean_cor <- model[,c(13,14,15,37,38,39,36,35,34)]  #換欄位位置

library(sandwich)
library(RcmdrMisc)
library(apaTables)
# 輸出相關文件
pearson_video_percentage <- rcorr.adjust(video_percentage_cor)
pearson_video_percentage
apa.cor.table(video_percentage_cor,filename = "C:/Users/0963625209/Desktop/碩論yo/分析/video_percentage_r.doc")

pearson_do_percentage <- rcorr.adjust(do_percentage_cor)
pearson_do_percentage
apa.cor.table(do_percentage_cor,filename = "C:/Users/0963625209/Desktop/碩論yo/分析/do_percentage_r.doc")

pearson_video_mean <- rcorr.adjust(video_mean_cor)
pearson_video_mean
apa.cor.table(video_mean_cor,filename = "C:/Users/0963625209/Desktop/碩論yo/分析/video_mean_revise.doc")

pearson_do_mean <- rcorr.adjust(do_mean_cor)
pearson_do_mean
apa.cor.table(do_mean_cor,filename = "C:/Users/0963625209/Desktop/碩論yo/分析/do_mean_revise.doc")
