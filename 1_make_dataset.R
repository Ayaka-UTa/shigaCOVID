#####
#xlsxフォルダにおいたファイルを全て読み込んでデータセットをつくる
#事前にテーブルの数を揃えておく(自動でやってるのでたまに分離している)
#2021/4/21 cording by Aya U.Ta.
#かかった時間(4)
#####
setwd("../shigaCOVID")
library(xlsx)
hizuke <- read.csv("hizuke.csv",header=F)#日付データ修正用ファイル読み込み
population <- read.csv("population.csv",header=F)#人口ファイル読み込み
setwd("../xlsx")
filename <- dir()
date <- seq(as.Date("2021-03-26"), as.Date("2021-05-14"), by = "day")#取得ファイルの発表日のリストを作る
cat(length(filename), length(date)) #日付とファイルの数があっているかcheck

prlmData <- svltData <- Data <- NULL

for(i in 1:length(date)){
  sheet1 <- read.xlsx(filename[i],sheetIndex=1) #速報
  if(ncol(sheet1)==11){sheet1 <- cbind(sheet1,NA); sheet1 <- sheet1[c(1:10,12,11)]}#11列しかないときは10列目の後ろに空の列を追加
  annDate <- rep(date[i], nrow(sheet1))#発表日のカラムを追加する
  sheet1 <- cbind(sheet1, annDate)
  colnames(sheet1) <- c("patientNo","age","sex","city", "job","onsetDate","consulDate","posiconDate","route","contact", "cluster","contactFor","annDate")#列名を英語にしておく
  prlmData <- rbind(prlmData, sheet1)
  
  sheet2 <- read.xlsx(filename[i],sheetIndex=2) #重症度
  annDate <- rep(date[i],nrow(sheet2))#pdfの日付のカラムを追加する
  sheet2 <- cbind(sheet2, annDate)
  if(ncol(sheet2)>5)sheet2 <- sheet2[c(1:3,5,6)]#5241475にだけ退院という項目がある
  svltData <- rbind(svltData, sheet2)
  
  sheet3 <- read.xlsx(filename[i],sheetIndex=3) #２週間分のまとめ
  if(ncol(sheet3)==12){sheet3 <- cbind(sheet3,NA); sheet3 <- sheet3[c(1:11,13,12)]}#12列しかないときは10列目の後ろに空の列を追加
  annDate <- rep(date[i],nrow(sheet2))#発表日のカラムを追加する
  sheet3 <- cbind(sheet3, annDate)
  colnames(sheet3) <- c("patientNo","announce","age","sex","city", "job","onsetDate","consulDate","posiconDate","route","contact", "cluster","contactFor","annDate")#列名を英語にしておく
  Data <- rbind(Data, sheet3)
}

####svltData重症度データのクリーニング(単位(人)を除く)



####Dataのクリーニング
###所々列名が行に紛れ込んでるので除く
Data[,1] <- as.numeric(Data[,1])
Data <- Data[-which(is.na(Data[,1])),]

###クラスター情報の分離がうまく行ってないページがあるのでなんとかする
for(n in 1:nrow(Data)){
  if(nchar(Data[n,11] > 4)){
    Data[n,12] <- substr(Data[n,11], 8,20)#クラスター情報を12列目へ移動(間の空白の数が揃ってないっぽい)
    Data[n,11] <- substr(Data[n,11], 1,2)#接触情報は2文字以内とする
  }
}

###古いデータと更新済みのデータが混じっているので更新済みのデータだけData2にまとめる
Data2 <- NULL
patients <-unique(Data$patientNo)
for(p in 1:length(patients)){
  Data2 <- rbind(Data2,Data[which(Data[,1]==patients[p]),][sum(Data[,1]==patients[p]),])
}

###Data2に速報をくっつける
addData <- subset(prlmData,annDate==max(date))[,1]
addData <- cbind(addData, as.character(max(date)))
colnames(addData)<- c("patientNo","announce")
addData <- cbind(addData,subset(prlmData,annDate==max(date))[,2:13])
addData$patientNo <- as.numeric(addData$patientNo)
##クラスター情報の分離がうまく行ってないページがあるのでなんとかする
for(n in 1:nrow(addData)){
  if(nchar(addData[n,11] > 4)){
    addData[n,12] <- substr(addData[n,11], 5,20)#クラスター情報を12列目へ移動(間の空白の数が揃ってないっぽい)
    addData[n,11] <- substr(addData[n,11], 1,2)#接触情報は2文字以内とする
  }
}
Data2 <-rbind(Data2, addData)

###日付データを整える(元ファイルはエクセルで作った方が早い8/1までなので注意)
for(d in 1:nrow(Data2)){
  if(length(which(Data2[d,2]==hizuke[1]))!=0)Data2[d,2] <- hizuke[which(Data2[d,2]==hizuke[1]),2]
  if(length(which(Data2[d,7]==hizuke[1]))!=0)Data2[d,7]<- hizuke[which(Data2[d,7]==hizuke[1]),2]
  if(length(which(Data2[d,8]==hizuke[1]))!=0)Data2[d,8]<- hizuke[which(Data2[d,8]==hizuke[1]),2]
  if(length(which(Data2[d,9]==hizuke[1]))!=0) Data2[d,9]<- hizuke[which(Data2[d,9]==hizuke[1]),2]
}

###Data2の型指定
Data2$city <- factor(Data2$city, levels = c("大津市", "草津市", "守山市","栗東市", "野洲市", "甲賀市","湖南市", "東近江市","近江八幡市","日野町","竜王町", "彦根市","愛荘町","豊郷町","甲良町","多賀町","米原市","長浜市","高島市","県外"))
Data2$announce <- as.Date(Data2$announce)

filename  <- substr(filename,1,7)
filename <- cbind(filename,date)

setwd("../shigaCOVID")
write.csv(prlmData,"prlmData.csv",row.names=FALSE)#速報のまとめ
write.csv(svltData,"svltData.csv",row.names=FALSE)#重症度のまとめ
write.csv(Data,"Data.csv",row.names=FALSE)#過去２週間分データの更新分まとめ
write.csv(Data2,"Data2.csv",row.names=FALSE)#速報と過去データのダブりをなくした完全版
write.csv(filename,"filename.csv",row.names=FALSE)#ファイル名と日付のメタデータ
#save.image("shigaCOVID.Rdata")
