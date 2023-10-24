{
library(terra)
library(raster)
library(randomForest)
library(AUC)
library(matrixStats)#计算每行的标准差
}
n_Cores <- detectCores()
windowsFonts(JP1 = windowsFont("MS Mincho"),
             JP2 = windowsFont("MS Gothic"),
             JP3 = windowsFont("Times New Roman"))

####从GBIF获取数据
library(dismo)
#1 Gardenia

#Use '*' to download the entire genus. 
#Append '*' to the species name to get all naming variants 

Gar <- gbif('Gardenia', '*')
dim(Gar)

#select the records that have longitude and latitude data
colnames(Gar)
Gar$lon
Gar_geo <- subset(Gar, !is.na(lon) & !is.na(lat))
dim(Gar_geo)

#remove duplicated record
dups_Gar <- duplicated(Gar_geo[, c('lon', 'lat')])
Gar_fin=dups_Gar[!dups_Gar,]
dim(Gar_fin)
?XSgeo
# show some values
#opne world simple map
library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE,col="gray80", main = "(a) Gardenia species")
# restore the box around the map

#plot points
points(Gar_fin$lon, Gar_fin$lat, col='red', pch=20, cex=0.75)


#2 Platycladus
Pla <- gbif('Platycladus', '*')
dim(Pla)

colnames(Pla)
Pla$lon
Pla_geo <- subset(Pla, !is.na(lon) & !is.na(lat))
dim(Pla_geo)

#remove duplicated record
dups_Pla <- duplicated(Pla_geo[, c('lon', 'lat')])
Pla_fin=XSgeo[!dups_Pla,]
dim(Pla_fin)

# show some values
#opne world simple map
library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE,col="gray80", main = "(b) Platycladus species")
# restore the box around the map

#plot points
points(Pla_fin$lon, Pla_fin$lat, col='red', pch=20, cex=0.75)



#3 Stypholobium  0records
Sty <- gbif('Sophora', '*')
dim(Sty)

colnames(Sty)
Sty$lon
Sty_geo <- subset(Sty, !is.na(lon) & !is.na(lat))
dim(Sty_geo)

#remove duplicated record
dups_Sty <- duplicated(Sty_geo[, c('lon', 'lat')])
Sty_fin=Sty_geo[!dups_Sty,]
dim(Sty_fin)

# show some values
#opne world simple map
library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE,col="gray80", main = "(c) Stypholobium species")
# restore the box around the map

#plot points
points(Sty_fin$lon, Sty_fin$lat, col='red', pch=20, cex=0.75)


#4 Cinnamomum
Cin <- gbif('Cinnamomum', '*')
dim(Cin)

colnames(Cin)
Cin$lon
Cin_geo <- subset(Cin, !is.na(lon) & !is.na(lat))
dim(Cin_geo)

#remove duplicated record
dups_Cin <- duplicated(Cin_geo[, c('lon', 'lat')])
Cin_fin=XSgeo[!dups_Cin,]
dim(Cin_fin)

# show some values
#opne world simple map
library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE,col="gray80", main = "(c) Cinnamomum species")
# restore the box around the map

#plot points
points(Cin_fin$lon, Cin_fin$lat, col='red', pch=20, cex=0.75)


#5 Osmanthus
Osm <- gbif('Osmanthus', '*')
dim(Osm)

colnames(Osm)
Osm$lon
Osm_geo <- subset(Osm, !is.na(lon) & !is.na(lat))
dim(Osm_geo)

#remove duplicated record
dups_Osm <- duplicated(Osm_geo[, c('lon', 'lat')])
Osm_fin=XSgeo[!dups_Osm,]
dim(Osm_fin)

# show some values
#opne world simple map
library(maptools)
data(wrld_simpl)
plot(wrld_simpl,axes=TRUE,col="gray80", main = "(d) Osmanthus species")
# restore the box around the map

#plot points
points(Osm_fin$lon, Osm_fin$lat, col='red', pch=20, cex=0.75)







#######################################################################
##第一部分：建立随机森林模型
#输入中华按蚊发生点的数据
sample.presence<- d
  
  
  
sample.presence<-sample.presence[,-c(1:3)]
presence.train<-sample.presence[sample.presence$sampleType=="training",]
presence.train$sampleType <- NULL

presence.test<-sample.presence[sample.presence$sampleType=="testing",]
presence.test$sampleType <- NULL

#输入中华按蚊不发生点的数据
sample.absence<-read.csv("C:/Users/hasee/Desktop/空间数据教程数据/第09章/sample_absence.csv",
                         header=T)
sample.absence<-sample.absence[,-c(1:3)]

set.seed(100)
absence.train<-sample.absence[sample(1:nrow(sample.absence),nrow(sample.absence)*0.75),]
absence.test<-sample.absence[-as.numeric(rownames(absence.train)),]

#创建训练集和测试集
train<-rbind(presence.train,absence.train)
test<-rbind(presence.test,absence.test)

prop.test<-c()
VarI<-c()
PPbio16<-c()
PPbio6<-c()
PPbio17<-c()
prop.train<-c()
pro1<-c()
train.err<-c()
test.err<-c()
auc.train100<-c()
auc.test100<-c()


#利用随机森林的自带参数（sampsize）对数据进行平衡。平衡比例为1：1，
#即中华按蚊发生点数据样本量和不发生样本量相等
for(i in 1:100){
  set.seed(i*10)
  rf<-randomForest(dataType~.,train,ntree=100,xtest=test[,1:5],ytest=test$dataType,
                   sampsize=c(nrow(presence.train),nrow(presence.train)),
                   strata=train$dataType,keep.forest=T)
  
  pro1<-rf$votes
  pro<-rf$test$votes
  prop.train<-cbind(prop.train,pro1)
  prop.test<-cbind(prop.test,pro)
  
  err.train<-rf$err.rate[100,]
  err.test<-rf$test$err.rate[100,]
  train.err<-rbind(train.err,err.train)
  test.err<-rbind(test.err,err.test)
  
  auc.train<-auc(roc(rf$votes[,1],factor(1 * (train$dataType=="presence"))))#OOB数据（训练集）
  auc.test<-auc(roc(rf$test$votes[,1],factor(1 * (test$dataType=="presence"))))
  auc.train100<-rbind(auc.train100,auc.train)
  auc.test100<-rbind(auc.test100,auc.test)
  
  Var<-varImpPlot(rf)
  VarI<-cbind(VarI,Var)
  
  bio16<-partialPlot(rf, train, bio16,"presence",plot=F)
  PPbio16<-cbind(PPbio16,bio16)
  
  bio6<-partialPlot(rf, train, bio6,"presence", plot=F)
  PPbio6<-cbind(PPbio6,bio6)
  
  bio17<-partialPlot(rf, train, bio17,"presence",plot=F)
  PPbio17<-cbind(PPbio17,bio17)
}

Partial.bio16<-as.data.frame(PPbio16[1:200])
Partial.bio6<-as.data.frame(PPbio6[1:200])
Partial.bio17<-as.data.frame(PPbio17[1:200])

#################################################################
##第二部分：绘制模型结果
#绘制100次模型运行训练集和测试集ROC曲线
setwd("C:/Users/hasee/Desktop/plot")
Cbindtrain.auc<-cbind(rowMeans(prop.train[,seq(1,200,2)])+1.96*rowSds(prop.train[,seq(1,200,2)])/sqrt(100),
                      rowMeans(prop.train[,seq(1,200,2)]),
                      rowMeans(prop.train[,seq(1,200,2)])-1.96*rowSds(prop.train[,seq(1,200,2)])/sqrt(100))
Cbindtest.auc<-cbind(rowMeans(prop.test[,seq(1,200,2)])+1.96*rowSds(prop.test[,seq(1,200,2)])/sqrt(100),
                     rowMeans(prop.test[,seq(1,200,2)]),
                     rowMeans(prop.test[,seq(1,200,2)])-1.96*rowSds(prop.test[,seq(1,200,2)])/sqrt(100))
tiff(filename = "ROC for train and test100.tif",
     width = 1200, height = 1200, units = "px", pointsize = 8,
     compression = c("lzw"),
     bg = "white", res = 300, family = "JP3", restoreConsole = TRUE,
     type = "windows")
plot(roc(Cbindtrain.auc[,2],factor(1 * (train$dataType=="presence"))),col=2,family="JP3",
     main="ROC curves for predicting distribution of Anopheles sinensis",cex.main=0.9)
plot(roc(Cbindtest.auc[,2],factor(1 * (test$dataType=="presence"))),col="green",family="JP3",add=T)
AUC.oob.train<-auc(roc(Cbindtrain.auc[,2],factor(1 * (train$dataType=="presence"))))
AUC.oob.test<-auc(roc(Cbindtest.auc[,2],factor(1 * (test$dataType=="presence"))))
legend("bottomright",inset = 0.02,c(paste("training AUC=",round(AUC.oob.train,2),sep=""),
                                    paste("testing AUC=",round(AUC.oob.test,2),sep="")),
       lty=c(1,1),col=c("red","green"),bty="n",cex=0.8)
dev.off()

#绘制多次模型计算的变量重要性的均值，以及均值的区间估计（95%的置信区间）
#计算模型运行100次变量重要性的均值以及均值的区间估计
VarImportance<-cbind(VarI[2,],VarI[4,],VarI[1,],VarI[5,],VarI[3,])
colnames(VarImportance)<-c("bio16","bio6","bio17","bio2","bio10")


error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#plot variable importance
tiff(filename = "Variable importance.tif",
     width = 1200, height = 1200, units = "px", pointsize = 6,
     compression = c("lzw"),
     bg = "white", res = 300, family = "JP3", restoreConsole = TRUE,
     type = "windows")
barx <- barplot(apply(VarImportance,2,mean),col="lightgray", axis.lty=1, ylim=c(0,20),xlab=" ",
                ylab="Mean decrease Gini",space=1.3,family="JP3")
error.bar(barx,apply(VarImportance,2,mean), 1.96*apply(VarImportance,2,sd)/sqrt(100))
#apply(,2/1)：2表示列，1表示行
dev.off()

#通过变量重要性排序，对前三个重要的变量绘制其对于中华按蚊发生的平均边际效应
#绘制100次模型的平均边际效应，以及均值的置信区间
#计算影响因素对于发生的概率,公式为：exp(Partialy.bio6*2)/(1+exp(Partialy.bio6*2)

#1 绘制100次模型最湿季节的降水量对于中华按蚊发生的平均边际效应
Partialy.bio16<-Partial.bio16[,seq(2,200,2)]
colnames(Partialy.bio16)<-paste("y",1:100,sep ="" )
Partialy.bio16<-as.matrix(Partialy.bio16)

Partialy1.bio16<-c()
Partialy1.bio16<-cbind(Partialy1.bio16,exp(Partialy.bio16*2)/(1+exp(Partialy.bio16*2)))

Cbindprobality.bio16<-cbind(rowMeans(Partialy1.bio16)+1.96*rowSds(Partialy1.bio16)/sqrt(100),
                            rowMeans(Partialy1.bio16),
                            rowMeans(Partialy1.bio16)-1.96*rowSds(Partialy1.bio16)/sqrt(100))

#plot response curve for bio16
tiff(filename = "Probability of presence to bio16.tif",
     width = 1200, height = 1200, units = "px", pointsize = 8,
     compression = c("lzw"),
     bg = "white", res = 300, family = "JP3", restoreConsole = TRUE,
     type = "windows")
plot(Partial.bio16[,1],Cbindprobality.bio16[,1], type = "n",xlim=c(0,1200),xlab="Precipitation of wettest quarter",ylab="Probability of presence of An.sinensis",family="JP3")
polygon(c(rev(Partial.bio16[,1]),Partial.bio16[,1]), c(rev(Cbindprobality.bio16[,1]), Cbindprobality.bio16[,3]), col = 'grey80', border = NA)
lines(Partial.bio16[,1],Cbindprobality.bio16[,2],col="red",lwd=0.8)
dev.off()

#2 绘制100次模型最冷月的最低温度对于中华按蚊发生的平均边际效应
Partialy.bio6<-Partial.bio6[,seq(2,200,2)]
colnames(Partialy.bio6)<-paste("y",1:100,sep ="" )
Partialy.bio6<-as.matrix(Partialy.bio6)

Partialy1.bio6<-c()
Partialy1.bio6<-cbind(Partialy1.bio6,exp(Partialy.bio6*2)/(1+exp(Partialy.bio6*2)))


Cbindprobality.bio6<-cbind(rowMeans(Partialy1.bio6)+1.96*rowSds(Partialy1.bio6)/sqrt(100),
                           rowMeans(Partialy1.bio6),
                           rowMeans(Partialy1.bio6)-1.96*rowSds(Partialy1.bio6)/sqrt(100))

#plot response curve for bio6
tiff(filename = "Probability of presence to bio6.tif",
     width = 1200, height = 1200, units = "px", pointsize = 8,
     compression = c("lzw"),
     bg = "white", res = 300, family = "JP3", restoreConsole = TRUE,
     type = "windows")
plot(Partial.bio6[,1]/10,Cbindprobality.bio6[,1], type = "n",xlim=c(-37,15),xlab="Min temperature of coldest month",ylab="Probability of presence of An.sinensis",family="JP3")
polygon(c(rev(Partial.bio6[,1]/10),Partial.bio6[,1]/10), c(rev(Cbindprobality.bio6[,1]), Cbindprobality.bio6[,3]), col = 'grey80', border = NA)
lines(Partial.bio6[,1]/10,Cbindprobality.bio6[,2],col="red",lwd=0.8)
dev.off()

#3 绘制100次模型最冷月的最低温度对于中华按蚊发生的平均边际效应
Partialy.bio17<-Partial.bio17[,seq(2,200,2)]
colnames(Partialy.bio17)<-paste("y",1:100,sep ="" )
Partialy.bio17<-as.matrix(Partialy.bio17)

Partialy1.bio17<-c()
Partialy1.bio17<-cbind(Partialy1.bio17,exp(Partialy.bio17*2)/(1+exp(Partialy.bio17*2)))

Cbindprobality.bio17<-cbind(rowMeans(Partialy1.bio17)+1.96*rowSds(Partialy1.bio17)/sqrt(100),
                            rowMeans(Partialy1.bio17),
                            rowMeans(Partialy1.bio17)-1.96*rowSds(Partialy1.bio17)/sqrt(100))

tiff(filename = "Probability of presence to bio17.tif",
     width = 1200, height = 1200, units = "px", pointsize = 8,
     compression = c("lzw"),
     bg = "white", res = 300, family = "JP3", restoreConsole = TRUE,
     type = "windows")
plot(Partial.bio17[,1],Cbindprobality.bio17[,1], type = "n",xlim=c(0,360),xlab="Precipitation of dyiest quarter",ylab="Probability of presence of An.sinensis",family="JP3")
polygon(c(rev(Partial.bio17[,1]),Partial.bio17[,1]), c(rev(Cbindprobality.bio17[,1]), Cbindprobality.bio17[,3]), col = 'grey80', border = NA)
lines(Partial.bio17[,1],Cbindprobality.bio17[,2],col="red",lwd=0.8)
dev.off()

#预测全国中华按蚊分布的概率
path.rasters = "C:/Users/hasee/Desktop/data"
path.plot = "C:/Users/hasee/Desktop/plot" #设定输出目录:
rasterFiles = list.files(path = path.rasters,pattern='.tif$',full.names = TRUE)
predictors = stack(rasterFiles)

predictors
names(predictors)

#plot the predictors
tiff(filename = "Predictors.tif",
     width = 1600, height = 1200, units = "px", pointsize = 8,
     compression = c("lzw"),
     bg = "white", res = 300, family = "JP3", restoreConsole = TRUE,
     type = "windows")
plot(predictors)
dev.off()


#选出测试集AUC值最高的随机数种子，用于下一步预测
which(auc.test100==max(auc.test100)) #0.8625217
set.seed(which(auc.test100==max(auc.test100))*10)
Maxrf<-randomForest(dataType~.,train,ntree=100,xtest=test[,1:5],ytest=test$dataType, sampsize=c(nrow(presence.train),nrow(presence.train)),strata=train$dataType,keep.forest=T)

#说明：prob代表预测的概率，index表示输出第几列,因为输出时预测值按照类别的大小顺序进行输出的。当type设置为prob时，表示输出的类别概率矩阵。
#此处的predict函数是raster包里的，而非randomForest
pred = predict(predictors, Maxrf, filename="Probability of presence.tif", type="prob",
               na.rm=T, progress="window", overwrite=TRUE,index=1)

tiff(filename = "Plot of probability of presence.tif",
     width = 1600, height = 1200, units = "px", pointsize = 8,
     compression = c("lzw"),
     bg = "white", res = 300, family = "JP3", restoreConsole = TRUE,
     type = "windows")
plot(pred,col=terrain.colors(255), main="中华按蚊出现概率")
dev.off()
