library(rio);library(tidyverse);library(randomForest);library(e1071);library(gbm)
#upland
upland_train <- import('upland_train.xlsx',which = 'train')
head(upland_train,3)
upland_test <- import('upland_test.xlsx',which = 'test')
head(upland_test,3)
##Randomforest
upland_rf <- randomForest(EF~N_input+W+MAT+PH+CN+ST,
                          data =upland_train, mtry=3, ntree = 500)
upland_train_rf <- predict(upland_rf,newdata = upland_train)
upland_train_rf <- as.data.frame(upland_train_rf)
upland_test_rf <- predict(upland_rf,newdata = upland_test)
upland_test_rf <- as.data.frame(upland_test_rf)
par(mfrow = c(1,2))
plot(upland_train$EF,upland_train_rf$upland_train_rf)
plot(upland_test$EF,upland_test_rf$upland_test_rf)
##GBM
upland_gbm <- gbm(EF~N_input+W+MAT+PH+CN+ST,         # formula
                  data=upland_train,                   # dataset
                  distribution="gaussian",     # see the help for other choices
                  n.trees=1000,                # number of trees
                  shrinkage=0.05,              # shrinkage or learning rate,
                  # 0.001 to 0.1 usually work
                  interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
                  bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
                  train.fraction = 1,        # fraction of data for training,
                  # first train.fraction*N used for training
                  #n.minobsinnode = 10,         # minimum total weight needed in each node
                  cv.folds = 3,                # do 3-fold cross-validation
                  keep.data=TRUE,              # keep a copy of the dataset with the object
                  verbose=FALSE)
upland_train_gbm <- predict(upland_gbm,upland_train,1000)
upland_train_gbm <- as.data.frame(upland_train_gbm)
upland_test_gbm <- predict(upland_gbm,upland_test,1000)
upland_test_gbm <- as.data.frame(upland_test_gbm)
plot(upland_train$EF,upland_train_gbm$upland_train_gbm)
plot(upland_test$EF,upland_test_gbm$upland_test_gbm)
##RBF
upland_tuned<-tune.svm(EF~N_input+W+MAT+PH+CN+ST,
                       data = upland_train,gamma = 10^(-3:2),cost=10^(-3:4))
upland_tuned 
upland_svm<-svm(EF~N_input+W+MAT+PH+CN+ST,
                data = upland_train,gamma=1,cost=1,kernel="radial")

upland_train_svm <- predict(upland_svm,newdata=upland_train)
upland_test_svm <- predict(upland_svm,newdata=upland_test)
upland_train_svm <- as.data.frame(upland_train_svm)
upland_test_svm <- as.data.frame(upland_test_svm)
plot(upland_train$EF,upland_train_svm$upland_train_svm)
plot(upland_test$EF,upland_test_svm$upland_test_svm)
upland_train_all <- cbind(upland_train$EF,upland_train_rf,upland_train_gbm,upland_train_svm)
head(upland_train_all,3)
upland_test_all <- cbind(upland_test$EF,upland_test_rf,upland_test_gbm,upland_test_svm)
export(upland_train_all,'upland_train_all.xlsx')
export(upland_test_all,'upland_test_all.xlsx')

#rice
rice_train <- import('rice_train.xlsx',which = 'train')
head(rice_train,3)
rice_test <- import('rice_test.xlsx',which = 'test')
head(rice_test,3)
##Randomforest
rice_rf <- randomForest(EF~N_input+W+MAT+PH+CN+ST,
                        data =rice_train, mtry=3, ntree = 500)
rice_train_rf <- predict(rice_rf,newdata = rice_train)
rice_train_rf <- as.data.frame(rice_train_rf)
rice_test_rf <- predict(rice_rf,newdata = rice_test)
rice_test_rf <- as.data.frame(rice_test_rf)
par(mfrow = c(1,2))
plot(rice_train$EF,rice_train_rf$rice_train_rf)
plot(rice_test$EF,rice_test_rf$rice_test_rf)
##GBM
rice_gbm <- gbm(EF~N_input+W+MAT+PH+CN+ST,         # formula
                data=rice_train,                   # dataset
                distribution="gaussian",     # see the help for other choices
                n.trees=1000,                # number of trees
                shrinkage=0.05,              # shrinkage or learning rate,
                # 0.001 to 0.1 usually work
                interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
                bag.fraction = 1,          # subsampling fraction, 0.5 is probably best
                train.fraction = 0.8,        # fraction of data for training,
                # first train.fraction*N used for training
                n.minobsinnode = 10,         # minimum total weight needed in each node
                #cv.folds = 3,                # do 3-fold cross-validation
                keep.data=TRUE,              # keep a copy of the dataset with the object
                verbose=FALSE)
###
rice_train_gbm <- predict(rice_gbm,rice_train,1000)
rice_train_gbm <- as.data.frame(rice_train_gbm)
rice_test_gbm <- predict(rice_gbm,rice_test,1000)
rice_test_gbm <- as.data.frame(rice_test_gbm)
plot(rice_train$EF,rice_train_gbm$rice_train_gbm)
plot(rice_test$EF,rice_test_gbm$rice_test_gbm)
##RBF
rice_tuned<-tune.svm(EF~N_input+W+MAT+PH+CN+ST,
                     data = rice_train,gamma = 10^(-3:2),cost=10^(-3:4))
rice_tuned #
rice_svm<-svm(EF~N_input+W+MAT+PH+CN+ST,
              data = rice_train,gamma=0.1,cost=1,kernel="radial")

rice_train_svm <- predict(rice_svm,newdata=rice_train)
rice_test_svm <- predict(rice_svm,newdata=rice_test)
rice_train_svm <- as.data.frame(rice_train_svm)
rice_test_svm <- as.data.frame(rice_test_svm)
plot(rice_train$EF,rice_train_svm$rice_train_svm)
plot(rice_test$EF,rice_test_svm$rice_test_svm)
rice_train_all <- cbind(rice_train$EF,rice_train_rf,rice_train_gbm,rice_train_svm)
head(rice_train_all,3)
rice_test_all <- cbind(rice_test$EF,rice_test_rf,rice_test_gbm,rice_test_svm)
export(rice_train_all,'rice_train_all.xlsx')
export(rice_test_all,'rice_test_all.xlsx')

#BNE
bne_train <- import('bne_train.xlsx',which = 'train')
head(bne_train,3)
bne_test <- import('bne_test.xlsx',which = 'test')
head(bne_test,3)
##Randomforest
bne_rf <- randomForest(NO~N_input+W+MAT+PH+CN+ST,
                       data =bne_train, mtry=3, ntree = 500)
bne_train_rf <- predict(bne_rf,newdata = bne_train)
bne_train_rf <- as.data.frame(bne_train_rf)
bne_test_rf <- predict(bne_rf,newdata = bne_test)
bne_test_rf <- as.data.frame(bne_test_rf)
par(mfrow = c(1,2))
plot(bne_train$NO,bne_train_rf$bne_train_rf)
plot(bne_test$NO,bne_test_rf$bne_test_rf)
##GBM
bne_gbm <- gbm(NO~N_input+W+MAT+PH+CN+ST,         # formula
               data=bne_train,                   # dataset
               distribution="gaussian",     # see the help for other choices
               n.trees=1000,                # number of trees
               shrinkage=0.05,              # shrinkage or learning rate,
               # 0.001 to 0.1 usually work
               interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
               bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
               train.fraction = 1,        # fraction of data for training,
               # first train.fraction*N used for training
               #n.minobsinnode = 10,         # minimum total weight needed in each node
               cv.folds = 3,                # do 3-fold cross-validation
               keep.data=TRUE,              # keep a copy of the dataset with the object
               verbose=FALSE)

bne_train_gbm <- predict(bne_gbm,bne_train,1000)
bne_train_gbm <- as.data.frame(bne_train_gbm)
bne_test_gbm <- predict(bne_gbm,bne_test,1000)
bne_test_gbm <- as.data.frame(bne_test_gbm)
plot(bne_train$NO,bne_train_gbm$bne_train_gbm)
plot(bne_test$NO,bne_test_gbm$bne_test_gbm)
##RBF
bne_tuned<-tune.svm(NO~N_input+W+MAT+PH+CN+ST,
                    data = bne_train,gamma = 10^(-3:2),cost=10^(-3:4))
bne_tuned #
bne_svm<-svm(NO~N_input+W+MAT+PH+CN+ST,
             data = bne_train,gamma=0.1,cost=10,kernel="radial")

bne_train_svm <- predict(bne_svm,newdata=bne_train)
bne_test_svm <- predict(bne_svm,newdata=bne_test)
bne_train_svm <- as.data.frame(bne_train_svm)
bne_test_svm <- as.data.frame(bne_test_svm)
plot(bne_train$NO,bne_train_svm$bne_train_svm)
plot(bne_test$NO,bne_test_svm$bne_test_svm)
bne_train_all <- cbind(bne_train$NO,bne_train_rf,bne_train_gbm,bne_train_svm)
head(bne_train_all,3)
bne_test_all <- cbind(bne_test$NO,bne_test_rf,bne_test_gbm,bne_test_svm)
export(bne_train_all,'bne_train_all.xlsx')
export(bne_test_all,'bne_test_all.xlsx')

#grassland
grassland_train <- import('grassland_train.xlsx',which = 'train')
head(grassland_train,3)
grassland_test <- import('grassland_test.xlsx',which = 'test')
head(grassland_test,3)
##Randomforest
grassland_rf <- randomForest(NO~N_input+W+MAT+PH+CN+ST,
                             data =grassland_train, mtry=3, ntree = 500)
grassland_train_rf <- predict(grassland_rf,newdata = grassland_train)
grassland_train_rf <- as.data.frame(grassland_train_rf)
grassland_test_rf <- predict(grassland_rf,newdata = grassland_test)
grassland_test_rf <- as.data.frame(grassland_test_rf)
par(mfrow = c(1,2))
plot(grassland_train$NO,grassland_train_rf$grassland_train_rf)
plot(grassland_test$NO,grassland_test_rf$grassland_test_rf)
##GBM
grassland_gbm <- gbm(NO~N_input+W+MAT+PH+CN+ST,         # formula
                     data=grassland_train,                   # dataset
                     distribution="gaussian",     # see the help for other choices
                     n.trees=1000,                # number of trees
                     shrinkage=0.05,              # shrinkage or learning rate,
                     # 0.001 to 0.1 usually work
                     interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
                     bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
                     train.fraction = 1,        # fraction of data for training,
                     # first train.fraction*N used for training
                     #n.minobsinnode = 10,         # minimum total weight needed in each node
                     cv.folds = 3,                # do 3-fold cross-validation
                     keep.data=TRUE,              # keep a copy of the dataset with the object
                     verbose=FALSE)
###
grassland_train_gbm <- predict(grassland_gbm,grassland_train,1000)
grassland_train_gbm <- as.data.frame(grassland_train_gbm)
grassland_test_gbm <- predict(grassland_gbm,grassland_test,1000)
grassland_test_gbm <- as.data.frame(grassland_test_gbm)
plot(grassland_train$NO,grassland_train_gbm$grassland_train_gbm)
plot(grassland_test$NO,grassland_test_gbm$grassland_test_gbm)
##RBF
grassland_tuned<-tune.svm(NO~N_input+W+MAT+PH+CN+ST,
                          data = grassland_train,gamma = 10^(-3:2),cost=10^(-3:4))
grassland_tuned 
grassland_svm<-svm(NO~N_input+W+MAT+PH+CN+ST,
                   data = grassland_train,gamma=1,cost=1,kernel="radial")

grassland_train_svm <- predict(grassland_svm,newdata=grassland_train)
grassland_test_svm <- predict(grassland_svm,newdata=grassland_test)
grassland_train_svm <- as.data.frame(grassland_train_svm)
grassland_test_svm <- as.data.frame(grassland_test_svm)
plot(grassland_train$NO,grassland_train_svm$grassland_train_svm)
plot(grassland_test$NO,grassland_test_svm$grassland_test_svm)
grassland_train_all <- cbind(grassland_train$NO,grassland_train_rf,grassland_train_gbm,grassland_train_svm)
head(grassland_train_all,3)
grassland_test_all <- cbind(grassland_test$NO,grassland_test_rf,grassland_test_gbm,grassland_test_svm)
export(grassland_train_all,'grassland_train_all.xlsx')
export(grassland_test_all,'grassland_test_all.xlsx')

#forest
forest_train <- import('forest_train.xlsx',which = 'train')
head(forest_train,3)
forest_test <- import('forest_test.xlsx',which = 'test')
head(forest_test,3)
##Randomforest
forest_rf <- randomForest(NO~N_input+W+MAT+PH+CN+ST,
                          data =forest_train, mtry=3, ntree = 500)
forest_train_rf <- predict(forest_rf,newdata = forest_train)
forest_train_rf <- as.data.frame(forest_train_rf)
forest_test_rf <- predict(forest_rf,newdata = forest_test)
forest_test_rf <- as.data.frame(forest_test_rf)
par(mfrow = c(1,2))
plot(forest_train$NO,forest_train_rf$forest_train_rf)
plot(forest_test$NO,forest_test_rf$forest_test_rf)
##GBM
forest_gbm <- gbm(NO~N_input+W+MAT+PH+CN+ST,         # formula
                  data=forest_train,                   # dataset
                  distribution="gaussian",     # see the help for other choices
                  n.trees=1000,                # number of trees
                  shrinkage=0.05,              # shrinkage or learning rate,
                  # 0.001 to 0.1 usually work
                  interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
                  bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
                  train.fraction = 1,        # fraction of data for training,
                  # first train.fraction*N used for training
                  #n.minobsinnode = 10,         # minimum total weight needed in each node
                  cv.folds = 3,                # do 3-fold cross-validation
                  keep.data=TRUE,              # keep a copy of the dataset with the object
                  verbose=FALSE)
###
forest_train_gbm <- predict(forest_gbm,forest_train,1000)
forest_train_gbm <- as.data.frame(forest_train_gbm)
forest_test_gbm <- predict(forest_gbm,forest_test,1000)
forest_test_gbm <- as.data.frame(forest_test_gbm)
plot(forest_train$NO,forest_train_gbm$forest_train_gbm)
plot(forest_test$NO,forest_test_gbm$forest_test_gbm)
##RBF
forest_tuned<-tune.svm(NO~N_input+W+MAT+PH+CN+ST,
                       data = forest_train,gamma = 10^(-3:2),cost=10^(-3:4))
forest_tuned #
forest_svm<-svm(NO~N_input+W+MAT+PH+CN+ST,
                data = forest_train,gamma=1,cost=1000,kernel="radial")

forest_train_svm <- predict(forest_svm,newdata=forest_train)
forest_test_svm <- predict(forest_svm,newdata=forest_test)
forest_train_svm <- as.data.frame(forest_train_svm)
forest_test_svm <- as.data.frame(forest_test_svm)
plot(forest_train$NO,forest_train_svm$forest_train_svm)
plot(forest_test$NO,forest_test_svm$forest_test_svm)
forest_train_all <- cbind(forest_train$NO,forest_train_rf,forest_train_gbm,forest_train_svm)
head(forest_train_all,3)
forest_test_all <- cbind(forest_test$NO,forest_test_rf,forest_test_gbm,forest_test_svm)
export(forest_train_all,'forest_train_all.xlsx')
export(forest_test_all,'forest_test_all.xlsx')
