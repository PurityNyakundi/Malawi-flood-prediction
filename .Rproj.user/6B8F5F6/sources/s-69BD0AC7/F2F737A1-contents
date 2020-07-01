library(caret)
library(xgboost)
library(tidyverse)
library(ggplot2)
library(GGally)
Train<-Train <- read_csv("D:/codebase/malawi zindi/Train.csv")
dim(Train)
test <- read_csv("D:/codebase/malawi zindi/test.csv")
dim(test)
names(Train)<-c("x","y","target_2015","elevation","prec1","prec2","prec3","prec4","prec5","prec6","prec7","prec8","prec6",
                "prec10","prec11","prec12","prec13","prec14","prec15","prec16","prec17","Lc_mode","Square_ID  ")
names(Train)
names(test)<-c("x","y","target_2015","elevation","prec1","prec2","prec3","prec4","prec5","prec6","prec7","prec8","prec6",
                "prec10","prec11","prec12","prec13","prec14","prec15","prec16","prec17","Lc_mode","Square_ID  ")

colSums(is.na(Train))
colSums(is.na(test))

view(Train)
view(test)


#dim(data2)
#view(data2)
#data2 = data2[,-c(45,23)]
#str(data2)
ggcorr(Train,label = F,palette = "RdBu",name = "cor")
ggcorr(test,label = F,palette = "RdBu",name = "Cor")
#datax<-c("")
#ggduo(data2,,columnsX = )

Train1 = Train[,-23]
#view(Train)
test1= test[,-23]
library(Matrix)
xg
#dtrain<-xgb.DMatrix(as.matrix(Train1)),label = target_2015
#dtest<-xgb.DMatrix(as.matrix(test1))
#y_train<-test[,3]
#y_train
bst<-xgboost(data = dtrain,
             label =as.numeric(Train1$target_2015),max_depth =4,eta = 1,nthread = 2,nrounds = 10,objective = "reg:linear")

target_2019 <-predict(bst,dtest)

target_2019= round(target_2019,3)

tail(target_20191)
dtrain <- xgb.DMatrix(as.matrix(Train1), 
                      label = as.numeric(Train1$target_2015))
dtest <- xgb.DMatrix(as.matrix(test1), 
                     label = as.numeric(test1$target_2015))

params <- list(max_depth = 3, 
               objective = "reg:linear",
               silent = 0)

watchlist <- list(train = dtrain, eval = dtest)

bst <- xgb.train(params = params, data = dtrain, nrounds = 5, nthread = 2)
# Model accuracy with new features
accuracy.after <- sum((predict(bst, dtest) == test1$target_2015) /
  length(test1$target_2015))

# Here the accuracy was already good and is now perfect.
cat(paste("The accuracy was before adding leaf features and it is now",
          accuracy.after, "!\n"))

bst <- xgb.train(data=dtrain, 
                 booster = "gblinear",
                 max_depth=5, 
                 nthread = 2, 
                 nrounds=2, 
                 watchlist=watchlist, eval_metric = "error", eval_metric = "logloss", objective = "reg:linear")


target_20191<-predict(bst,dtest)

head(target_20191)

target_2019<-round(target_20191,3)
head(target_2019)

Square_ID= test[,23]
sub<-cbind(Square_ID,target_2019)
write.csv(sub,file = "SampleSubmission.csv",row.names = F)
SampleSubmission.csv<-read.csv("SampleSubmission.csv",header = T)
view(SampleSubmission.csv)

library(readr)
SampleSubmission <- read_csv("SampleSubmission.csv")
View(SampleSubmission)




library(xgboost)

xgboost_model <- xgboost(data = as.matrix(Train1), 
                         label = as.numeric(Train1$target_2015),
                         max_depth = 3, 
                         objective = "reg:linear", 
                         nrounds = 10, 
                         verbose = FALSE,
                         prediction = TRUE)
xgboost_model
summary(xgboost_model)

target_20192 = round(predict(xgboost_model, 
        as.matrix(test1)),3) 

target_20192

Square_ID= test[,23]
sub<-cbind(Square_ID,target_2019)
write.csv(sub,file = "SampleSubmission.csv",row.names = F)
SampleSubmission.csv<-read.csv("SampleSubmission.csv",header = T)
view(SampleSubmission.csv)

library(readr)
SampleSubmission <- read_csv("SampleSubmission.csv")
View(SampleSubmission)



  as.tibble() %>%
  mutate(target = value,
         label = as.numeric(test1$target_2015)) %>%
  count(prediction, label)


xgb.params<-list(
  colsample_bytree = 0.7,
  subsample =0.7,
  max_depth = 5,
  booster = "gbtree",
  #eval_metric = "rsme",
  objective = "reg:linear",
  gamma = 0
)
gd_dt = xgb.train(xgb.params,dtrain,nrounds = 54)
gd_dt