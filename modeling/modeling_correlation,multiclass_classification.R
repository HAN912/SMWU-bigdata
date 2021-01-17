library(xlsx)
library(dplyr)
library(caret)

movie<-read.xlsx("C:\\Users\\tkrhk\\Desktop\\real_final3.xlsx", sheetIndex = 1, encoding = "UTF-8")
movie<-movie[,-1]


#상관관계
movie$log누적관객수<-log(movie$누적관객수)
cor(movie[,c(40,4,5,6,7,33)])

#산점도 
pairs(movie[,c(40,4,5,6,7,33)]) 

table(movie$y)

#multi-class classification

#4-class
#0:100만 미만, 1:100만이상 300미만, 2: 300만 이상 500만 미만 3: 500이상
#movie$y<-ifelse(movie$누적관객수<1000000,0, ifelse(movie$누적관객수<3000000,1,ifelse(movie$누적관객수<5000000,2,3)))

#classification에 필요한 변수들만 따로 저장 
movie_d<-movie[,c(-1,-2,-3)]
str(movie_d)

#factor로 변환 
movie_d$y<-factor(movie$y)
movie_d$국적<-factor(movie_d$국적)
movie_d$배급사<-factor(movie_d$배급사)
movie_d$등급<-factor(movie_d$등급)
movie_d$주말<-factor(movie_d$주말)
movie_d[,11:29][sapply(movie_d[,11:29],is.numeric)]<-lapply(movie_d[,11:29][sapply(movie_d[,11:29],is.numeric)], as.factor)
movie_d[,31:34][sapply(movie_d[,31:34],is.numeric)]<-lapply(movie_d[,31:34][sapply(movie_d[,31:34],is.numeric)], as.factor)


#train:test=8:2
set.seed(10)
n=nrow(movie_d)
ind<-sample.int(n)
n_train<-floor(n*0.8)
n_test<-n-(n_train)

ind_tr<-ind[1:n_train]
ind_te<-ind[(n_train+1):n]

df.train<-movie_d[ind_tr,]
df.te<-movie_d[ind_te,]


#--------------------------svm----------------------------------------
library(e1071)

#5-folds
k=5
df.train$id<-sample(1:k, nrow(df.train), replace=T)
list<-1:k

prediction<-data.frame()
testsetcopy<-data.frame()

for (i in 1:k) {
  trainingset<-subset(df.train,id %in% list[-i])
  testset<-subset(df.train, id %in% c(i))
  fit1<-svm(trainingset$y~.,data = trainingset[,-36], type='C-classification')
  temp<-as.data.frame(predict(fit1,testset[,-36],decision.values=TRUE))
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,c(35,36)]))
}

result <- cbind(prediction, testsetcopy)
names(result) <- c("Predicted", "Actual","id")
head(result,10)
result$Actual<-factor(result$Actual)


#folds별 f1-score구하기 
f1<-data.frame()
precision<-data.frame()
for (i in 1:5) {
  r<-result %>% filter(id == i)
  f <- confusionMatrix(r$Predicted,r$Actual)$byClass[,7]
  p<-confusionMatrix(r$Predicted,r$Actual)$byClass[,5]
  f1<-rbind(f1,f)
  precision<-rbind(precision,p)
}
names(f1)<-c("class0","class1","class2","class3")
names(precision)<-c("class0","class1","class2","class3")

f1
precision

#folds의 평균 f1-score와 precision
apply(f1,2,mean) 
apply(precision,2,mean)



#-----------multinomial logestic regression----------------------------------------
library(nnet)


#5-fols CV
prediction<-data.frame()
testsetcopy<-data.frame()

for (i in 1:k) {
  trainingset<-subset(df.train,id %in% list[-i])
  testset<-subset(df.train, id %in% c(i))
  fit1<-nnet::multinom(trainingset$y~.,data = trainingset[,-36])
  temp<-as.data.frame(fit1 %>% predict(testset[,-36]))
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,c(35,36)]))
}

result <- cbind(prediction, testsetcopy)
names(result) <- c("Predicted", "Actual","id")
head(result,10)
result$Actual<-factor(result$Actual)


f1<-data.frame()
precision<-data.frame()
for (i in 1:5) {
  r<-result %>% filter(id == i)
  f <- confusionMatrix(r$Predicted,r$Actual)$byClass[,7]
  p<-confusionMatrix(r$Predicted,r$Actual)$byClass[,5]
  f1<-rbind(f1,f)
  precision<-rbind(precision,p)
}

names(f1)<-c("class0","class1","class2","class3")
names(precision)<-c("class0","class1","class2","class3")


f1
precision

apply(f1,2,sum,na.rm=T)/5
apply(precision,2,sum,na.rm=T)/5

#---------------gbm------------------------------------------
install.packages("gbm")
library(gbm)

prediction<-data.frame()
testsetcopy<-data.frame()

for (i in 1:k) {
  trainingset<-subset(df.train,id %in% list[-i])
  testset<-subset(df.train, id %in% c(i))
  fit<-gbm(y~.,data = trainingset[,-36],distribution = "multinomial",
            shrinkage = .01,n.minobsinnode = 10,n.trees = 200)
  pred<-predict.gbm(object = fit,newdata = testset[,-36], n.trees = 200,type = "response")
  temp = as.data.frame(as.integer(colnames(pred)[apply(pred, 1, which.max)]))
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,c(35,36)]))
}

result <- cbind(prediction, testsetcopy)
names(result) <- c("Predicted", "Actual","id")
head(result,10)
result$Predicted<-factor(result$Predicted)
result$Actual<-factor(result$Actual)

f1<-data.frame()
precision<-data.frame()
for (i in 1:5) {
  r<-result %>% filter(id == i)
  f <- confusionMatrix(r$Predicted,r$Actual)$byClass[,7]
  p<-confusionMatrix(r$Predicted,r$Actual)$byClass[,5]
  f1<-rbind(f1,f)
  precision<-rbind(precision,p)
}

names(f1)<-c("class0","class1","class2","class3")
names(precision)<-c("class0","class1","class2","class3")

f1
precision

apply(f1,2,sum,na.rm=T)/5
apply(precision,2,sum,na.rm=T)/5

#--------------------naive bayes---------------------------------------------
library(e1071)

prediction<-data.frame()
testsetcopy<-data.frame()

for (i in 1:k) {
  trainingset<-subset(df.train,id %in% list[-i])
  testset<-subset(df.train, id %in% c(i))
  fit<-naiveBayes(trainingset$y~.,data = trainingset[,-36])
  temp<-as.data.frame(predict(fit,testset[,-36],type='class'))
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,c(35,36)]))
}

result <- cbind(prediction, testsetcopy)
names(result) <- c("Predicted", "Actual","id")
head(result,10)
result$Predicted<-factor(result$Predicted)

f1<-data.frame()
precision<-data.frame()
for (i in 1:5) {
  r<-result %>% filter(id == i)
  f <- confusionMatrix(r$Predicted,r$Actual)$byClass[,7]
  p<-confusionMatrix(r$Predicted,r$Actual)$byClass[,5]
  f1<-rbind(f1,f)
  precision<-rbind(precision,p)
}

names(f1)<-c("class0","class1","class2","class3")
names(precision)<-c("class0","class1","class2","class3")

f1
precision

apply(f1,2,sum,na.rm=T)/5
apply(precision,2,sum,na.rm=T)/5

#--------------------randomforest----------------------------
install.packages('randomForest')
library(randomForest)
library(plyr)
library(dplyr)

prediction<-data.frame()
testsetcopy<-data.frame()

for (i in 1:k) {
  trainingset<-subset(df.train,id %in% list[-i])
  testset<-subset(df.train, id %in% c(i))
  fit<-randomForest(trainingset$y~.,data = trainingset[,-36],ntree = 100)
  temp<-as.data.frame(predict(fit,testset[,-36]))
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,c(35,36)]))
}

result <- cbind(prediction, testsetcopy)
names(result) <- c("Predicted", "Actual","id")
head(result,10)


f1<-data.frame()
precision<-data.frame()
for (i in 1:5) {
  r<-result %>% filter(id == i)
  f <- confusionMatrix(r$Predicted,r$Actual)$byClass[,7]
  p<-confusionMatrix(r$Predicted,r$Actual)$byClass[,5]
  f1<-rbind(f1,f)
  precision<-rbind(precision,p)
}

confusionMatrix(result$Predicted,result$Actual)
names(f1)<-c("class0","class1","class2","class3")
names(precision)<-c("class0","class1","class2","class3")


f1
precision

apply(f1,2,sum,na.rm=T)/5
apply(precision,2,sum,na.rm=T)/5




#------------LGBM-----------------------------------------------------------
install.packages("lightgbm", repos = "https://cran.r-project.org")
library(lightgbm)
movie_e<-movie_d
names(movie_e)<-1:35


#train:test=8:2
set.seed(10)
n=nrow(movie_e)
ind<-sample.int(n)
n_train<-floor(n*0.8)
n_test<-n-(n_train)

ind_tr<-ind[1:n_train]
ind_te<-ind[(n_train+1):n]

df.train<-movie_e[ind_tr,]
df.te<-movie_e[ind_te,]


k=5
df.train$id<-sample(1:k, nrow(df.train), replace=T)
list<-1:k

params <- list(objective = "multiclass", metric = 'multi_logloss', num_class = 4)

prediction<-data.frame()
testsetcopy<-data.frame()


for (i in 1:k) {
  trainingset<-as.matrix(subset(df.train,id %in% list[-i]))
  testset<-as.matrix(subset(df.train, id %in% c(i)))
  dtrain<-lgb.Dataset(data=trainingset[,c(-35,-36)],label=trainingset[,35])
  dtest <- lgb.Dataset.create.valid(dtrain, data = testset[, c(-35,-36)], label = testset[, 35])
  valids <- list(test = dtest)
  fit<-lgb.train(params
                 , dtrain
                 , 200
                 , valids
                 , min_data = 1
                 , learning_rate = 1
                 , early_stopping_rounds = 10L)
  temp<-predict(fit, testset[, c(-35,-36)], reshape = TRUE)
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,35]))
}

result6 <- cbind(prediction, testsetcopy[, 1])
result6

#각 row 값에서 지정한 n번째 열의 위치를 찾아주는 function
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

result6$p<-apply(result6[,1:4],1,maxn(1))

result6<-result6[,c(6,5)]
names(result6) <- c("Predicted", "Actual")

result6$Predicted<-factor(result6$Predicted-1) #class 예측값이 1:4로 되어 있어 -1해줌 
result6$Actual<-factor(result6$Actual)

#lgb는 oof를 따라서 각 folds의 평균이 아닌 전체 예측값에서 f1-score와 precision을 구함 
lgb<-confusionMatrix(result6$Predicted, result6$Actual)
lgb
lgb$byClass
sum(lgb$byClass[,7],na.rm=T)/4 #.6742
sum(lgb$byClass[,5],na.rm=T)/4 #7688




#----------------xgboost---------------------------------
library("xgboost") 
library("dplyr")
#xgboost는 모든 값이 숫자형으로 되어 있어야함. 그래서 새로 데이터셋을 만듬 
movie_x<-read.xlsx("C:\\Users\\tkrhk\\Desktop\\real_final3.xlsx", sheetIndex = 1, encoding = "UTF-8")

#0:100만 미만, 1:100만이상 300미만, 2: 300만 이상 500만 미만 3: 500이상
movie_x$y<-ifelse(movie$누적관객수<1000000,0, ifelse(movie$누적관객수<3000000,1,ifelse(movie$누적관객수<5000000,2,3)))

movie_x<-movie_x[,c(-1,-2,-3,-4)]

#train:test split
set.seed(10)
n=nrow(movie_x)
ind<-sample.int(n)
n_train<-floor(n*0.8)
n_test<-n-(n_train)

ind_tr<-ind[1:n_train]
ind_te<-ind[(n_train+1):n]

df.train<-movie_x[ind_tr,]
df.te<-movie_x[ind_te,]

# split train data and make xgb.DMatrix
df.train<-as.matrix(df.train)
df.te<-as.matrix(df.te)

train_matrix <- xgb.DMatrix(data = df.train[,-35], label = df.train[,35])
test_matrix <- xgb.DMatrix(data =df.te[,-35], label = df.te[,35])

xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = 4)

cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = 50,
                   nfold = 5,
                   verbose = FALSE,
                   prediction = TRUE)

OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = df.train[,35]+1)

head(OOF_prediction)

xgb<-confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")
xgb$byClass
sum(xgb$byClass[,7],na.rm=T)/4 #모든 classd의 평균 f1
sum(xgb$byClass[,5],na.rm=T)/4 #모든 classd의 평균 precision

xgb
xgb$byClass




############################3-class classification###########################
#0:100만 미만, 1:100만이상 500미만, 2: 500이상
movie_d$y<-ifelse(movie$누적관객수<1000000,0,ifelse(movie$누적관객수<5000000,1,2))

#train:test=8:2
set.seed(10)
n=nrow(movie_d)
ind<-sample.int(n)
n_train<-floor(n*0.8)
n_test<-n-(n_train)

ind_tr<-ind[1:n_train]
ind_te<-ind[(n_train+1):n]

df.train<-movie_d[ind_tr,]
df.te<-movie_d[ind_te,]
#--------------------------svm----------------------------------------
library(e1071)

#5-folds
k=5
df.train$id<-sample(1:k, nrow(df.train), replace=T)
list<-1:k

prediction<-data.frame()
testsetcopy<-data.frame()

for (i in 1:k) {
  trainingset<-subset(df.train,id %in% list[-i])
  testset<-subset(df.train, id %in% c(i))
  fit1<-svm(trainingset$y~.,data = trainingset[,-36], type='C-classification')
  temp<-as.data.frame(predict(fit1,testset[,-36],decision.values=TRUE))
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,c(35,36)]))
}

result <- cbind(prediction, testsetcopy)
names(result) <- c("Predicted", "Actual","id")
head(result,10)
result$Actual<-factor(result$Actual)


#folds별 f1-score구하기 
f1<-data.frame()
precision<-data.frame()
for (i in 1:5) {
  r<-result %>% filter(id == i)
  f <- confusionMatrix(r$Predicted,r$Actual)$byClass[,7]
  p<-confusionMatrix(r$Predicted,r$Actual)$byClass[,5]
  f1<-rbind(f1,f)
  precision<-rbind(precision,p)
}
names(f1)<-c("class0","class1","class2")
names(precision)<-c("class0","class1","class2")

f1
precision

#folds의 평균 f1-score와 precision
apply(f1,2,mean) 
apply(precision,2,mean)



#-----------multinomial logestic regression----------------------------------------
library(nnet)


#5-fols CV
prediction<-data.frame()
testsetcopy<-data.frame()

for (i in 1:k) {
  trainingset<-subset(df.train,id %in% list[-i])
  testset<-subset(df.train, id %in% c(i))
  fit1<-nnet::multinom(trainingset$y~.,data = trainingset[,-36])
  temp<-as.data.frame(fit1 %>% predict(testset[,-36]))
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,c(35,36)]))
}

result <- cbind(prediction, testsetcopy)
names(result) <- c("Predicted", "Actual","id")
head(result,10)
result$Actual<-factor(result$Actual)


f1<-data.frame()
precision<-data.frame()
for (i in 1:5) {
  r<-result %>% filter(id == i)
  f <- confusionMatrix(r$Predicted,r$Actual)$byClass[,7]
  p<-confusionMatrix(r$Predicted,r$Actual)$byClass[,5]
  f1<-rbind(f1,f)
  precision<-rbind(precision,p)
}

names(f1)<-c("class0","class1","class2")
names(precision)<-c("class0","class1","class2")

f1
precision

apply(f1,2,sum,na.rm=T)/5
apply(precision,2,sum,na.rm=T)/5

#---------------gbm------------------------------------------
install.packages("gbm")
library(gbm)

prediction<-data.frame()
testsetcopy<-data.frame()

for (i in 1:k) {
  trainingset<-subset(df.train,id %in% list[-i])
  testset<-subset(df.train, id %in% c(i))
  fit<-gbm(y~.,data = trainingset[,-36],distribution = "multinomial",
           shrinkage = .01,n.minobsinnode = 10,n.trees = 200)
  pred<-predict.gbm(object = fit,newdata = testset[,-36], n.trees = 200,type = "response")
  temp = as.data.frame(as.integer(colnames(pred)[apply(pred, 1, which.max)]))
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,c(35,36)]))
}

result <- cbind(prediction, testsetcopy)
names(result) <- c("Predicted", "Actual","id")
head(result,10)
result$Predicted<-factor(result$Predicted)
result$Actual<-factor(result$Actual)

f1<-data.frame()
precision<-data.frame()
for (i in 1:5) {
  r<-result %>% filter(id == i)
  f <- confusionMatrix(r$Predicted,r$Actual)$byClass[,7]
  p<-confusionMatrix(r$Predicted,r$Actual)$byClass[,5]
  f1<-rbind(f1,f)
  precision<-rbind(precision,p)
}

names(f1)<-c("class0","class1","class2")
names(precision)<-c("class0","class1","class2")

f1
precision

apply(f1,2,sum,na.rm=T)/5
apply(precision,2,sum,na.rm=T)/5

#--------------------naive bayes---------------------------------------------
library(e1071)

prediction<-data.frame()
testsetcopy<-data.frame()

for (i in 1:k) {
  trainingset<-subset(df.train,id %in% list[-i])
  testset<-subset(df.train, id %in% c(i))
  fit<-naiveBayes(trainingset$y~.,data = trainingset[,-36])
  temp<-as.data.frame(predict(fit,testset[,-36],type='class'))
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,c(35,36)]))
}

result <- cbind(prediction, testsetcopy)
names(result) <- c("Predicted", "Actual","id")
head(result,10)
result$Predicted<-factor(result$Predicted)

f1<-data.frame()
precision<-data.frame()
for (i in 1:5) {
  r<-result %>% filter(id == i)
  f <- confusionMatrix(r$Predicted,r$Actual)$byClass[,7]
  p<-confusionMatrix(r$Predicted,r$Actual)$byClass[,5]
  f1<-rbind(f1,f)
  precision<-rbind(precision,p)
}

names(f1)<-c("class0","class1","class2")
names(precision)<-c("class0","class1","class2")

f1
precision

apply(f1,2,sum,na.rm=T)/5
apply(precision,2,sum,na.rm=T)/5

#--------------------randomforest----------------------------
install.packages('randomForest')
library(randomForest)
library(plyr)
library(dplyr)

prediction<-data.frame()
testsetcopy<-data.frame()

for (i in 1:k) {
  trainingset<-subset(df.train,id %in% list[-i])
  testset<-subset(df.train, id %in% c(i))
  fit<-randomForest(trainingset$y~.,data = trainingset[,-36],ntree = 100)
  temp<-as.data.frame(predict(fit,testset[,-36]))
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,c(35,36)]))
}

result <- cbind(prediction, testsetcopy)
names(result) <- c("Predicted", "Actual","id")
head(result,10)


f1<-data.frame()
precision<-data.frame()
for (i in 1:5) {
  r<-result %>% filter(id == i)
  f <- confusionMatrix(r$Predicted,r$Actual)$byClass[,7]
  p<-confusionMatrix(r$Predicted,r$Actual)$byClass[,5]
  f1<-rbind(f1,f)
  precision<-rbind(precision,p)
}

confusionMatrix(result$Predicted,result$Actual)
names(f1)<-c("class0","class1","class2")
names(precision)<-c("class0","class1","class2")


f1
precision

apply(f1,2,sum,na.rm=T)/5
apply(precision,2,sum,na.rm=T)/5




#------------LGBM-----------------------------------------------------------
install.packages("lightgbm", repos = "https://cran.r-project.org")
library(lightgbm)
movie_e<-movie_d
names(movie_e)<-1:35


#train:test=8:2
set.seed(10)
n=nrow(movie_e)
ind<-sample.int(n)
n_train<-floor(n*0.8)
n_test<-n-(n_train)

ind_tr<-ind[1:n_train]
ind_te<-ind[(n_train+1):n]

df.train<-movie_e[ind_tr,]
df.te<-movie_e[ind_te,]


k=5
df.train$id<-sample(1:k, nrow(df.train), replace=T)
list<-1:k

params <- list(objective = "multiclass", metric = 'multi_logloss', num_class = 3)

prediction<-data.frame()
testsetcopy<-data.frame()


for (i in 1:k) {
  trainingset<-as.matrix(subset(df.train,id %in% list[-i]))
  testset<-as.matrix(subset(df.train, id %in% c(i)))
  dtrain<-lgb.Dataset(data=trainingset[,c(-35,-36)],label=trainingset[,35])
  dtest <- lgb.Dataset.create.valid(dtrain, data = testset[, c(-35,-36)], label = testset[, 35])
  valids <- list(test = dtest)
  fit<-lgb.train(params
                 , dtrain
                 , 200
                 , valids
                 , min_data = 1
                 , learning_rate = 1
                 , early_stopping_rounds = 10L)
  temp<-predict(fit, testset[, c(-35,-36)], reshape = TRUE)
  prediction<-rbind(prediction,temp)
  testsetcopy<-rbind(testsetcopy,as.data.frame(testset[,35]))
}

result6 <- cbind(prediction, testsetcopy[, 1])
result6

#각 row 값에서 지정한 n번째 열의 위치를 찾아주는 function
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

result6$p<-apply(result6[,1:3],1,maxn(1))

result6<-result6[,c(5,4)]
names(result6) <- c("Predicted", "Actual")

result6$Predicted<-factor(result6$Predicted-1) #class 예측값이 1:4로 되어 있어 -1해줌 
result6$Actual<-factor(result6$Actual)

#lgb는 oof를 따라서 각 folds의 평균이 아닌 전체 예측값에서 f1-score와 precision을 구함 
lgb<-confusionMatrix(result6$Predicted, result6$Actual)
lgb
lgb$byClass
sum(lgb$byClass[,7],na.rm=T)/4 #f1
sum(lgb$byClass[,5],na.rm=T)/4 #precision




#----------------xgboost---------------------------------
library("xgboost") 
library("dplyr")
#xgboost는 모든 값이 숫자형으로 되어 있어야함. 그래서 새로 데이터셋을 만듬 
movie_x<-read.xlsx("C:\\Users\\tkrhk\\Desktop\\real_final3.xlsx", sheetIndex = 1, encoding = "UTF-8")

#0:100만 미만, 1:100만이상 300미만, 2: 300만 이상 500만 미만 3: 500이상
movie_x$y<-ifelse(movie$누적관객수<1000000,0, ifelse(movie$누적관객수<3000000,1,ifelse(movie$누적관객수<5000000,2,3)))

movie_x<-movie_x[,c(-1,-2,-3,-4)]

#train:test split
set.seed(10)
n=nrow(movie_x)
ind<-sample.int(n)
n_train<-floor(n*0.8)
n_test<-n-(n_train)

ind_tr<-ind[1:n_train]
ind_te<-ind[(n_train+1):n]

df.train<-movie_x[ind_tr,]
df.te<-movie_x[ind_te,]

# split train data and make xgb.DMatrix
df.train<-as.matrix(df.train)
df.te<-as.matrix(df.te)

train_matrix <- xgb.DMatrix(data = df.train[,-35], label = df.train[,35])
test_matrix <- xgb.DMatrix(data =df.te[,-35], label = df.te[,35])

xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = 3)

cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = 50,
                   nfold = 5,
                   verbose = FALSE,
                   prediction = TRUE)

OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = df.train[,35]+1)

head(OOF_prediction)

xgb<-confusionMatrix(factor(OOF_prediction$max_prob),
                     factor(OOF_prediction$label),
                     mode = "everything")
xgb$byClass
sum(xgb$byClass[,7],na.rm=T)/4 #모든 classd의 평균 f1
sum(xgb$byClass[,5],na.rm=T)/4 #모든 classd의 평균 precision

xgb
xgb$byClass

#최종 모델의 변수  importance
importance(cv_model)
