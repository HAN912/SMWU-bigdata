##AutoML

library(readxl)
setwd("C:\\Users\\yangj")
a<-read_excel("real_final5.xlsx")
real_final5<-as.data.frame(a)
str(real_final5)
real_final5<-real_final5[,-c(1,2)]
head(real_final5)
df<-real_final5
names(df)<-1:35

set.seed(10)
n=nrow(df)
ind<-sample.int(n)
n.tr<-floor(n*0.6)
n.val<-floor(n*0.2)
n.te<-n-(n.tr+n.val)

ind.tr<-ind[1:n.tr]
ind.val<-ind[(n.tr+1):(n.tr+n.val)]
ind.te<-ind[(n.tr+n.val+1):n]

df.tr<-df[ind.tr,]
df.tr$'1'<-log(df.tr$'1')
df.val<-df[ind.val,]

library(h2o)
h2o.init()


train.h<-as.h2o(df.tr)
val.h<-as.h2o(df.val)

y<-'1'
pred<-setdiff(names(train.h),y)

aml<-h2o.automl(x=pred,y=y,
                training_frame = train.h,
                max_models = 20,
                seed=10,
                max_runtime_secs = 20)
lb<-aml@leaderboard
as.data.frame(lb)
h2o.shutdown(prompt=F)



###########################################################################################################################
###########################################################################################################################
##lm, lm with lasso, knn reg

library(readxl)
library(class)
library(glmnet)
library(FNN)

setwd("C:\\Users\\yangj")
a<-read_excel("real_final5.xlsx")
real_final5<-as.data.frame(a)
str(real_final5)
real_final5<-real_final5[,-c(1,2)]
head(real_final5)
df<-real_final5

##5-fold 교차검증
##전체 데이터셋에 대해 tr과 te로 나눈 후
##for문에서 tr을 5개로 나눠서 하나는 cv.val으로 나머지 4개는 cv.tr로 둔다

K=5

set.seed(10)

n=nrow(df)
n.tr<-floor(n*0.8)
n.te<-n-n.tr

stack.lm=matrix(0,nrow=K,ncol=3)

ind<-sample.int(n)
ind.tr=ind[1:n.tr]
ind.te=ind[(n.tr+1):n]

df.tr<-df[ind.tr,]
df.te<-df[ind.te,]


########################################################################
# 1.lm

for (k in 1:K){
  
  df.cv.val=df.tr[(floor(n.tr/K)*(k-1)+1):(floor(n.tr/K)*k),]
  df.cv.tr=df.tr[-((floor(n.tr/K)*(k-1)+1):(floor(n.tr/K)*k)),]
  
  y.tr=log(df.cv.tr$누적관객수)
  x.tr=df.cv.tr[,-1]
  
  y.val=log(df.cv.val$누적관객수)
  x.val=df.cv.val[,-1]
  
  obj.lm=lm(log(누적관객수)~.,data=df.cv.tr)
  coef_lm<-coef(obj.lm)
  clist_lm<-rep(NA,length(coef_lm))
  for ( i in 1:length(coef_lm)){
    if (is.na(coef_lm[i])==T){
      value=0
      clist_lm[i]<-value
    }
    
    else if (is.na(coef_lm[i])==F){
      value<-coef_lm[i]
      clist_lm[i]<-value
    }
  }
  y.val.hat.lm=as.matrix(cbind(1,x.val))%*%as.vector(clist_lm)
  rmse.val.lm=sqrt(mean((y.val-y.val.hat.lm)^2))
  mae.val.lm = mean(abs(y.val - y.val.hat.lm)) 
  mape.val.lm =  mean(abs((y.val - y.val.hat.lm)/y.val)*100 )
  
  
  values=c(rmse.val.lm, mae.val.lm, mape.val.lm) 
  stack.lm[k,]=values
  
} 

stack.lm.cv<-apply(stack.lm,2,mean)
stack.lm.cv




#2. lasso

grid = 2^seq(from=50, to=-49, length=100)

rmses.lasso<-matrix(0,nrow=100,ncol=5) #람다별로 rmse모은 스택
maes.lasso<-matrix(0,nrow=100,ncol=5) #람다별로 mae모은 스택
mapes.lasso<-matrix(0,nrow=100,ncol=5) #람다별로 mape모은 스택

for (k in 1:K){
  df.cv.val=df.tr[(floor(n.tr/K)*(k-1)+1):(floor(n.tr/K)*k),]
  df.cv.tr=df.tr[-((floor(n.tr/K)*(k-1)+1):(floor(n.tr/K)*k)),]
  
  y.tr=log(df.cv.tr$누적관객수)
  x.tr=df.cv.tr[,-1]
  
  y.val=log(df.cv.val$누적관객수)
  x.val=df.cv.val[,-1]
  
  obj.lasso<-glmnet( x = as.matrix(x.tr), y=y.tr, family = "gaussian" ,
                     alpha=1, lambda = grid )
  val.rmses<-NA
  val.maes<-NA
  val.mapes<-NA
  
  coef_lasso<-coef(obj.lasso)
  
  for (g in 1:length(grid)) {
    yhat.lasso = as.matrix(cbind(1, x.val)) %*% coef_lasso[ ,g] 
    rmse = sqrt( mean((y.val - yhat.lasso)^2) )
    mae = mean(abs(y.val - yhat.lasso)) 
    mape =  mean(abs((y.val - yhat.lasso)/y.val)*100 )
    
    val.rmses[g] = rmse
    val.maes[g] = mae
    val.mapes[g] = mape
    
  }
  
  
  
  rmses.lasso[,k]=val.rmses
  maes.lasso[,k]=val.maes
  mapes.lasso[,k]=val.mapes
  
}

rmses.lasso.cv=apply(rmses.lasso,1,mean) #람다별로 5-fold cv rmse 평균 구하기 
rmses.lasso.cv

maes.lasso.cv=apply(maes.lasso,1,mean) #람다별로 5-fold cv mae 평균 구하기 
maes.lasso.cv

mapes.lasso.cv=apply(mapes.lasso,1,mean) #람다별로 5-fold cv mape 평균 구하기 
mapes.lasso.cv

lasso.measure.cv1<-c(min(rmses.lasso.cv),maes.lasso.cv[which.min(rmses.lasso.cv)],mapes.lasso.cv[which.min(rmses.lasso.cv)])
lasso.measure.cv2<-c(rmses.lasso.cv[which.min(maes.lasso.cv)],min(maes.lasso.cv),mapes.lasso.cv[which.min(maes.lasso.cv)])
lasso.measure.cv3<-c(rmses.lasso.cv[which.min(mapes.lasso.cv)],maes.lasso.cv[which.min(mapes.lasso.cv)],min(mapes.lasso.cv))

lasso.measure.cv1 #cv rmse 최소값을 가지는 람다에 대한 cv rmse, cv mae, cv mape
grid[which.min(rmses.lasso.cv)]

lasso.measure.cv2 #cv mae 최소값을 가지는 람다에 대한 cv rmse, cv mae, cv mape 
grid[which.min(maes.lasso.cv)]

lasso.measure.cv3 #cv mape 최소값을 가지는 람다에 대한 cv rmse, cv mae, cv mape
grid[which.min(mapes.lasso.cv)]



#3. knn


group = 50
rmses.knn<-matrix(0,nrow=50,ncol=5) #k별로 rmse모은 스택
maes.knn<-matrix(0,nrow=50,ncol=5) #k별로 mae모은 스택
mapes.knn<-matrix(0,nrow=50,ncol=5) #k별로 mape모은 스택

for (k in 1:K){
  
  df.cv.val=df.tr[(floor(n.tr/K)*(k-1)+1):(floor(n.tr/K)*k),]
  df.cv.tr=df.tr[-((floor(n.tr/K)*(k-1)+1):(floor(n.tr/K)*k)),]
  
  y.tr=log(df.cv.tr$누적관객수)
  x.tr=df.cv.tr[,-1]
  
  y.val=log(df.cv.val$누적관객수)
  x.val=df.cv.val[,-1]
  
  
  val.rmses<-NA
  val.maes<-NA
  val.mapes<-NA
  
  for (g in 1:group) {
    yhat.knn = knn.reg(train=as.matrix(x.tr), test=as.matrix(x.val), y=y.tr, k=g)$pred
    val.rmses[g] = sqrt(mean( (y.val - yhat.knn)^2 ))
    val.maes[g] = mean( abs(y.val - yhat.knn) )
    val.mapes[g] = mean( abs((y.val - yhat.knn)/y.val)*100 )
    
  }
  
  rmses.knn[,k]=val.rmses
  maes.knn[,k]=val.maes
  mapes.knn[,k]=val.mapes
  
  #print((1:group)[which.min(rmses.knn)]) #k의 값을 어떤 것으로 설정했을 때 가장 작은 rmse를 가졌는지 알아보는 코드
  
  
} 


rmses.knn.cv=apply(rmses.knn,1,mean) #k별로 5-fold cv rmse 평균 구하기 
rmses.knn.cv

maes.knn.cv=apply(maes.knn,1,mean) #k별로 5-fold cv mae 평균 구하기 
maes.knn.cv

mapes.knn.cv=apply(mapes.knn,1,mean) #k별로 5-fold cv mape 평균 구하기 
mapes.knn.cv

knn.measure.cv1<-c(min(rmses.knn.cv),maes.knn.cv[which.min(rmses.knn.cv)],mapes.knn.cv[which.min(rmses.knn.cv)])
knn.measure.cv2<-c(rmses.knn.cv[which.min(maes.knn.cv)],min(maes.knn.cv),mapes.knn.cv[which.min(maes.knn.cv)])
knn.measure.cv3<-c(rmses.knn.cv[which.min(mapes.knn.cv)],maes.knn.cv[which.min(mapes.knn.cv)],min(mapes.knn.cv))

knn.measure.cv1 #cv rmse 최소값을 가지는 k에 대한 cv rmse, cv mae, cv mape
print((1:group)[which.min(rmses.knn.cv)])

knn.measure.cv2 #cv mae 최소값을 가지는 k에 대한 cv rmse, cv mae, cv mape 
print((1:group)[which.min(maes.knn.cv)])

knn.measure.cv3 #cv mape 최소값을 가지는 k에 대한 cv rmse, cv mae, cv mape
print((1:group)[which.min(mapes.knn.cv)])




#######################################################
#표로 정리

table<-data.frame(stack.lm.cv,
                  lasso.measure.cv1,lasso.measure.cv2,lasso.measure.cv3,
                  knn.measure.cv1,knn.measure.cv2,knn.measure.cv3)

colnames(table)<-c("lm",
                   'lasso1 (lambda=0.03125)','lasso2 (lambda=0.03125)','lasso3 (lambda=0.015625)',
                   'knn1 (k=7)','knn2 (k=7)','knn3 (k=7)')
rownames(table)<-c('RMSE','MAE','MAPE')
t(table)

###########################################################################################################################
###########################################################################################################################
#스크린 상한제 관련 회귀분석

library(readxl)

setwd("C:\\Users\\yangj")
a<-read_excel("real_final5.xlsx")
real_final5<-as.data.frame(a)
str(real_final5)
real_final5<-real_final5[,-c(1,2)]
head(real_final5)
df<-real_final5

season1<-ifelse(df$겨울==1,1,0)
season2<-ifelse(df$봄==1,2,0)
season3<-ifelse(df$여름==1,3,0)
season4<-ifelse(df$가을==1,4,0)
season<-season1+season2+season3+season4
season<-as.factor(season)
df.modify<-cbind(df,season)
df.modify<-df.modify[,-c(32,33,34,35)]
str(df.modify)


#우도비 검정 
lm1<-lm(log(누적관객수)~.,data=df.modify)
summary(lm1)
d1<-deviance(lm1)
df1<-lm1$df.residual

lm2<-lm(log(누적관객수)~.-스크린수,data=df.modify)
summary(lm2)
d2<-deviance(lm2)
df2<-lm2$df.residual

1-pchisq(d2-d1,df2-df1)
