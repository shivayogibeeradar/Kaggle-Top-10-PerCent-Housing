library(tidyverse)
k=read.csv("/Users/shivayogibiradar/Desktop/Projects/train.csv")
l=read.csv("/Users/shivayogibiradar/Desktop/Projects/test.csv")
nrow(df)
write.csv(df,file="data.csv")
t=sapply(k, function(x) sum(is.na(x)))
t=t[t>0]
t=sort(t)
barplot(t,col="Blue",cex.names = 0.75,las=2,horiz = TRUE)

t=sapply(l, function(x) sum(is.na(x)))
t=t[t>0]
t=sort(t)
barplot(t,col="Blue",cex.names = 0.75,las=2,horiz = TRUE)

hist(k$SalePrice,xlab="SalePrice")
hist(log(k$SalePrice),xlab='LogSalePrice')
#boxplot(log(k$SalePrice))
#hist(log(k$SalePrice+1))


#install.packages("regr")

k$MasVnrArea[is.na(k$MasVnrArea)]=0
l$MasVnrArea[is.na(l$MasVnrArea)]=0


#sqf=(k$X1stFlrS+k$X2ndFlrSF)



boxplot(k$SalePrice)


c


k$PoolQC=as.character(k$PoolQC)
k$PoolQC=none(k$PoolQC)

names=colnames(k)[colSums(is.na(k)) > 600]


#####for test.csv

names=colnames(l)[colSums(is.na(l)) > 600]
none=function(x){
  x=as.character(x)
  x=ifelse(is.na(x), "None", x)
  return(x)
  
}



str(k$MasVnrArea)
?sapply()
k[which(is.na(k$MasVnrArea)),]
##########coding missing values
k[names]=sapply(k[names],function(x) none(x))
l[names]=sapply(l[names],function(x) none(x))

names1=colnames(k)[colSums(is.na(k)) > 2 &colSums(is.na(k)) <200]
#k[names1]=k[names1][,-2]
###test
names2=colnames(l)[colSums(is.na(l)) > 2 &colSums(is.na(l)) <200]

k[names1]=sapply(k[names1],function(x) none(x))
#test
#l[names2]=l[names2][,-3]
l[names2]=sapply(l[names2],function(x) none(x))


names=colnames(l)[colSums(is.na(l)) > 0 & colSums(is.na(l))<2]


#########cariable functional set to typ


l$Functional[is.na(l$Functional)]="Typ"


##################bsmt
l[which(is.na(l$BsmtHalfBath)),]



none1=function(x){
x[is.na(x)] <- 0
  return(x)
  
}

l[which(is.na(l$BsmtHalfBath)),]=none1(l[which(is.na(l$BsmtHalfBath)),])

l[which(is.na(l$GarageArea)),]=none1(l[which(is.na(l$GarageArea)),])


l[which(is.na(l$Utilities)),]


l %>% group_by(Utilities) %>% summarise(count=n())
none3=function(x){
  x=as.character(x)
  x=ifelse(is.na(x), "AllPub", x)
  return(x)
  
}



l$Utilities[is.na(l$Utilities)]="AllPub"

l$SaleType[is.na(l$SaleType)]="WD"




l %>% group_by(l$KitchenQual) %>% summarise(n=n())
l$KitchenQual[is.na(l$KitchenQual)]="TA"

#(l[which(is.na(l$Exterior1st)),])

l %>% group_by(l$Exterior1st) %>% summarise(n=n())
l %>% group_by(l$Exterior2nd) %>% summarise(n=n())

l$Exterior1st[is.na(l$Exterior1st)]="VinylSd"
l$Exterior2nd[is.na(l$Exterior2nd)]="VinylSd"

#####Lotfrontage


z=k %>% dplyr::select(LotFrontage,Neighborhood) %>%
  group_by(Neighborhood)%>% summarize(median=median(LotFrontage,na.rm=TRUE))

t=k[which(is.na(k$LotFrontage)),] %>% dplyr::select(LotFrontage,Neighborhood)
q=left_join(t,z,by="Neighborhood")

k$LotFrontage[which(is.na(k$LotFrontage))]=q$median

#########Lotfrontage test
z=l %>% dplyr::select(LotFrontage,Neighborhood) %>%
  group_by(Neighborhood)%>% summarize(median=median(LotFrontage,na.rm=TRUE))

t=l[which(is.na(l$LotFrontage)),] %>% dplyr::select(LotFrontage,Neighborhood)
q=left_join(t,z,by="Neighborhood")

l$LotFrontage[which(is.na(l$LotFrontage))]=q$median













k$Electrical[1380]="SBrkr"

k=k %>% mutate(totalsqft=TotalBsmtSF+X1stFlrSF+X2ndFlrSF)


##test
l=l %>% mutate(totalsqft=TotalBsmtSF+X1stFlrSF+X2ndFlrSF)

which(is.na(l$totalsqft))

t=which(is.na(l[661,]))

l[661,t]=0
l=l %>% mutate(totalsqft=TotalBsmtSF+X1stFlrSF+X2ndFlrSF)

l$GarageArea
which(is.na(l$GarageArea))
l[1117,]


l$GarageType

k%>%filter(totalsqft>7500) 



dim(k)

##############modelling
unique(k$Neighborhood)

k %>% group_by(Neighborhood) %>% summarize(median=median(SalePrice)) %>% ggplot()+geom_col(aes(x=reorder(Neighborhood,-median),y=median,fill=Neighborhood))+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))


k %>% group_by(SaleCondition) %>% summarize(median=median(SalePrice)) %>% ggplot()+geom_col(aes(x=reorder(SaleCondition,-median),y=median,fill=SaleCondition))+theme(axis.text.x=element_text(angle=25,hjust=1,vjust=0.5))+xlab("SaleCondition")


k %>% group_by(KitchenQual) %>% summarize(median=median(SalePrice)) %>% ggplot()+geom_col(aes(x=reorder(KitchenQual,-median),y=median,fill=KitchenQual))+theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+xlab("KitchenQuality")
k$MSSubClass=as.factor(k$MSSubClass)
k %>% group_by(MSSubClass) %>% summarize(median=median(SalePrice)) %>% ggplot()+geom_col(aes(x=reorder(MSSubClass,-median),y=median,fill=MSSubClass))+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))

k %>% mutate(totalsqft=TotalBsmtSF+X1stFlrSF+X2ndFlrSF) %>% ggplot()+geom_point(aes(totalsqft,SalePrice,color=SaleCondition))+geom_smooth(aes(totalsqft,SalePrice))


k %>% mutate(totalsqft=TotalBsmtSF+X1stFlrSF+X2ndFlrSF) %>% filter(totalsqft>6000 & SalePrice<500000)
k=k[-which(k$totalsqft>7500),]
k$PoolQC=as.factor(k$PoolQC)
k$MSSubClass=as.factor(k$MSSubClass)

#cols=c("MSZoning",'Street',"Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","")
#######remember
#k = (k %>% mutate_if(is.factor,funs(factor(.))))
#k[(which(is.factor(k))),]
k$MasVnrArea=as.numeric(k$MasVnrArea)

col=("MoSold")
k[col]=lapply(k[col], factor)

z=Filter(is.factor, k)
ncol(z)
ncol(k)
z1=Filter(is.numeric, k)
ncol(z1)

?as.Date

(k$YearBuilt)
#######fitting regreeesion line
#install.packages("Metrics")
library(Metrics)
fit1=lm(data=k[,-1],SalePrice~.)
summary(fit1)
##########checking residuals
residuals(fit1)

plot(residuals(fit1),predict(fit1,k))

hist(log(k$SalePrice))
qqnorm(fit1$residuals, main = "Normal qqplot of residuals")
qqline(fit1$residuals)
k$lsaleprice=log(k$SalePrice)
fit1=lm(data=k[,-1],lsaleprice~.)
rmse(fit1$fitted.values,k$lsaleprice)
#################Transforming sale price to to log
k$lsaleprice=log(k$SalePrice)

library(caret)
k$GarageYrBlt=as.character(k$GarageYrBlt)
l$GarageYrBlt=as.character(l$GarageYrBlt)
k$GarageYrBlt[k$GarageYrBlt=="None"]=0
l$GarageYrBlt[l$GarageYrBlt=="None"]=0
k$GarageYrBlt=as.numeric(k$GarageYrBlt)
l$GarageYrBlt=as.numeric(l$GarageYrBlt)
l$MSSubClass=as.factor(l$MSSubClass)
k$MoSold=as.numeric(k$MoSold)
k$MasVnrArea=as.numeric(k$MasVnrArea)
l$MasVnrArea=as.numeric(l$MasVnrArea)
k1=k[,-which( colnames(k)=="Id" )]
k1=k[,-which( colnames(k)=="SalePrice" )]

dim(k)
dim(k1)
k = (k %>% mutate_if(is.character,funs(factor(.))))
l = (l%>% mutate_if(is.character,funs(factor(.))))
k1 = (k1 %>% mutate_if(is.character,funs(factor(.))))
split(names(k),sapply(k, function(x) paste(class(x), collapse=" ")))
split(names(l),sapply(l, function(x) paste(class(x), collapse=" ")))
split(names(k1),sapply(k1, function(x) paste(class(x), collapse=" ")))



k=k[,-which( colnames(k)=="Utilities" )]
l=l[,-which( colnames(l)=="Utilities" )]

k1=k[,-which( colnames(k)=="Id" )]
k1=k[,-which( colnames(k)=="SalePrice" )]



dim(k)
dim(l)
dim(k1)

dim(model.matrix(~.,k))
dim(model.matrix(~.,l))


k1=k1[,-1]
df=k1[,-which( colnames(k1) %in% c("SalePrice","lsaleprice"))]
dim(df)
dim(l)
l1=l
l=l[,-1]

nrow(l)
df=rbind(df,l)
nrow(k1)
nrow(df)
nrow(df)
df=model.matrix(~.,df)[,-1]
testf=df[1459:2917,]
trainf=df[1:1458,]
trainf=as.data.frame(trainf)
testf=as.data.frame(testf)
trainf$lsaleprice=k1$lsaleprice
split(names(k1),sapply(k1, function(x) paste(class(x), collapse=" ")))
split(names(df),sapply(df, function(x) paste(class(x), collapse=" ")))

library(caret)
set.seed(456)

intrain<- createDataPartition(y=trainf$lsaleprice,p=0.75, list=FALSE)

library(Metrics)


train=trainf[intrain,]
test=trainf[-intrain,]

fit2=lm(data=train,lsaleprice~.)
fit3=lm(data=train,lsaleprice~.)

summary(fit2)

qqnorm(fit2$residuals, main = "Normal qqplot of residuals")
qqline(fit2$residuals)

plot(residuals(fit2),predict(fit2,train))

head(sort(fit2$residuals,decreasing = TRUE),n=10)
rmse(predict(fit2,test),test$lsaleprice)

#######Residuals column
q=c(826,971,89,689,1146,739,329,14,682,775)
k[q,]
rmse(fit2$fitted.values,test$lsaleprice)
##################Split dtaset
#seed=789
#ratio=sample(1:nrow(k1),size=0.7*nrow(k1))
#train=k1[ratio,]
#test=k1[-ratio,]
#dim(train)
###########subset selection taking toolong
#######################PCREGRESSION
#install.packages("pls")
library(pls)
set.seed(123)
pca_fit=pcr(lsaleprice~.,data=train1)
scale(train)
train1=scale(train)
train1=data.frame(model.matrix(~.,train))
train1=as.data.frame(train1)
dim(train1)
summary(pca_fit)
validationplot(pca_fit)
validationplot(pca_fit, val.type = "R2")
predplot(pca_fit)
qqnorm(pca_fit$residuals, main = "Normal qqplot of residuals")
qqline(pca_fit$residuals)
rmse(predict(pca_fit,test1),test$lsaleprice)
###########################################################
library(glmnet)
x <- model.matrix(lsaleprice~.,trainf)[,-1]


y <- trainf$lsaleprice
x_prime <- as.data.frame(x)
y_prime <- y
Training_prime <- cbind(y_prime, x_prime)
x <- model.matrix(y_prime~.,Training_prime)[,-1]
y <- Training_prime$y_prime

grid = 10^seq(15,-2, length = 100)

ridge.mode = glmnet(x, y, alpha=0, lambda = grid)
plot(ridge.mode, main = "Ridge regression",label = TRUE, xvar = "lambda", xlim = c(-5,20))

cv.out <- cv.glmnet(x,y, alpha = 0)
plot(cv.out)
bestlam.ridge = cv.out$lambda.min
bestlam.ridge
log(bestlam.ridge)
ridge.mode <- glmnet(x, y, alpha=0, lambda = bestlam.ridge)
predict(ridge.mode, s = bestlam.ridge, type = "coefficients")


lasso.mod <- glmnet(x,y, alpha = 1, lambda = grid)
plot(lasso.mod, main = "Lasso regression", label = TRUE, xvar = "lambda", xlim = c(-5,15))

cv.out <- cv.glmnet(x,y,alpha = 1)
plot(cv.out)

bestlam.lasso <- cv.out$lambda.min
bestlam.lasso
test1 <- model.matrix(lsaleprice ~.,test)[,-1]

lasso.mode <- glmnet(x, y, alpha=1, lambda = bestlam.lasso)
predict(lasso.mode, s = bestlam.lasso, type = "coefficients")
rmse(predict(lasso.mode,newx=test1),test$lsaleprice)
#############Applying the model on entire training set
lasso.mode <- glmnet(x, y, alpha=1, lambda = bestlam.lasso)
predict(lasso.mode, s = bestlam.lasso, type = "coefficients")


elastic.mode=glmnet(x, y, alpha=0.5, lambda = grid)

elastic.mod <- glmnet(x,y, alpha = 0.5, lambda = grid)
plot(elastic.mod, main = "elastic net regression", label = TRUE, xvar = "lambda", xlim = c(-5,20))

cv.out <- cv.glmnet(x,y,alpha = 0.5)
plot(cv.out)
bestlam.elastic <- cv.out$lambda.min
bestlam.elastic 


elastic.mode <- glmnet(x, y, alpha=1, lambda = bestlam.elastic)
predict(elastic.mode, s = bestlam.elastic, type = "coefficients")

rmse(predict(elastic.mode,newx=test1),test$lsaleprice)








setwd("/Users/shivayogibiradar/Desktop/kaggle submissions")
write.csv(predict(ridge.mode,newx=testf),file="ridge1.csv")
write.csv(predict(lasso.mode,newx=testf),file="lasso1.csv")

rmse(predict(ridge.mode,newx=test1),test$lsaleprice)
residuals=(predict(ridge.mode,newx=x)-train$lsaleprice)
qqnorm(residuals, main = "Normal qqplot of residuals")
qqline(residuals)
head(sort(residuals[,1],decreasing = TRUE),n=10)
d=c(496,633,1325,917,969,463,729,875,534,589)
#write.csv(k,file="/Users/shivayogibiradar/Desktop/train.csv")
#write.csv(l,file="/Users/shivayogibiradar/Desktop/test.csv")
#install.packages("h2o")

library(caret)
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
gbmFit1 <- train(lsaleprice ~ ., data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
gbm_dev <- predict(gbmFit1, test)
rmse(predict(gbmFit1,newx=test),test$lsaleprice)

#install.packages("h2o")
library(h2o)
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "2G") 
train1=as.h2o(trainf)


splits <- h2o.splitFrame(
  train1,           ##  splitting the H2O frame we read above
  c(0.6,0.2),   ##  create splits of 60% and 20%; 
  ##  H2O will create one more split of 1-(sum of these parameters)
  ##  so we will get 0.6 / 0.2 / 1 - (0.6+0.2) = 0.6/0.2/0.2
  seed=1234)    ##  setting a seed will ensure reproducible results (not R's seed)

train <- h2o.assign(splits[[1]], "train.hex")   
## assign the first result the R variable train
## and the H2O name train.hex
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

## take a look at the first few rows of the data set
   ## rows 1-5, all columns

## run our first predictive model
1#caretensembles

gbm <- h2o.gbm(x = 1:ncol(train)-1, y = ncol(train), training_frame = train)

h2o.performance(gbm, newdata = valid)
predictors=1:ncol(train)-1
response=ncol(train)
gbm <- h2o.gbm(
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.01,                                                         
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "RMSE", 
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8,                                                   
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                 
)

h2o.performance(gbm, valid = TRUE)


hyper_params = list( max_depth = seq(1,29,2) )
#hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "RMSE", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid                                                                       

## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("depth_grid", sort_by="RMSE")    
sortedGrid

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))

hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                                             
  
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                                         
  
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train))-1,1),                                 
  
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)
search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,         
  
  ## build no more than 100 models
  max_models = 100,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "RMSE",
  stopping_tolerance = 1e-3
)

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  
  ## which algorithm to run
  algorithm = "gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid1", 
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,                                                 
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "RMSE", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234                                                             
)

sortedGrid <- h2o.getGrid("final_grid1", sort_by = "RMSE")    
sortedGrid
for (i in 1:5) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  print((h2o.performance(gbm, valid = TRUE)))
}


gbm <- h2o.getModel(sortedGrid@model_ids[[1]])

(h2o.performance(gbm, newdata = test))


model <- do.call(h2o.gbm,
                 ## update parameters in place
                 {
                   p <- gbm@parameters
                   p$model_id = NULL          ## do not overwrite the original grid model
                   p$training_frame = train1      ## use the full dataset
                   p$validation_frame = NULL  ## no validation frame
                             ## cross-validation
                   p
                 }
)

h2o.performance(model, newdata = test)

test1=as.h2o(testf)

predictions= exp(h2o.predict(model, newdata=test1))

setwd("/Users/shivayogibiradar/Desktop/kaggle submissions")
write.csv(as.data.frame(predictions),file="ensemble4.csv")


rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=1:ncol(train)-1,                        ## the predictor columns, by column index
  y=ncol(train),                         ## the target index (what we are predicting)
  model_id = "rf_covType_v1",    ## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let 
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of 
  ##  the prior two 2-tree averages.
  ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 1000000)                ## Set the random seed so that this can be
##  repr

test1=as.h2o(testf)
h2o.performance(g, newdata = test)
predictions= exp(h2o.predict(rf1, newdata=testf))



library(devtools)
#install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")
library(h2oEnsemble)
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.gbm.wrapper"
fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train, 
                    
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))

perf <- h2o.ensemble_performance(fit, newdata = valid)
?h2o.ensemble_performance()
print(perf,metric="MSE")
0.0148
preds= (predict(fit, newdata=test1))
predictions <- as.data.frame(preds$pred)
predictions=exp(predictions)


h2o.glm.1 <- function(..., alpha = 0.0) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.2 <- function(..., alpha = 0.5) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.3 <- function(..., alpha = 1.0) h2o.glm.wrapper(..., alpha = alpha)
h2o.randomForest.1 <- function(..., ntrees = 800, nbins = 50, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.randomForest.2 <- function(..., ntrees = 700, sample_rate = 0.75, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.3 <- function(..., ntrees = 600, sample_rate = 0.85, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.4 <- function(..., ntrees = 500, nbins = 50, balance_classes = TRUE, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, balance_classes = balance_classes, seed = seed)
h2o.gbm.1 <- function(..., ntrees = 840,mindepth=9, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, seed = seed,minDepth=mindepth)
h2o.gbm.2 <- function(..., ntrees = 1000, nbins = 50, seed = 1,mindepth=10) h2o.gbm.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed,mindepth=mindepth)
                                                                                      
h2o.gbm.3 <- function(..., ntrees = 100, max_depth = 10, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.gbm.4 <- function(..., ntrees = 100, col_sample_rate = 0.8, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.5 <- function(..., ntrees = 100, col_sample_rate = 0.7, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.6 <- function(..., ntrees = 100, col_sample_rate = 0.6, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.7 <- function(..., ntrees = 100, balance_classes = TRUE, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, balance_classes = balance_classes, seed = seed)
h2o.gbm.8 <- function(..., ntrees = 100, max_depth = 3, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.deeplearning.1 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.2 <- function(..., hidden = c(200,200,200), activation = "Tanh", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.3 <- function(..., hidden = c(500,500), activation = "RectifierWithDropout", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.4 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, balance_classes = TRUE, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, balance_classes = balance_classes, seed = seed)
h2o.deeplearning.5 <- function(..., hidden = c(100,100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.6 <- function(..., hidden = c(50,50), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.7 <- function(..., hidden = c(100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)











learner <- c("h2o.glm.1",
             "h2o.randomForest.1", "h2o.randomForest.2",
             "h2o.gbm.1", "h2o.gbm.2", "h2o.deeplearning.3", "h2o.deeplearning.3")


fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train,
                    
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))

perf <- h2o.ensemble_performance(g, newdata = valid)
print(perf,metric="MSE")


preds= (predict(g, newdata=test1))
predictions <- as.data.frame(preds$pred)
predictions=exp(predictions)

0.0127^0.5

library(pls)
set.seed (1000)
trainf$lsaleprice=k1$lsaleprice
pcr_model <- pcr(lsaleprice~., data = trainf, validation = "CV")

pcr_pred <- exp(predict(pcr_model, testf))

setwd("/Users/shivayogibiradar/Desktop/kaggle submissions")
write.csv(pcr_pred,file="pcr1.csv")

xgb <- h2o.xgboost(x = x
                   ,y = y
                   ,training_frame = train
                   ,validation_frame = valid
                   ,model_id = "xgb_model_1"
                   ,stopping_rounds = 3
                   ,stopping_metric = "RMSE"
                   ,distribution = "gaussian"
                   ,score_tree_interval = 1
                   ,learn_rate=0.1
                   ,ntrees=200
                   ,subsample = 0.75
                   ,colsample_bytree = 0.75
                   ,tree_method = "hist"
                   ,grow_policy = "depthwise"
                   ,booster = "gbtree"
                   ,gamma = 0.0
)

exp(predict(z, test1))

h2o.performance(xgb, newdata = test)


library(caret)

library(leaps)



set.seed(42)
cv_5 = trainControl(method = "cv", number = 5)

hit_elnet = train(
  lsaleprice ~ ., data = trainf,
  method = "glmnet",
  trControl = cv_5
)


hit_elnet

hit_elnet_int = train(
  lsaleprice ~ . ^ 2, data = trainf,
  method = "glmnet",
  trControl = cv_5,
  tuneLength = 10
)


get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}


get_best_result(hit_elnet_int)

trainf=as.data.frame(trainf)
trainf$lsaleprice=k$saleprice
x = ( trainf)[,-length(trainf)]
y = train$lsaleprice

fit_lasso_cv = cv.glmnet(x, y, alpha = 1)
sqrt(fit_lasso_cv$cvm[fit_lasso_cv$lambda == fit_lasso_cv$lambda.min]) # CV-RMSE minimum

preds=exp(predict(hit_elnet_int,testf))


setwd("/Users/shivayogibiradar/Desktop/kaggle submissions")
write.csv(as.data.frame(predictions),file="glm1.csv")
library(rpart)
library(MASS)
#install.packages("tree")
library(tree)
trainf=as.data.frame(trainf)
trainf$lsaleprice=k$lsaleprice
tree1 = tree(lsaleprice ~ ., data = trainf)

z=read.csv("lasso.csv")
k=as.data.frame(k)
z=as.data.frame(z)
Saleprice=rbind(k$SalePrice,z$SalePrice)

library(reshape2)

#install.packages("ggrepel")
library(ggrepel)
library(ggplot2)
library("scales")
k2=k
k2$price_per_sqfoot=k2$SalePrice/k2$totalsqft
k2$t=rescale(k2$price_per_sqfoot, to=c(-1,1))
k2$z=rescale(k2$OverallQual, to=c(-1,1))
k2$Neighborhood=as.factor(k2$Neighborhood)
k3=k2 %>% group_by(Neighborhood) %>% summarize(count=n(),price=mean(t),score=mean(z))

############Perception Plots
install.packages("ggalt")
library(ggalt)
df=k3 %>% filter(k3$price<0 & k3$score>0)
ggplot(data=k3,
       aes(x=price,
           y=score,
           label=Neighborhood))+
  geom_point(aes(color=Neighborhood))+
  labs(x = "Overall Quality Score",
       y = "Price Per Sq Feet",
       title = "Cluster Plot Of  Price per sqft vs Overall Quality of house")+
  lims(x=c(-1,1),
       y=c(-1,1)) +
  theme_minimal() +
  coord_fixed() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text_repel(aes(label=Neighborhood),
                  size=2.5,
                  box.padding = 0.25)+geom_encircle(aes(x=price, y=score), 
                                                    data=df, 
                                                    color="red", 
                                                    size=2, 
                                                    expand=0.02) 


library(tidyverse)






setwd("/Users/shivayogibiradar/Desktop/hw3-shivayogibiradar")
z=read.csv("kk1.csv")
z$pricepersqfoot=(z$SalePrice/z$totalsqft)
z$price=rescale(z$pricepersqfoot,to=c(-1,1))
z$dist=rescale(z$Iowa.State.University..IA,to=c(-1,1))
z1=z %>% 
  group_by(Neighborhood) %>%
  summarize(price=mean(price),dist=mean(dist),na.rm=TRUE)


df=z1 %>% filter(z1$price<0 & z1$dist<0)
ggplot(data=z1,
       aes(x=price,
           y=dist,
           label=Neighborhood))+
  geom_point(aes(color=Neighborhood))+
  labs(x = "Distance",
       y = "Price",
       title = "Custer plot of Price Per sqft vs Distance From Iowa State University")+
  lims(x=c(-1,1),
       y=c(-1,1)) +
  theme_minimal() +
  coord_fixed() + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_text_repel(aes(label=Neighborhood),
                  size=2.5,
                  box.padding = 0.25)+geom_encircle(aes(x=price, y=dist), 
                                                     data=df, 
                                                     color="red", 
                                                     size=2, 
                                                     expand=0.02) 











ggplot(data)

z$n
######City hall

z$dist=rescale(z$City.Hall.IA,to=c(-1,1))

z1=z %>% 
  group_by(Neighborhood) %>% 
  summarize(price=mean(price),
            dist=mean(dist),na.rm=TRUE)
df=z1 %>% filter(z1$price<0 & z1$dist<0)
ggplot(data=z1,
       aes(x=price,
           y=dist,
           label=Neighborhood))+
  geom_point(aes(color=Neighborhood))+
  labs(x = "Distance",
       y = "Price",
       title = "Perception plot of price per sqft vs City Hall/Downtown area")+
  lims(x=c(-1,1),
       y=c(-1,1)) +
  theme_minimal() +
  coord_fixed() + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_text_repel(aes(label=Neighborhood),
                  size=2.5,
                  box.padding = 0.25)+geom_encircle(aes(x=price, y=dist), 
                                                    data=df, 
                                                    color="red", 
                                                    size=2, 
                                                    expand=0.02) 





########Hospital


z$dist=min(z$Mcfarland.clinic.IA,z$Planned.Parenthood...Ames.Health.Center.IA,z$Story.County.Medical.Center.IA)
z$dist=rescale(z$City.Hall.IA,to=c(-1,1))

z1=z %>% 
  group_by(Neighborhood) %>% 
  summarize(price=mean(price),dist=mean(dist),na.rm=TRUE)
df=z1 %>% filter(z1$price<0 & z1$dist<0)
ggplot(data=z1,
       aes(x=price,
           y=dist,
           label=Neighborhood))+ 
  geom_point(aes(color=Neighborhood))+
  labs(x = "Distance",
       y = "Price",
       title = "Cluster plot of Price per Sqft Vs Distance from Hospitals")+
  lims(x=c(-1,1),y=c(-1,1)) +
  theme_minimal() + 
  coord_fixed() + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  geom_encircle(aes(x=price, y=dist), 
                                  data=df, 
                                         color="red", 
                                                     size=2, 
                                                     expand=0.02) +geom_text_repel(aes(label=Neighborhood),
                                                                                   size=2.5,
                                                                                   box.padding = 0.25)








library(h2o)
 localH2O = h2o.init(nthreads = -1)

m <- h2o.glm(x =x, y = y, data = train) #

g=h2o.glm(x=x,
         y=y,
         training_frame =train,
          family="gaussian",
        lambda_search = TRUE
    )


?h2o.glm()