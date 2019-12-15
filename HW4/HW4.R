rm(list = ls())

require(data.table)
require(zoo)

setwd("C:/Users/baris.isik/Desktop/Master/IE 582/fall19-barsssk/HW4")
set.seed(seed = 7)
# source("functions.r")

Matches <- fread("matches.csv")
Stats <- fread("stats.csv")

Matches <- Matches[match_status == "Finished"
                   ,.(M_id = match_id,Date = as.Date(as.POSIXct(epoch,origin = "1970-01-01")),
                      H_id = match_hometeam_id, A_id = match_awayteam_id, H_Score = match_hometeam_score,
                      A_Score = match_awayteam_score)]

Matches[,RV_Score := H_Score + A_Score]
Matches[,RV_CatScore := as.factor(ifelse(RV_Score < 2.5,0,1))]

H_Matches <- Matches[,.(M_id,Date,HomeAway = "H",T_id = H_id,Score = H_Score,Opponent_Score = A_Score)]
H_Matches[,Row := .N, by = .(Date,T_id)]

A_Matches <- Matches[,.(M_id,Date,HomeAway = "A",T_id = A_id,Score = A_Score,Opponent_Score = H_Score)]
A_Matches[,Row := .N, by = .(Date,T_id)]

# if there is more than one game of team per day we considered it as duplicated game and removed
duplicated_matches <- unique(c(H_Matches[Row > 1, M_id],A_Matches[Row > 1, M_id]))

H_Stats <- Stats[,.(M_id = match_id,HomeAway = "H", BallPossesion = home_BallPossession,
                    CornerKicks = home_CornerKicks, GoalAttempts = home_GoalAttempts,
                    ShotsOnGoal = home_ShotsonGoal, GoalKeeperPerformance = (away_ShotsonGoal - home_GoalkeeperSaves) / away_ShotsonGoal)]

A_Stats <- Stats[,.(M_id = match_id,HomeAway = "A", BallPossesion = away_BallPossession,
                    CornerKicks = away_CornerKicks, GoalAttempts = away_GoalAttempts,
                    ShotsOnGoal = away_ShotsonGoal, GoalKeeperPerformance = (home_ShotsonGoal - away_GoalkeeperSaves) / home_ShotsonGoal)]

Features <- rbind(H_Matches,A_Matches)
Stats <- rbind(H_Stats,A_Stats)
Stats[,GoalKeeperPerformance := ifelse(is.na(GoalKeeperPerformance),1,
                                       ifelse(GoalKeeperPerformance < 0,0,
                                              ifelse(GoalKeeperPerformance > 1,1,GoalKeeperPerformance)))]

# remove duplicated matches
Features <- Features[!(M_id %in% duplicated_matches)]
Features[,Row := NULL]
Features <- merge(x = Features, y = Stats, by = c("M_id", "HomeAway"),all = F)

# convert ball possession to numeric
Features[,BallPossesion := as.integer(gsub(x = BallPossesion, pattern = "%",replacement = ""))/100]
Normalize_Fit <- function(Data = NULL, cols = NULL){
  mincols <- NULL
  rangecols <- NULL
  for(i in cols){
    
    mintemp <- min(Data[,get(i)],na.rm = T)
    rangetemp <- max(Data[,get(i)],na.rm = T) - mintemp
    
    mincols <- c(mincols,mintemp)
    rangecols <- c(rangecols,rangetemp)
    
  }
  
  return(list(cols = cols, min = mincols, range = rangecols))
  
}
Normalize_Predict <- function(Data = NULL, Normalizer = NULL){
  
  counter <- 1
  for(i in Normalizer$cols){
    Data[,(i) := (get(i) - Normalizer$min[counter]) / Normalizer$range[counter]]
    counter <- counter + 1
  }
}

# to calculate attacking power we use
Features[,TempScore := ifelse(Score <5,Score,5)]
Features[,`:=`(TempScore = ifelse(Score < quantile(Score,probs = c(0.95),na.rm = T)+1,Score,quantile(Score,probs = c(0.95),na.rm = T)+1),
               CornerKicks = ifelse(CornerKicks < quantile(CornerKicks,probs = c(0.9),na.rm = T)+1,CornerKicks,quantile(CornerKicks,probs = c(0.9),na.rm = T)+1),
               GoalAttempts = ifelse(GoalAttempts < quantile(GoalAttempts,probs = c(0.9),na.rm = T)+1,GoalAttempts,quantile(GoalAttempts,probs = c(0.9),na.rm = T)+1),
               ShotsOnGoal = ifelse(ShotsOnGoal < quantile(ShotsOnGoal,probs = c(0.9),na.rm = T)+1,ShotsOnGoal,quantile(ShotsOnGoal,probs = c(0.9),na.rm = T)+1))]

Normalizer <- Normalize_Fit(Data = Features,cols = c("TempScore","CornerKicks","GoalAttempts","ShotsOnGoal","BallPossesion"))
Normalize_Predict(Data = Features, Normalizer = Normalizer)

AreaOfSpider <- function(Arrays = NULL){
  Area <- 0
  # Arrays  <- unlist(Arrays)
  len <- length(Arrays)
  Arrays <- sort(x = Arrays,decreasing = T)
  Arrays <- Arrays[1:len-1]
  len <- len-1
  angle <- 360/len
  Arrays[len + 1] <- Arrays[1]
  for(i in 1:len){
    Area <- Area + sin(pi*angle/180)*Arrays[i]*Arrays[i+1]/2
  }
  return(Area)
}
Features[,Row := .I]
Features[, AttackingPower := AreaOfSpider(Arrays = c(TempScore,CornerKicks,GoalAttempts,ShotsOnGoal,BallPossesion)),by = Row]

Features[,c("TempScore","CornerKicks","GoalAttempts","ShotsOnGoal","BallPossesion","Row") := NULL]

Features[,Season := ifelse(Date < as.Date("2018-06-15"),"2017-2018",
                           ifelse(Date < as.Date("2019-06-15"),"2018-2019","2019-2020"))]

setorder(Features,T_id,Date)
Features[,Match_No := seq_len(.N), by = .(T_id,Season)]

# home performance of each team
H_Features <- copy(Features[HomeAway == "H"])
# shift all features by 1 since we wont use current match stats
shiftcols <- c("Score","Opponent_Score","GoalKeeperPerformance","AttackingPower")
H_Features[,(shiftcols) := lapply(.SD,FUN = function(x){
  shift(x = x,n = 1,fill = NA)
}),.SDcols = shiftcols, by = .(T_id,Season)]

for(i in shiftcols){
  newname <- paste0("H_Avg_",i,"_Last3HomeGames")
  H_Features[,(newname):= rollmean(x = get(i),k = 3,fill = NA,align = "right")]
}
H_Features <- H_Features[,c("M_id","T_id",paste0("H_Avg_",shiftcols,"_Last3HomeGames")),with = F]

# away performance of each team
A_Features <- copy(Features[HomeAway == "A"])
# shift all features by 1 since we  wont use current match stats
shiftcols <- c("Score","Opponent_Score","GoalKeeperPerformance","AttackingPower")
A_Features[,(shiftcols) := lapply(.SD,FUN = function(x){
  shift(x = x,n = 1,fill = NA)
}),.SDcols = shiftcols, by = .(T_id,Season)]

for(i in shiftcols){
  newname <- paste0("A_Avg_",i,"_Last3AwayGames")
  A_Features[,(newname):= rollmean(x = get(i),k = 3,fill = NA,align = "right")]
}
A_Features <- A_Features[,c("M_id","T_id",paste0("A_Avg_",shiftcols,"_Last3AwayGames")),with = F]

# overall performance of each team
Features[,TotalScore := Score + Opponent_Score]
shiftcols <- c("TotalScore","GoalKeeperPerformance","AttackingPower")

Features[,(shiftcols) := lapply(.SD,FUN = function(x){
  shift(x = x,n = 1,fill = NA)
}),.SDcols = shiftcols, by = .(T_id,Season)]

for(i in shiftcols){
  newname <- paste0("Avg_",i,"_Last5Games")
  Features[,(newname):= rollmean(x = get(i),k = 5,fill = NA,align = "right")]
}

Features <- Features[,c("M_id","HomeAway","T_id",shiftcols,paste0("Avg_",shiftcols,"_Last5Games")),with = F]

Matches <- Matches[,.(M_id,H_id,A_id,RV_Score,RV_CatScore)]
Backup <- copy(Matches)
Matches <- copy(Backup)

# join home features
Matches <- merge(x = Matches, y = H_Features, by.x = c("M_id","H_id"), by.y = c("M_id","T_id"),all = F)
Matches <- merge(x = Matches, y = Features[HomeAway == "H"], by.x = c("M_id","H_id"), by.y = c("M_id","T_id"),all = F)
Matches[,HomeAway := NULL]

oldnames <- names(Matches)[(ncol(Matches)-5):ncol(Matches)]
setnames(Matches,oldnames,paste0("H_",oldnames))

# join away features
Matches <- merge(x = Matches, y = A_Features, by.x = c("M_id","A_id"), by.y = c("M_id","T_id"),all = F)
Matches <- merge(x = Matches, y = Features[HomeAway == "A"], by.x = c("M_id","A_id"), by.y = c("M_id","T_id"),all = F)
Matches[,HomeAway := NULL]

oldnames <- names(Matches)[(ncol(Matches)-5):ncol(Matches)]
setnames(Matches,oldnames,paste0("A_",oldnames))


Matches  <- na.omit(Matches)
FeatureCols <- names(Matches)[6:ncol(Matches)]

#### Lasso ####
require(glmnet)
LassoReg_Data <- copy(Matches[,c("RV_Score",FeatureCols),with = F])
LassoCls_Data <- copy(Matches[,c("RV_CatScore",FeatureCols),with = F])

Normalizer <- Normalize_Fit(Data = LassoReg_Data,cols = FeatureCols)
Normalize_Predict(Data = LassoReg_Data, Normalizer = Normalizer)

Normalizer <- Normalize_Fit(Data = LassoCls_Data,cols = FeatureCols)
Normalize_Predict(Data = LassoCls_Data, Normalizer = Normalizer)

LassoReg_Results <- NULL
LassoCls_Results <- NULL

# regression
x_reg <- model.matrix(RV_Score~.,data=LassoReg_Data)
y_reg <- LassoReg_Data[,RV_Score]

MSEs <- NULL
for(i in 1:50){
  cv <- cv.glmnet(y = y_reg, x = x_reg, alpha=1, nfolds=5,family = "gaussian")  
  MSEs <- cbind(MSEs, cv$cvm)
}
rownames(MSEs) <- cv$lambda
SelectedLambda_Reg <- as.numeric(names(which.min(rowMeans(MSEs))))

# classification
x_cls <- model.matrix(RV_CatScore~.,data=LassoCls_Data)
y_cls <- LassoCls_Data[,RV_CatScore]

MSEs <- NULL
for(i in 1:50){
  cv <- cv.glmnet(y = y_cls, x = x_cls, alpha=1, nfolds=5,family = "binomial")  
  MSEs <- cbind(MSEs, cv$cvm)
}
rownames(MSEs) <- cv$lambda
SelectedLambda_Cls <- as.numeric(names(which.min(rowMeans(MSEs))))

#### Decision Tree ####
require(rpart)
TreeReg_Data <- copy(Matches[,c("RV_Score",FeatureCols),with = F])
TreeCls_Data <- copy(Matches[,c("RV_CatScore",FeatureCols),with = F])

TreeCV <- NULL
for(i in c(10,50,100,200)){
  for(j in c(0.01,0.005,0.001)){
    Tree_reg <- rpart(formula = RV_Score~.,data = TreeReg_Data,method = "anova",
                      control = rpart.control(minsplit = i,cp = j))
    cptable_reg <- as.data.table(Tree_reg$cptable)
    Temp <- data.table(class = "Reg", minsplit = i, cp = j, cv_error = cptable_reg[nrow(cptable_reg),xerror])
    TreeCV <- rbind(TreeCV,Temp)
    Tree_cls <- rpart(formula = RV_CatScore~.,data = TreeCls_Data, method = "class",
                      control = rpart.control(minsplit = i,cp = j))
    cptable_cls <- as.data.table(Tree_cls$cptable)
    Temp <- data.table(class = "Cls",minsplit = i, cp = j, cv_error = cptable_cls[nrow(cptable_cls),xerror])
    TreeCV <- rbind(TreeCV,Temp)
  }
}

setorder(TreeCV,class,cv_error)
TreeCV[,Row := seq_len(.N),by = class]
TreeCV <- TreeCV[Row == 1]

#### Random Forest ####
require(randomForest)
RforestReg_Data <- copy(Matches[,c("RV_Score",FeatureCols),with = F])
RforestCls_Data <- copy(Matches[,c("RV_CatScore",FeatureCols),with = F])

Rforest_Reg <- rfcv(trainx = RforestReg_Data[,FeatureCols,with = F], trainy = RforestReg_Data[,RV_Score], 
                    cv.fold=5,ntree = 500)

Selected_m_Rforest_Reg <- as.integer(names(which.min(Rforest_Reg$error.cv[which(Rforest_Reg$n.var <= length(FeatureCols)/2)])))

Rforest_Cls <- rfcv(trainx = RforestCls_Data[,FeatureCols,with = F], trainy = RforestCls_Data[,RV_CatScore], 
                    cv.fold=5,ntree = 500)

Selected_m_Rforest_Cls <- as.integer(names(which.min(Rforest_Cls$error.cv[which(Rforest_Cls$n.var <= length(FeatureCols)/2)])))


#### Gbm ####
require(gbm)
GbmReg_Data <- copy(Matches[,c("RV_Score",FeatureCols),with = F])
GbmCls_Data <- copy(Matches[,c("RV_CatScore",FeatureCols),with = F])
GbmCls_Data[,RV_CatScore:=as.numeric(as.character(RV_CatScore)) ]
GbmCV <- NULL
for(j in c(0.1,0.05)){
  for(k in c(3,5)){
    Gbm_Reg <- gbm(formula = RV_Score~.,data = GbmReg_Data,
                   distribution = "gaussian",cv.folds = 5,n.trees = 1000,shrinkage = j,interaction.depth = k)
    
    Temp <- data.table(class = "Reg",ntrees = which.min(Gbm_Reg$cv.error),shrinkage = j,depth = k,error = min(Gbm_Reg$cv.error))
    GbmCV <- rbind(GbmCV,Temp)
    
    Gbm_Cls <- gbm(formula = RV_CatScore~.,data = GbmCls_Data,
                   distribution = "bernoulli",cv.folds = 5,n.trees = 1000,shrinkage = j,interaction.depth = k)
    Temp <- data.table(class = "Cls",ntrees = which.min(Gbm_Cls$cv.error),shrinkage = j,depth = k,error = min(Gbm_Cls$cv.error))
    GbmCV <- rbind(GbmCV,Temp)
  }
}

setorder(GbmCV,class,error)
GbmCV[,Row := seq_len(.N),by = class]
GbmCV <- GbmCV[Row == 1]
GbmCV[,Row := NULL]

#### 3 ####
Matches[,`:=`(Rand_1 = runif(n = nrow(Matches),min = 0,max = 100),
              Rand_2 = runif(n = nrow(Matches),min = 0,max = 100),
              Rand_3 = runif(n = nrow(Matches),min = 0,max = 100))]

Matches[,`:=`(TrainTest_1 = ifelse(Rand_1 < 25,"Test","Train"),
              TrainTest_2 = ifelse(Rand_2 < 25,"Test","Train"),
              TrainTest_3 = ifelse(Rand_3 < 25,"Test","Train"),
              Rand_1 = NULL, Rand_2 = NULL, Rand_3 = NULL)]

Results_Reg <- NULL
Results_Cls <- NULL

reg_rmse <- function(x,y){
  return((sum((x-y)^2)/length(x))^0.5)
}
cls_accuracy <- function(x,y){
  return(sum(x==y)/length(x))
}

for(i in 1:3){
  Train_Reg_data <- Matches[get(paste0("TrainTest_",i)) == "Train",c("RV_Score",FeatureCols),with = F]
  Test_Reg_data <- Matches[get(paste0("TrainTest_",i)) == "Test",c("RV_Score",FeatureCols),with = F]
  
  Train_Cls_data <- Matches[get(paste0("TrainTest_",i)) == "Train",c("RV_CatScore",FeatureCols),with = F]
  Test_Cls_data <- Matches[get(paste0("TrainTest_",i)) == "Test",c("RV_CatScore",FeatureCols),with = F]
  
  # gbm regression
  fit_gbm_reg <- gbm(formula = RV_Score~.,data = Train_Reg_data,
                     distribution = "gaussian",n.trees = GbmCV[class == "Reg",ntrees],
                     shrinkage = GbmCV[class == "Reg",shrinkage],interaction.depth = GbmCV[class == "Reg",depth])
  
  error <- reg_rmse(x = Train_Reg_data[,RV_Score], y = predict(object = fit_gbm_reg,newdata = Train_Reg_data,n.trees = GbmCV[class == "Reg",ntrees]))
  temp <- data.table(run = i,method = "gbm",data = "train",rmse = error)
  Results_Reg <- rbind(Results_Reg,temp)
  
  error <- reg_rmse(x = Test_Reg_data[,RV_Score], y = predict(object = fit_gbm_reg,newdata = Test_Reg_data,n.trees = GbmCV[class == "Reg",ntrees]))
  temp <- data.table(run = i,method = "gbm",data = "test",rmse = error)
  Results_Reg <- rbind(Results_Reg,temp)
  
  
  # gbm classification
  Train_Cls_data[,RV_CatScore:=as.numeric(as.character(RV_CatScore))]
  fit_gbm_cls <- gbm(formula = RV_CatScore~.,data = Train_Cls_data,
                     distribution = "bernoulli",n.trees = GbmCV[class == "Cls",ntrees],
                     shrinkage = GbmCV[class == "Cls",shrinkage],interaction.depth = GbmCV[class == "Cls",depth])
  
  error <- 1- cls_accuracy(x = Train_Cls_data[,RV_CatScore], y = ifelse(predict(object = fit_gbm_cls,newdata = Train_Cls_data,n.trees = GbmCV[class == "Cls",ntrees],type = "response")>0.5,1,0))
  temp <- data.table(run = i,method = "gbm",data = "train",missclassification = error)
  Results_Cls <- rbind(Results_Cls,temp)
  
  error <- 1- cls_accuracy(x = Test_Cls_data[,RV_CatScore], y = ifelse(predict(object = fit_gbm_cls,newdata = Test_Cls_data,n.trees = GbmCV[class == "Cls",ntrees],type = "response")>0.5,1,0))
  temp <- data.table(run = i,method = "gbm",data = "test",missclassification = error)
  Results_Cls <- rbind(Results_Cls,temp)
  
  # rforest regression
  fit_rforest_reg <- randomForest(RV_Score ~., data = RforestReg_Data,ntree =  500, nodesize = 5, 
                                  mtry = Selected_m_Rforest_Reg,keep.forest = T,check.names = T)  
  
  error <- reg_rmse(x = Train_Reg_data[,RV_Score], y = predict(object = fit_rforest_reg,newdata = Train_Reg_data))
  temp <- data.table(run = i,method = "rforest",data = "train",rmse = error)
  Results_Reg <- rbind(Results_Reg,temp)
  
  error <- reg_rmse(x = Test_Reg_data[,RV_Score], y = predict(object = fit_rforest_reg,newdata = Test_Reg_data))
  temp <- data.table(run = i,method = "rforest",data = "test",rmse = error)
  Results_Reg <- rbind(Results_Reg,temp)
  
  # rforest classification
  Train_Cls_data[,RV_CatScore:=as.factor(RV_CatScore)]
  fit_rforest_cls <- randomForest(RV_CatScore ~., data = Train_Cls_data,ntree =  500, nodesize = 5, 
                                  mtry = Selected_m_Rforest_Cls,keep.forest = T,check.names = T)  
  
  error <- 1 - cls_accuracy(x = Train_Cls_data[,RV_CatScore], y = predict(object = fit_rforest_cls,newdata = Train_Cls_data))
  temp <- data.table(run = i,method = "rforest",data = "train",missclassification = error)
  Results_Cls <- rbind(Results_Cls,temp)
  
  error <- 1- cls_accuracy(x = Test_Cls_data[,RV_CatScore], y = predict(object = fit_rforest_cls,newdata = Test_Cls_data))
  temp <- data.table(run = i,method = "rforest",data = "test",missclassification = error)
  Results_Cls <- rbind(Results_Cls,temp)
  
  # tree regression
  fit_tree_reg <- Tree_reg <- rpart(formula = RV_Score~.,data = Train_Reg_data,method = "anova",
                                    control = rpart.control(minsplit = TreeCV[class=="Reg",minsplit],
                                                            cp = TreeCV[class=="Reg",cp]))
  
  error <- reg_rmse(x = Train_Reg_data[,RV_Score], y = predict(object = fit_tree_reg,newdata = Train_Reg_data))
  temp <- data.table(run = i,method = "decision tree",data = "train",rmse = error)
  Results_Reg <- rbind(Results_Reg,temp)
  
  error <- reg_rmse(x = Test_Reg_data[,RV_Score], y = predict(object = fit_tree_reg,newdata = Test_Reg_data))
  temp <- data.table(run = i,method = "decision tree",data = "test",rmse = error)
  Results_Reg <- rbind(Results_Reg,temp)
  
  # tree classification
  fit_tree_cls <- rpart(formula = RV_CatScore~.,data = Train_Cls_data, method = "class",
                           control = rpart.control(minsplit = TreeCV[class=="Cls",minsplit],
                                                   cp = TreeCV[class=="Cls",cp]))
  
  error <- 1 - cls_accuracy(x = Train_Cls_data[,RV_CatScore], y = predict(object = fit_tree_cls,newdata = Train_Cls_data,type = "class"))
  temp <- data.table(run = i,method = "decision tree",data = "train",missclassification = error)
  Results_Cls <- rbind(Results_Cls,temp)
  
  error <- 1- cls_accuracy(x = Test_Cls_data[,RV_CatScore], y = predict(object = fit_tree_cls,newdata = Test_Cls_data,type = "class"))
  temp <- data.table(run = i,method = "decision tree",data = "test",missclassification = error)
  Results_Cls <- rbind(Results_Cls,temp)
  
  # lasso regression
  Normalizer <- Normalize_Fit(Data = Train_Reg_data,cols = FeatureCols)
  Normalize_Predict(Data = Train_Reg_data, Normalizer = Normalizer)
  Normalize_Predict(Data = Test_Reg_data, Normalizer = Normalizer)
  
  x_reg <- model.matrix(RV_Score~.,data=Train_Reg_data)
  y_reg <- Train_Reg_data[,RV_Score]
  
  fit_lasso_reg <- glmnet(x = x_reg,y = y_reg, alpha = 1,lambda = SelectedLambda_Reg, family = "gaussian")
  
  error <- reg_rmse(x = y_reg, y = predict(object = fit_lasso_reg,newx = x_reg))
  temp <- data.table(run = i,method = "lasso",data = "train",rmse = error)
  Results_Reg <- rbind(Results_Reg,temp)
  
  y_reg <- Test_Reg_data[,RV_Score]
  x_reg <- model.matrix(~.,data=Test_Reg_data[,2:ncol(Test_Reg_data)])
  
  error <- reg_rmse(x = y_reg, y = predict(object = fit_lasso_reg,newx = x_reg))
  temp <- data.table(run = i,method = "lasso",data = "test",rmse = error)
  Results_Reg <- rbind(Results_Reg,temp)
  
  # lasso classification
  Normalizer <- Normalize_Fit(Data = Train_Cls_data,cols = FeatureCols)
  Normalize_Predict(Data = Train_Cls_data, Normalizer = Normalizer)
  Normalize_Predict(Data = Test_Cls_data, Normalizer = Normalizer)
  
  x_cls <- model.matrix(RV_CatScore~.,data=Train_Cls_data)
  y_cls <- Train_Cls_data[,RV_CatScore]
  
  fit_lasso_cls <- glmnet(x = x_cls,y = y_cls, alpha = 1,lambda = SelectedLambda_Reg, family = "binomial")
  
  error <- 1- cls_accuracy(x = y_cls, y = ifelse(predict(object = fit_lasso_cls,newx = x_cls,type = "response")>0.5,1,0))
  temp <- data.table(run = i,method = "lasso",data = "train",missclassification = error)
  Results_Cls <- rbind(Results_Cls,temp)
  
  y_cls <- Test_Cls_data[,RV_CatScore]
  x_cls <- model.matrix(~.,data=Test_Cls_data[,2:ncol(Train_Cls_data)])
  
  error <- 1- cls_accuracy(x = y_cls, y = ifelse(predict(object = fit_lasso_cls,newx = x_cls,type = "response")>0.5,1,0))
  temp <- data.table(run = i,method = "lasso",data = "test",missclassification = error)
  Results_Cls <- rbind(Results_Cls,temp)
  
}

Results_Cls <- Results_Cls[,.(missclassification = mean(missclassification)), by = .(method,data)]
Results_Reg <- Results_Reg[,.(rmse = mean(rmse)), by = .(method,data)]




