 ########################################
 #
 # Machine learning
 #
 # Supervised learning
 #
 # Decision trees
 #
 # Edson Zangiacomi Martinez (2023)
 # 
 #######################################
 
 #Clear existing data and graphics
 rm(list=ls())
 graphics.off()
 library(rpart)
 library(rpart.plot)
 library(ipred)      # Improved predictive models by direct and indirect bootstrap aggregation
 library(adabag)     # Algoritmo de boosting adaptativo
 urlfile <- "https://raw.githubusercontent.com/edsonzmartinez/cursoR/main/lowbwt.csv"
 dados <- read.csv2(urlfile)
 head(dados)
 # Peso materno
 dados$peso.mae <- dados$LWT*0.453592
 # Visitas ao médico
 table(dados$FTV)
 dados$FTV2 <- ifelse(dados$FTV<3,dados$FTV,2)
 table(dados$FTV2)
 # Partos prematuros
 table(dados$PTL)
 dados$partos.prematuros <- ifelse(dados$PTL==0,0,1)
 table(dados$partos.prematuros)
 # Baixo peso ao nascer
 dados$resp<-ifelse(dados$LOW==1,"Baixo peso","Peso adequado")
 dados$resp.f <- factor(dados$resp)
 # refactor reference level
 dados$resp.f <- relevel(dados$resp.f, ref = "Peso adequado")
 # Decision tree
 fit <- rpart::rpart(resp.f ~ AGE + peso.mae + factor(RACE) + factor(SMOKE) + factor (partos.prematuros) + factor(HT) + factor(UI) + factor(FTV2), data=dados,method = 'class')
 rpart.plot(fit,type=4)
 rpart.rules(fit, cover = TRUE)
 # A confusion matrix based on this tree
 table(dados$resp.f)
 conf.freq <- table(dados$resp.f, predict(fit, type="class"))
 colnames(conf.freq) <- c("Predito como peso adequado", "Predito como baixo peso")
 conf.freq
 conf.matrix <- round(prop.table(table(dados$resp.f, predict(fit, type="class")), 2), 2)
 colnames(conf.matrix) <- c("Predito como peso adequado", "Predito como baixo peso")
 conf.matrix
 # Complexity parameter
 printcp(fit)
 plotcp(fit)
 # Prune
 fitprune <- prune(fit,cp=0.02)
 rpart.plot(fitprune,type=4)
 # Bagging
 set.seed(999)
 fitbag <- ipred::bagging(resp.f ~ AGE + peso.mae + factor (partos.prematuros) + factor(HT), data=dados, 
             nbagg = 500,  # number of bootstrap replications
             coob  = TRUE, # compute an out-of-bag estimate of the misclassification or mean-squared error
			 control = rpart.control(minsplit=20, cp=0.02)) # options that control details of the rpart algorithm
 fitbag
 conf.matrix.bag <- round(prop.table(table(dados$resp.f, predict(fitbag, type="class")), 2), 2)
 colnames(conf.matrix.bag) <- c("Predito como peso adequado", "Predito como baixo peso")
 conf.matrix.bag
 # Cada uma das árvores
 rpart.plot(fitbag$mtrees[[1]]$btree,type=4)
 rpart.plot(fitbag$mtrees[[2]]$btree,type=4)
 rpart.plot(fitbag$mtrees[[3]]$btree,type=4)
 rpart.plot(fitbag$mtrees[[4]]$btree,type=4)
 # Boosting
 set.seed(999)
 fitboo <- adabag::boosting(resp.f ~ AGE + peso.mae + factor (partos.prematuros) + factor(HT), data=dados, 
             mfinal = 100, # the number of iterations for which boosting is run 
             boos  = TRUE) # if TRUE (by default), a bootstrap sample of the training set is drawn using the
                           # weights for each observation on that iteratio
 fitboo
 fitboo$importance
 predict.boosting(fitboo, newdata=dados)$confusion
 # Boosting (melhor)
 m   <- length(dados[,1])
 sub <- sample(1:m,2*m/3)
 fitboo <- adabag::boosting(resp.f ~ AGE + peso.mae + factor (partos.prematuros) + factor(HT),data=dados[sub, ],mfinal = 100,
  control=rpart.control(maxdepth=5))
 fitboo.pred <- predict.boosting(fitboo,newdata=dados[-sub, ])
 fitboo.pred$confusion
 fitboo.pred$error
 #comparing error evolution in training and test set
 errorevol(fitboo,newdata=dados[sub, ])  -> evol.train
 errorevol(fitboo,newdata=dados[-sub, ]) -> evol.test
 plot.errorevol(evol.test,evol.train)
 # randomForest
 set.seed(100)
 dados2 <- dados
 dados2$RACE  <- factor(dados2$RACE)
 dados2$SMOKE <- factor(dados2$SMOKE)
 dados2$partos.prematuros <- factor(dados2$partos.prematuros)
 dados2$HT    <- factor(dados2$HT)
 dados2$UI    <- factor(dados2$UI)
 dados2$FTV2  <- factor(dados2$FTV2)
 samp  <- sample(nrow(dados2),0.7*nrow(dados2),replace=FALSE)
 train <- dados[samp,]
 valid <- dados[-samp,]
 model.fp <- randomForest::randomForest(resp.f ~ AGE + peso.mae + RACE + SMOKE + partos.prematuros + HT + UI + FTV2, data=train, mtry=6, importance=TRUE)
 model.fp
 #
 predtrain <- predict(model.fp,train,type="class")
 table(predtrain,train$resp.f)
 #
 predvalid <- predict(model.fp,valid,type="class")
 table(predvalid,valid$resp.f)
 # Variable Importance Plot
 randomForest::varImpPlot(model1,main="Variable Importance Plot")
