#################
# Aula 6        #
#################

# Regressão logística

####################################################################
# SIZE: 189 observations, 11 variables                             #
# SOURCE: Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013)   #
# Applied Logistic Regression: Third Edition.                      #
# These data are copyrighted by John Wiley & Sons Inc. and must    #
# be acknowledged and used accordingly. Data were collected at     #
# Baystate Medical Center, Springfield, Massachusetts during 1986. #
####################################################################
# ID    = Identification Code                                      #
# LOW   = Low Birth Weight Baby (1=Yes under 2500g, 0=No)          #
# AGE   = Mother's age in years                                    #
# LWT   = Weight at Last Period                                    # 
# RACE  = Race (1=White, 2=Black, 3=Other)                         #
# SMOKE = Smoke during Pregnancy (1=Yes, 0=No)                     #
# PTL   = History of Premature Labour (# of times)                 #
# HT    = History of Hypertension (1=Yes, 0=No)                    #
# UI    = Presence of Uterine Irritability (1=Yes, 0=No)           #
# FTV   = Visits to Doctor During 1st trimester                    #
# BWT   = Baby's birth Weight in Grams                             #
####################################################################
# Lendo a base de dados
urlfile="https://raw.githubusercontent.com/edsonzmartinez/cursoR/main/lowbwt.csv"
w <- read.csv2(urlfile)
w$y <- ifelse(w$BWT<2500,1,0)
# Convertendo peso, de libras para quilogramas
w$LWTKG <- w$LWT * 0.453592
# Primeiras 20 linhas
head(w,20)
# Histograma do peso da mãe no último período menstrual
hist(w$LWTKG,xlab="Peso da mãe no último período menstrual (kg)",ylab="Frequências",main="",col="#FF6666")
# Boxplot do peso da mãe
boxplot(w$LWTKG~w$y,xlab="Peso do RN",ylim=c(20,120),axes=FALSE,ylab="Peso da mãe no último período menstrual (kg)",col="#FF6666")
axis(2,las=2)
axis(1,at=1:2,labels=c("0: 2500kg ou mais","1: menos de 2500kg"))
# Tabelas peso vs. y
w$LWGcat <- cut(w$LWTKG,seq(30,120,10))
w$LWGcat
table(w$LWGcat,w$y)
prop.table(table(w$LWGcat,w$y),1)
# Regressão logística simples
# Peso da mãe como variável independente
model <- glm(y ~ LWTKG,family=binomial(link='logit'),data=w)
summary(model)
b <- coefficients(model)
b0 <- as.numeric(b[1])
b1 <- as.numeric(b[2])
# Função logito
logit.function <- function(x,b0,b1) return (exp(b0+b1*x)/(1+exp(b0+b1*x)))
logit.function(40,b0,b1)
logit.function(90,b0,b1)

# Preditos para idade materna de 40 kg e 90 kg
predict(model, newdata = list(LWTKG = c(40,90)), type = "response")
# Distância de Cook
plot(model, which = 4)

obs <- prop.table(table(w$LWGcat,w$y),1)
obs
kx <- seq(35,115,10)
ky <- logit.function(kx,b0,b1)
kc <- seq(30,120,0.001)
plot(kc,logit.function(kc,b0,b1),col="red",type="l",ylim=c(0,0.7),axes=FALSE,ylab="Probabilidade",xlab="Peso da mãe no último período menstrual (kg)")
points(kx,ky,pch=19,col="red")
points(kx,obs[,2],pch=19,col="blue")
axis(2,las=1)
axis(1,at=kx,labels=levels(w$LWGcat))


qLWT <- quantile(w$LWTKG, probs = seq(0,1,0.1))
w$LWGcatQ <- cut(w$LWTKG,as.numeric(qLWT))
obs <- prop.table(table(w$LWGcatQ,w$y),1)
obs
pmds <- c()
for (k in 1:10) pmds[k] <- (qLWT[k+1]+qLWT[k])/2
plot(kc,logit.function(kc,b0,b1),col="red",type="l",ylim=c(0,0.7),axes=FALSE,ylab="Probabilidade",xlab="Peso da mãe no último período menstrual (kg)")
points(pmds,logit.function(pmds,b0,b1),pch=19,col="red")
points(pmds,obs[,2],pch=19,col="blue")
axis(2,las=1)
axis(1,at=pmds,labels=levels(w$LWGcatQ))


# Regressão logística simples
# Logaritmo do peso da mãe como variável independente
model.log <- glm(y ~ log(LWTKG),family=binomial(link='logit'),data=w)
summary(model)
b.log <- coefficients(model.log)
b0.log <- as.numeric(b.log[1])
b1.log <- as.numeric(b.log[2])

qLWT <- quantile(w$LWTKG, probs = seq(0,1,0.1))
w$LWGcatQ <- cut(w$LWTKG,as.numeric(qLWT))
obs <- prop.table(table(w$LWGcatQ,w$y),1)
obs
pmds <- c()
for (k in 1:10) pmds[k] <- (qLWT[k+1]+qLWT[k])/2
plot(kc,logit.function(kc,b0,b1),col="red",type="l",ylim=c(0,0.7),axes=FALSE,ylab="Probabilidade",xlab="Peso da mãe no último período menstrual (kg)")
points(kc,logit.function(log(kc),b0.log,b1.log),col="limegreen",type="l")
points(pmds,logit.function(pmds,b0,b1),pch=19,col="red")
points(pmds,logit.function(log(pmds),b0.log,b1.log),pch=19,col="limegreen")
points(pmds,obs[,2],pch=19,col="blue")
axis(2,las=1)
axis(1,at=pmds,labels=levels(w$LWGcatQ))


# Regressão logística simples
# Tabagismo como variável independente
model <- glm(y ~ SMOKE,family=binomial(link='logit'),data=w)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
# Preditos para mães não tabagistas e tabagistas
predict(model, newdata = list(SMOKE = c(0,1)), type = "response")

# Regressão logística simples
# Cor da pele como variável independente
model <- glm(y ~ RACE,family=binomial(link='logit'),data=w) # ERRADO!!!!!
summary(model)
model <- glm(y ~ factor(RACE),family=binomial(link='logit'),data=w) # CORRETO
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# Preditos para cada classe da variável
predict(model, newdata = list(RACE = c(0,1)), type = "response")

# Preparando as variáveis

# LOW   = Low Birth Weight Baby (1=Yes under 2500g, 0=No)          #
# AGE   = Mother's age in years                                    #
# LWT   = Weight at Last Period                                    # 
# RACE  = Race (1=White, 2=Black, 3=Other)                         #
# SMOKE = Smoke during Pregnancy (1=Yes, 0=No)                     #
# PTL   = History of Premature Labour (# of times)                 #
# HT    = History of Hypertension (1=Yes, 0=No)                    #
# UI    = Presence of Uterine Irritability (1=Yes, 0=No)           #
# FTV   = Visits to Doctor During 1st trimester                    #
# BWT   = Baby's birth Weight in grams                             #

table(w$PTL)
table(w$FTV)
w$PTL.c <- w$PTL
w$PTL.c[w$PTL>1] <- 1
w$FTV.c <- w$FTV
w$FTV.c[w$FTV>2] <- 2
w$LWTKG.c <- cut(w$LWTKG,breaks=c(30,50,55,65,120))
w$AGE.c   <- cut(w$AGE,  breaks=c(10,19,25,30,50))

table(w$PTL.c)
table(w$FTV.c)
table(w$LWTKG.c)
round(100*prop.table(table(w$LWTKG.c)),1)
table(w$AGE.c)
round(100*prop.table(table(w$AGE.c)),1)
# Baixo peso ao nascer de acordo com as faixas etárias
table(w$LWTKG.c,w$y)
k <- round(100*prop.table(table(w$LWTKG.c,w$y),1),1)
plot(1:4,k[,2],pch=19,col="red",bty="l",ylab="Frequência relativa (%)",xlab="Peso da mãe (kg)",ylim=c(0,50),axes=FALSE)
axis(2,las=1)
axis(1,at=1:4,labels=c("(30,50]","(50,55]","(55,65]","(65,120]"))
 
# Regressão logística simples
# Peso da mãe (categorizado) como variável independente
model <- glm(y ~ relevel(LWTKG.c, ref = "(65,120]"),family=binomial(link='logit'),data=w) 
summary(model)
exp(cbind(OR = coef(model), confint(model)))
# Idade da mãe (categorizado) como variável independente
model <- glm(y ~ relevel(AGE.c, ref = "(30,50]"),family=binomial(link='logit'),data=w) 
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# Regressão logística múltipla
# Irritabilidde uterina e história de partos prematuros como variáveis independentes
model <- glm(y ~ UI + PTL.c,family=binomial(link='logit'),data=w) 
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# Modelo com todas as variáveis
model <- glm(y ~ relevel(AGE.c, ref = "(30,50]") + relevel(LWTKG.c, ref = "(65,120]") 
      + factor(RACE) + SMOKE + UI + PTL.c + factor(FTV.c) + HT,family=binomial(link='logit'),data=w) 
summary(model)
round(exp(cbind(OR = coef(model), confint(model))),2)
# Distância de Cook
plot(model, which = 4)




