#################
# Aula 5        #
#################

# Effects of multicollinearity

# Usaremos a função vif() do pacote car 
# e envelopes para os resíduos, usando
# o pacote glmtoolbox
library(car)
library(glmtoolbox)

# Dados obtidos do site https://pages.cs.wisc.edu/~songwang/regression.html

urlfile <- "https://raw.githubusercontent.com/edsonzmartinez/cursoR/main/bloodpress.csv"

w <- read.csv(urlfile,head=TRUE,sep=";",dec=".")

##################################################
# 20 individuals with high blood pressure:       #
##################################################
# blood pressure (y = BP, in mm Hg)              #
# age (x1 = Age, in years)                       #
# weight (x2 = Weight, in kg)                    #
# body surface area (x3 = BSA, in sq m)          #
# duration of hypertension (x4 = Dur, in years)  #
# basal pulse (x5 = Pulse, in beats per minute)  #
# stress index (x6 = Stress)                     #
##################################################

# Correlações de Pearson
corl <- round(cor(w[,-1]),3)
# Tamanho da amostra
n <- dim(w)[1]

# Gráficos de dispersão
layout(matrix(1:36,ncol=6))
par(mar=c(4,4,1,1))
cc <- rep("black",n)
# cc[7] <- "red"
plot(w$Age,   w$BP,pch=19,las=1,xlab="Age",ylab="BP",bty="l",col=cc)
plot(w$Weight,w$BP,pch=19,las=1,xlab="Weight",ylab="BP",bty="l",col=cc)
plot(w$BSA,   w$BP,pch=19,las=1,xlab="BSA",ylab="BP",bty="l",col=cc)
plot(w$Dur,   w$BP,pch=19,las=1,xlab="Dur",ylab="BP",bty="l",col=cc)
plot(w$Pulse, w$BP,pch=19,las=1,xlab="Pulse",ylab="BP",bty="l",col=cc)
plot(w$Stress,w$BP,pch=19,las=1,xlab="Stress",ylab="BP",bty="l",col=cc)
plot(1,1,pch=NA,axes=FALSE,xlab="",ylab="")
plot(w$Weight,w$Age,pch=19,las=1,xlab="Weight",ylab="Age",bty="l",col=cc)
plot(w$BSA,   w$Age,pch=19,las=1,xlab="BSA",ylab="Age",bty="l",col=cc)
plot(w$Dur,   w$Age,pch=19,las=1,xlab="Dur",ylab="Age",bty="l",col=cc)
plot(w$Pulse, w$Age,pch=19,las=1,xlab="Pulse",ylab="Age",bty="l",col=cc)
plot(w$Stress,w$Age,pch=19,las=1,xlab="Stress",ylab="Age",bty="l",col=cc)
for (k in 1:2) plot(1,1,pch=NA,axes=FALSE,xlab="",ylab="")
plot(w$BSA,   w$Weight,pch=19,las=1,xlab="BSA",ylab="Weight",bty="l",col=cc)
plot(w$Dur,   w$Weight,pch=19,las=1,xlab="Dur",ylab="Weight",bty="l",col=cc)
plot(w$Pulse, w$Weight,pch=19,las=1,xlab="Pulse",ylab="Weight",bty="l",col=cc)
plot(w$Stress,w$Weight,pch=19,las=1,xlab="Stress",ylab="Weight",bty="l",col=cc)
for (k in 1:3) plot(1,1,pch=NA,axes=FALSE,xlab="",ylab="")
plot(w$Dur,   w$BSA,pch=19,las=1,xlab="Dur",ylab="BSA",bty="l",col=cc)
plot(w$Pulse, w$BSA,pch=19,las=1,xlab="Pulse",ylab="BSA",bty="l",col=cc)
plot(w$Stress,w$BSA,pch=19,las=1,xlab="Stress",ylab="BSA",bty="l",col=cc)
for (k in 1:4) plot(1,1,pch=NA,axes=FALSE,xlab="",ylab="")
plot(w$Pulse, w$Dur,pch=19,las=1,xlab="Pulse",ylab="Dur",bty="l",col=cc)
plot(w$Stress,w$Dur,pch=19,las=1,xlab="Stress",ylab="Dur",bty="l",col=cc)
for (k in 1:5) plot(1,1,pch=NA,axes=FALSE,xlab="",ylab="")
plot(w$Stress,w$Pulse,pch=19,las=1,xlab="Stress",ylab="Pulse",bty="l",col=cc)

# Gráficos de dispersão com correlações
pm <- function(x) (max(x)+min(x))/2
layout(matrix(1:36,ncol=6))
par(mar=c(4,4,1,1))
plot(w$Age,   w$BP,pch=19,las=1,xlab="Age",ylab="BP",bty="l",col=gray(0.8));    text(pm(w$Age),   pm(w$BP),round(cor(w$Age,   w$BP),4),col="red",cex=2,font=2)
plot(w$Weight,w$BP,pch=19,las=1,xlab="Weight",ylab="BP",bty="l",col=gray(0.8)); text(pm(w$Weight),pm(w$BP),round(cor(w$Weight,w$BP),4),col="red",cex=2,font=2)
plot(w$BSA,   w$BP,pch=19,las=1,xlab="BSA",ylab="BP",bty="l",col=gray(0.8));    text(pm(w$BSA),   pm(w$BP),round(cor(w$BSA,   w$BP),4),col="red",cex=2,font=2)
plot(w$Dur,   w$BP,pch=19,las=1,xlab="Dur",ylab="BP",bty="l",col=gray(0.8));    text(pm(w$Dur),   pm(w$BP),round(cor(w$Dur,   w$BP),4),col="red",cex=2,font=2)
plot(w$Pulse, w$BP,pch=19,las=1,xlab="Pulse",ylab="BP",bty="l",col=gray(0.8));  text(pm(w$Pulse), pm(w$BP),round(cor(w$Pulse, w$BP),4),col="red",cex=2,font=2)
plot(w$Stress,w$BP,pch=19,las=1,xlab="Stress",ylab="BP",bty="l",col=gray(0.8)); text(pm(w$Stress),pm(w$BP),round(cor(w$Stress,w$BP),4),col="red",cex=2,font=2)
plot(1,1,pch=NA,axes=FALSE,xlab="",ylab="")
plot(w$Weight,w$Age,pch=19,las=1,xlab="Weight",ylab="Age",bty="l",col=gray(0.8)); text(pm(w$Weight),pm(w$Age),round(cor(w$Weight,w$Age),4),col="red",cex=2,font=2)
plot(w$BSA,   w$Age,pch=19,las=1,xlab="BSA",ylab="Age",bty="l",   col=gray(0.8)); text(pm(w$BSA),   pm(w$Age),round(cor(w$BSA,   w$Age),4),col="red",cex=2,font=2)
plot(w$Dur,   w$Age,pch=19,las=1,xlab="Dur",ylab="Age",bty="l",   col=gray(0.8)); text(pm(w$Dur),   pm(w$Age),round(cor(w$Dur,   w$Age),4),col="red",cex=2,font=2)
plot(w$Pulse, w$Age,pch=19,las=1,xlab="Pulse",ylab="Age",bty="l", col=gray(0.8)); text(pm(w$Pulse), pm(w$Age),round(cor(w$Pulse, w$Age),4),col="red",cex=2,font=2)
plot(w$Stress,w$Age,pch=19,las=1,xlab="Stress",ylab="Age",bty="l",col=gray(0.8)); text(pm(w$Stress),pm(w$Age),round(cor(w$Stress,w$Age),4),col="red",cex=2,font=2)
for (k in 1:2) plot(1,1,pch=NA,axes=FALSE,xlab="",ylab="")
plot(w$BSA,   w$Weight,pch=19,las=1,xlab="BSA",ylab="Weight",bty="l",    col=gray(0.8)); text(pm(w$BSA),   pm(w$Weight),round(cor(w$BSA,   w$Weight),4),col="red",cex=2,font=2)
plot(w$Dur,   w$Weight,pch=19,las=1,xlab="Dur",ylab="Weight",bty="l",    col=gray(0.8)); text(pm(w$Dur),   pm(w$Weight),round(cor(w$Dur,   w$Weight),4),col="red",cex=2,font=2)
plot(w$Pulse, w$Weight,pch=19,las=1,xlab="Pulse",ylab="Weight",bty="l",  col=gray(0.8)); text(pm(w$Pulse), pm(w$Weight),round(cor(w$Pulse, w$Weight),4),col="red",cex=2,font=2)
plot(w$Stress,w$Weight,pch=19,las=1,xlab="Stress",ylab="Weight",bty="l", col=gray(0.8)); text(pm(w$Stress),pm(w$Weight),round(cor(w$Stress,w$Weight),4),col="red",cex=2,font=2)
for (k in 1:3) plot(1,1,pch=NA,axes=FALSE,xlab="",ylab="")
plot(w$Dur,   w$BSA,pch=19,las=1,xlab="Dur",ylab="BSA",bty="l",    col=gray(0.8)); text(pm(w$Dur),   pm(w$BSA),round(cor(w$Dur,   w$BSA),4),col="red",cex=2,font=2)
plot(w$Pulse, w$BSA,pch=19,las=1,xlab="Pulse",ylab="BSA",bty="l",  col=gray(0.8)); text(pm(w$Pulse), pm(w$BSA),round(cor(w$Pulse, w$BSA),4),col="red",cex=2,font=2)
plot(w$Stress,w$BSA,pch=19,las=1,xlab="Stress",ylab="BSA",bty="l", col=gray(0.8)); text(pm(w$Stress),pm(w$BSA),round(cor(w$Stress,w$BSA),4),col="red",cex=2,font=2)
for (k in 1:4) plot(1,1,pch=NA,axes=FALSE,xlab="",ylab="")
plot(w$Pulse, w$Dur,pch=19,las=1,xlab="Pulse",ylab="Dur",bty="l",  col=gray(0.8)); text(pm(w$Pulse), pm(w$Dur),round(cor(w$Pulse, w$Dur),4),col="red",cex=2,font=2)
plot(w$Stress,w$Dur,pch=19,las=1,xlab="Stress",ylab="Dur",bty="l", col=gray(0.8)); text(pm(w$Stress),pm(w$Dur),round(cor(w$Stress,w$Dur),4),col="red",cex=2,font=2)
for (k in 1:5) plot(1,1,pch=NA,axes=FALSE,xlab="",ylab="")
plot(w$Stress,w$Pulse,pch=19,las=1,xlab="Stress",ylab="Pulse",bty="l", col=gray(0.8)); text(pm(w$Stress),pm(w$Pulse),round(cor(w$Stress,w$Pulse),4),col="red",cex=2,font=2)

# Modelos de regressão
summary(lm(formula = BP ~ Weight, data = w))
summary(lm(formula = BP ~ BSA, data = w))
summary(lm(formula = BP ~ Age + Weight + BSA + Dur + Pulse + Stress, data = w))

# Regressão múltipla com todas as variáveis independentes
model1 <- lm(BP~., data = w[,-1])

scale2 <- function(x) return((x-mean(x))/(sd(x)*sqrt(length(x)-1)))
# Matriz W
n <- dim(w)[1]
W <- matrix(c(scale2(w$Age),scale2(w$Weight),scale2(w$BSA),scale2(w$Dur),scale2(w$Pulse),scale2(w$Stress)),nrow=n)
# Matriz W'W (matriz de correlações)
WW <- t(W)%*%W
WW
# Calculando as correlações usando a função cor()
round(cor(w[,-(1:2)]),3)

# Inversa da matriz W'W
solve(t(W)%*%W)
# VIF obtidos da função vif do pacote car
car::vif(model1)

# Normal plot com envelope
library(glmtoolbox)
glmtoolbox::envelope(model1, rep=5000, col="red", type="internal")

# Modelo sem BSA
m3 <- lm(formula = BP ~ Age + Weight + Pulse + Stress, data = w)
