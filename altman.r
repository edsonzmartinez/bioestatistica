#################
# Aula 3        #
#################

# Regressão linear simples

# Lendo os dados do livro de Douglas Altman
urlfile <- "https://raw.githubusercontent.com/edsonzmartinez/cursoR/main/altman.csv"
altman  <- read.csv2(urlfile)

# Ajuste do modelo e tabela de ANOVA
m1 <- lm(CTP~altura,data=altman)
summary(m1)
anova(m1)

# Quadrado médio dos erros (QME)
qme <- anova(m1)[[3]][2]

# Gráfico com a reta de regressão
plot(altman$altura,altman$CTP,pch=19,bty="l")
abline(m1,col="red",lwd=2)

# Standardized residuals
stdres <- rstandard(m1)

# Criando um data frame com os dados, preditos e resíduos
out <- data.frame(altura=altman$altura,y=altman$CTP,pred=predict(m1),res=m1$res,stdres=stdres)
out

# Gráficos
# Resíduos versus preditos
plot(m1, pch=19, which=1)
# Resíduos padronizados versus preditos
plot(predict(m1), stdres, pch=19, ylim=c(-3,3), xlab="Fitted values", ylab="Standardized residuals")
abline(c(0,0), lty=1)
abline(c(-3,0),lty=2)
abline(c(3,0), lty=2)
# Histograma dos resíduos
hist(m1$res, xlab="Resíduos", main = "Resíduos")
# Normal probability plot (resíduos não padronizados)
qqnorm(m1$res, pch=19, abline = c(0, 1), frame = FALSE)
qqline(m1$res, col = "steelblue", lwd = 2)
# Normal probability plot (resíduos padronizados, mais adequado)
plot(m1, pch=19, which=2)
# Testes de normalidade (cuidado!)
# Shapiro-Wilk's W test
shapiro.test(stdres) 
# Kolmogorov-Smirnov test (KS)
ks.test(stdres, "pnorm")
# Normal probability plot com envelope
# Pacote glmtoolbox
library(glmtoolbox)
glmtoolbox::envelope(m1, rep=5000, col="red", type="internal")

# Goldfeld-Quandt test (cuidado!)
gq <- altman[order(altman$altura),]
point1  <- floor((0.5 - 0.2/2) * n)
point2  <- ceiling((0.5 + 0.2/2) * n + 0.01)
m.gq1   <- lm(CTP[1:point1]~altura[1:point1],data=gq)
m.gq2   <- lm(CTP[point2:n]~altura[point2:n],data=gq)
aov.gq1 <- anova(m.gq1)
aov.gq2 <- anova(m.gq2)
F.gq    <- aov.gq2[2,3]/aov.gq1[2,3]
paste("Goldfeld-Quandt test, GQ = ",round(F.gq,4),", df1 = ",aov.gq2[2,1],", df2 = ",aov.gq1[2,1])
# Pacote lmtest
library(lmtest)
lmtest::gqtest(m1, data=altman, order.by =~altura, fraction=0.2, alternative="two.sided")

# Breusch-Pagan test (pacote lmtest)
lmtest::bptest(m1)
lmtest::bptest(m1,studentize = FALSE)

# Leverage
h <- hatvalues(m1)

# Gráfico de leverage
n <- length(h)
flag1 <- 3*sum(h)/n
flag2 <- 2*sum(h)/n
plot(hatvalues(m1), type = 'h', las=1, col="red")
lines(c(0,n+1),c(flag1,flag1),lty="dashed")
lines(c(0,n+1),c(flag2,flag2),lty="dashed")
paste("Flagged observations:")
which(as.numeric(h)>flag1)

# Gráfico identificando observações 2 e 20
plot(altman$altura,altman$CTP,pch=19,bty="l",las=1,xlab="Altura (cm)",ylab="CTP",ylim=c(0,10))
abline(m1,col="red",lwd=2)
text(altman$altura[2],altman$CTP[2],"2",pos=3,col="red")
text(altman$altura[20],altman$CTP[20],"20",pos=3,col="red")

# Ajuste do modelo, sem a observação 2
m2 <- lm(CTP[-2]~altura[-2],data=altman)
summary(m2)
anova(m2)

# Ajuste do modelo, sem a observação 20
m3 <- lm(CTP[-20]~altura[-20],data=altman)
summary(m3)
anova(m3)

# Gráfico, sem a observação 2
plot(altman$altura,altman$CTP,pch=19,bty="l",las=1,xlab="Altura (cm)",ylab="CTP",ylim=c(0,10))
abline(m1,col="red",lwd=2)
abline(m2,col="blue",lwd=2,lty="dashed")
text(altman$altura[2],altman$CTP[2],"2",pos=3,col="red")
legend(170,3,col=c("red","blue"),lty=c(1,2),lwd=2,legend=c("Toda a amostra","Sem a observação 2"))

# Gráfico, sem a observação 20
plot(altman$altura,altman$CTP,pch=19,bty="l",las=1,xlab="Altura (cm)",ylab="CTP",ylim=c(0,10))
abline(m1,col="red",lwd=2)
abline(m3,col="blue",lwd=2,lty="dashed")
text(altman$altura[20],altman$CTP[20],"20",pos=3,col="red")
legend(170,3,col=c("red","blue"),lty=c(1,2),lwd=2,legend=c("Toda a amostra","Sem a observação 20"))

# Distância de Cook
# As distâncias de Cook são dadas por:
cook <- (m1$res^2)/(2*qme)*h/(1-h)^2
# Ou, usando a função cooks.distance:
cooks.distance(m1)
plot(m1,which=4)
abline(h=4/(n-sum(h)-1),lty=2)
# Criando um data frame com os dados, preditos e resíduos
out <- data.frame(altura=altman$altura,y=altman$CTP,pred=predict(m1),res=m1$res,stdres=stdres,h=h,Cook=cook)
out

# Diagnósticos de resíduos - resumos
par(mfrow = c(2, 2))
plot(m1)


# Quarteto de Anscombe
# Ver código R e dados em:
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/anscombe.html


# Banda de confiança para a reta de regressão:
newx <- seq(min(altman$altura),max(altman$altura),by = 0.05)
conf_interval <- predict(m1, newdata=data.frame(altura=newx), interval="confidence", level = 0.95)
plot(altman$altura,altman$CTP,pch=19,bty="l",las=1,xlab="Altura (cm)",ylab="CTP",ylim=c(0,10))
abline(m1, col="red", lwd=2)
matlines(newx, conf_interval[,2:3], col = "blue", lty=2)

# Banda de predição para a reta de regressão:
newx <- seq(min(altman$altura),max(altman$altura),by = 0.05)
conf_interval <- predict(m1, newdata=data.frame(altura=newx), interval="predict", level = 0.95)
plot(altman$altura,altman$CTP,pch=19,bty="l",las=1,xlab="Altura (cm)",ylab="CTP",ylim=c(0,10))
abline(m1, col="red", lwd=2)
matlines(newx, conf_interval[,2:3], col = "blue", lty=2)

