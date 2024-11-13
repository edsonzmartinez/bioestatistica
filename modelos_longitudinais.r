#################
# Aula 8        #
#################

# Modelos longitudinais
library(lme4)
library(jtools)
library(lattice)
library(ggResidpanel)
library(performance)
####################################################################
# Lendo a base de dados
urlfile = "https://raw.githubusercontent.com/edsonzmartinez/basesdedados/main/Potthoff.csv"
w <- read.csv(urlfile,sep=";")
head(w,20)
# Boxplots
boxplot(w$y[w$Sexo==0]~w$idade[w$Sexo==0],xlab="Idade",ylab="Medidas",main="Girls")
boxplot(w$y[w$Sexo==1]~w$idade[w$Sexo==1],xlab="Idade",ylab="Medidas",main="Boys")
# Gráfico de perfis
plot(0,0,col=NA,ylim=c(0,35),xlim=c(0.8,4),xlab="Idade (anos)",ylab="Medidas",axes=FALSE)
axis(2,las=1)
axis(1,at=1:4,labels=c(8,10,12,14))
for (k in 1:27) {
 lines(1:4,w$y[w$num==k],col=rainbow(27)[k],type="l") }
# Gráfico de perfis - por sexo
corsexo <- c(rep("red",11),rep("blue",16))
plot(0,0,col=NA,ylim=c(0,35),xlim=c(0.8,4),xlab="Idade (anos)",ylab="Medidas",axes=FALSE)
axis(2,las=1)
axis(1,at=1:4,labels=c(8,10,12,14))
for (k in 1:27) {
 lines(1:4,w$y[w$num==k],col=corsexo[k],type="l") }
legend(3,10,col=c("red","blue"),legend=c("girls","boys"),lwd=1,bty = "n")
#
# Gráfico - lattice
lattice::xyplot(y ~ idade | num, data = w, strip = FALSE,
aspect = "xy", pch = 16, col.line = "black",
grid = TRUE, type = c("p", "l", "r"),
xlab = "Idade (anos)", ylab = "Medidas")
# Modelo de efeitos mistos
model1 <- lmer(y ~ Sexo + idade + (1|num), w)
summary(model1)
# Efeitos aleatórios
ranef(model1)$num
lattice::dotplot(ranef(model1, condVar=TRUE), strip = FALSE)
# Coeficientes
coefficients(model1)$num
# Nakagawa’s R2 for mixed models
# Conditional R2: takes both the fixed and random effects into account
# Marginal R2: considers only the variance of the fixed effects
performance::r2_nakagawa(model1)
# Gráfico de perfis
# Indivíduo k
id <- "Indivíduo 1"
k  <- 1
plot(c(8,10,12,14),w$y[w$num==k],col="red",pch=19,ylim=c(0,35),xlim=c(7.8,14),xlab="Idade (anos)",ylab="Medidas",axes=FALSE,main=id)
axis(2,las=1)
axis(1,at=c(8,10,12,14),labels=c(8,10,12,14))
a <- coefficients(model1)$num[k,1]
b <- coefficients(model1)$num[k,3]
linha <- function(x,a,b) linha <- a+b*x
lines(c(8,14),c(linha(8,a,b),linha(14,a,b)),col="blue")
# Preditos
predict(model1)
# Breusch-Pagan test (1979) for (non-)constant error variance
performance::check_heteroscedasticity(model1)
plot(performance::check_heteroscedasticity(model1))
# Distribuição dos dados e dos resíduos
plot(performance::check_distribution(model1))
# Gráfico de resíduos
plot(model1, type=c("p","smooth"), col.line=1, pch=18)
# Scale-location plot
plot(model1,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)
# QQ plot
lattice::qqmath(model1)
# Leverage
plot(model1, rstudent(.) ~ hatvalues(.))
# Pacote ggResidpanel
ggResidpanel::resid_panel(model1, smoother = TRUE, qqbands = TRUE)
