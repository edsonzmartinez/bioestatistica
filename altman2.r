#################
# Aula 4        #
#################

# Regressão linear simples

# Modelo com uma variável independente binária (sexo)

# Lendo os dados do livro de Douglas Altman
urlfile <- "https://raw.githubusercontent.com/edsonzmartinez/cursoR/main/altman.csv"
altman  <- read.csv2(urlfile)

# Criando uma variável dummy
altman$sexo.d <- ifelse(altman$sexo=="M",0,1)

# Ajuste do modelo e tabela de ANOVA
m1 <- lm(CTP~sexo.d,data=altman)
summary(m1)
anova(m1)

# Standardized residuals
stdres <- rstandard(m1)

# Criando um data frame com os dados, preditos e resíduos
out <- data.frame(sexo=altman$sexo,y=altman$CTP,pred=predict(m1),res=m1$res,stdres=stdres)
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

# Leverage
# Notar que agora os valores de leverage são todos iguais
h <- hatvalues(m1)

# Gráfico de leverage
n <- length(h)
flag1 <- 3*sum(h)/n
flag2 <- 2*sum(h)/n
# Os valores de leverage são todos iguais
plot(hatvalues(m1), type = 'h', las=1, col="red")
lines(c(0,n+1),c(flag1,flag1),lty="dashed")
lines(c(0,n+1),c(flag2,flag2),lty="dashed")
paste("Flagged observations:")
which(as.numeric(h)>flag1)

# Quadrado médio dos erros (QME)
qme <- anova(m1)[[3]][2]
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

# Ajuste do modelo, sem a observação 3
m2 <- lm(CTP[-3]~sexo.d[-3],data=altman)
summary(m2)
anova(m2)

par(mfrow = c(2, 2))
plot(m1,which=1,las=1)
plot(m1,which=2,pch=19,col="red")
plot(m1,which=3)
plot(m1,which=4)

# Comparação com o teste t de Student
t.test(altman$CTP~altman$sexo.d,var.equal=TRUE)
summary(m1)

