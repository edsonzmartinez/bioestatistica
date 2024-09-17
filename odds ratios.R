#################
# Aula 1        #
#################

# 1. Calculando um odds ratio usando o pacote epitools

library(epitools)

# Criando a tabela
exposure <- c("Exposed", "Unexposed")
outcome  <- c("Diseased", "Controls")
data <- matrix(c(68, 44, 32, 56), nrow=2, ncol=2, byrow=TRUE)
dimnames(data) <- list("Exposure"=exposure , "Outcome"=outcome)

# Exibindo os dados
data

# Calculando o odds ratio
oddsratio(data)

########################################################

# 2. Calculando um odds ratio usando o pacote epitools


# Criando as tabelas
data <- array(c(64,16,32,8,4,16,12,48),
              dim = c(2, 2, 2),
              dimnames = list( exposure = c("Exposed", "Unexposed"),
                               outcome  = c("Diseased", "Controls"),
                               smoking  = c("Smokers", "Non-smokers")))

# Exibindo os dados
data 

# Teste de Mantel-Haenszel 
mantelhaen.test(data)

########################################################
