#################
# Aula 7        #
#################

# Análise de sobrevivência

# Exemplo - ensaio aleatorizado com ratos

####################################################################
# For example, consider the results of a small randomized trial    #
# on rats. Suppose you randomize 40 rats that have been exposed to # 
# a carcinogen into two treatment groups (Drug X and Placebo). The # 
# event of interest is death from cancer induced by the carcinogen.# 
# The response is the time from randomization to death. Four rats  # 
# died of other causes; their survival times are regarded as       #
# censored observations. Interest lies in whether the survival     #
# distributions differ between the two treatments.                 #
####################################################################
# Source: SAS Institute Inc. 2012. SAS/STAT® 12.1                  #
# User’s Guide: Survival Analysis. Cary, NC: SAS Institute Inc.    #
####################################################################
# Days      = Survival time in days from treatment to death        #
# Status    = Censoring indicator variable:                        #
#             0 if censored and 1 if not censored                  #
# Treatment = Treatment indicator variable:                        #
#             0 if placebo and 1 if drug X                         #
# Sex       = Sex
####################################################################
# Lendo a base de dados
urlfile = "https://raw.githubusercontent.com/edsonzmartinez/basesdedados/main/survival_trial_rats.csv"
w <- read.csv2(urlfile)
# Pacotes 
library(survival)
library(ggsurvfit)
# Kaplan-Meier
km_fit <- survfit(Surv(Days,Status) ~ Treatment, data=w)
km_fit
# Comparação entre as curvas
# rho = 0 especifica o teste de log-rank
survdiff(Surv(Days,Status) ~ Treatment, rho = 0, data=w)
# Usando o pacote ggsurvfit
survfit2(Surv(Days,Status) ~ Treatment, data=w) %>% 
  ggsurvfit(linewidth = 1.5) +
  labs(x = "Time in days",y = "S(t)") + 
  add_censor_mark() + 
  add_pvalue(caption = "Log-rank {p.value}", rho=0) + 
  scale_y_continuous(limits = c(0,1)) + add_risktable() 


