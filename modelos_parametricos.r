 library(survival)
 library(bshazard)        # Para estimar a função de risco h(t) com suavização não paramétrica
 library(AdequacyModel)   # Para gerar o TTT plot com dados sem censuras
 library(EstimationTools) # Para gerar o TTT plot com dados censurados
 
 # Modificando a função TTT do pacote AdequacyModel, para permitir retonar
 # os valores de i/n e Trn(i)
 
 TTT2 <- function (x, lwd = 2, lty = 2, col = "black", grid = TRUE, ...) 
 {   n <- length(x)
     y <- order(x)
     sortx <- x[y]
     Trn <- rep(NA, times = n)
     r <- rep(NA, times = n)
     Trn[1] <- n * sortx[1]
     r[1] <- 1/n
     for (i in 2:n) {
         Trn[i] <- Trn[i - 1] + (n - i + 1) * (sortx[i] - sortx[i - 1])
         r[i] <- i/n
     }
     plot(r, Trn/Trn[n], xlab = "i/n", ylab = "T(i/n)", xlim = c(0, 
         1), ylim = c(0, 1), main = "", type = "l", lwd = lwd, 
         lty = 1, col = col, ...)
     lines(c(0, 1), c(0, 1), lty = lty, lwd = 1, ...)
     if (grid == "TRUE") 
         grid()
	 w <- data.frame(i.n = r, G.i.n = Trn/Trn[n])
	 return(w)
 }
 
 # Modelo exponencial
 
 # Dados
 x <- c(1, 2, 2, 3, 5, 8, 9, 10, 10, 10, 11, 12, 17, 18, 20,
 23, 24, 26, 27, 27, 28, 29, 31, 35, 36, 38, 44, 46, 48,
 50, 55, 62, 66, 73, 74, 75, 83, 88, 89,157)
 d <- rep(1,length(x)) 
 
 sur <- Surv(time = x, d)
 fit <- survfit(sur ~ 1)                        # Kaplan Meier
 reg <- survreg(sur ~ 1, dist = "exponential")  # Exponential model
 lambda <- 1 / exp(coef(reg))
 t_vals <- seq(0, max(x), length.out = 100)
 S_exp <- exp(-lambda * t_vals)
 exp_curve <- data.frame(time = t_vals, surv = S_exp)
 
 # Gráfico de Kaplan-Meier com a curva ajustada
 survfit2(sur ~ 1) %>% 
 ggsurvfit(linewidth = 1.5) +
  labs(x = "Tempo (dias)",
       y = "S(t) estimada") + 
  add_censor_mark() +   
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(data = exp_curve, aes(x = time, y = surv), 
         color = "red", size = 1) +
  add_risktable()

 # Gráfico de Kaplan-Meier com a curva ajustada e uma legenda
 ggsurvfit(fit, type = "survival") +
   geom_step(aes(x = time, y = estimate, color = "Kaplan-Meier"), linewidth = 1) +
   geom_line(
     data = exp_curve,
     aes(x = time, y = surv, color = "Exponencial"),
     linewidth = 1) +
   scale_color_manual(values = c("Kaplan-Meier" = "black", "Exponencial" = "red")) +
   labs(x = "Tempo (dias)", y = "S(t) estimada", color = NULL) +
   scale_y_continuous(limits = c(0, 1)) +
   add_censor_mark() +
   add_risktable() +
   theme_bw(base_size = 13) + 
   theme(
     legend.position = c(0.8, 0.85),  # posição dentro do gráfico
     legend.background = element_rect(fill = "white", color = "gray70", linewidth = 0.3),
     legend.key = element_blank(),
     legend.text = element_text(size = 11)
   )
   
 # Estimativas, média, AIC
 summary(reg)
 message("Média = ",round(exp(coef(reg)),1))
 message("AIC = ",round(AIC(reg),2))
 
 # Função risco estimada (pacote bshazard)
 fith <- bshazard(sur ~ 1, nbin = 100)
 plot(fith, ylim = c(0,.1), xlab = "Tempo (dias)", ylab = "h(t) estimada", las = 1)
 # Uma alternativa ao pacote bshazard é o pacote kernhaz
 fith <- khazard(times = x, delta = rep(1,length(x)))
 plot(fith)
 # No pacote kernhaz, o kernel de Epanechnikov é usado como default

 # TTT plot
 # A função TTT do pacote AdequacyModel só funciona para dados sem censura
 AdequacyModel::TTT(x, lwd = 2, col = "blue")


 # Modelo Weibull

 # Dados
 x <- c(1, 1, 2, 6, 10, 11, 15, 15, 16, 16, 17, 18, 19, 21, 21, 22, 24, 26, 28, 
 31, 32, 32, 37, 38, 39, 43, 44, 45, 46, 48, 52, 52, 52, 57, 60, 60, 60, 60, 60, 
 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60)
 # Vector of censoring indicator. 0 - censored, 1 - uncensored
 d <- c(0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0,
 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
 
 sur <- Surv(time = x, d)
 fit <- survfit(sur ~ 1)                      # Kaplan Meier
 reg <- survreg(sur ~ 1, dist = "weibull")    # Weibull model
 gamma.est <- 1 / reg$scale
 alpha.est <- exp(reg$coefficients)
 t_vals <- seq(0, max(x), length.out = 100)
 S_weib <- exp(- (t_vals / alpha.est)^gamma.est)
 weib_curve <- data.frame(time = t_vals, surv = S_weib)
 
 # Gráfico de Kaplan-Meier com a curva ajustada
 survfit2(sur ~ 1) %>% 
 ggsurvfit(linewidth = 1.5) +
  labs(x = "Tempo (dias)",
       y = "S(t) estimada") + 
  add_censor_mark() +   
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(data = weib_curve, aes(x = time, y = surv), 
         color = "red", size = 1) +
  add_risktable() 
 
 # Estimativas, média, AIC
 summary(reg)
 message("Alpha estimado = ",round(alpha.est,2))
 message("Gamma estimado = ",round(gamma.est,2))
 message("Média = ",round(alpha.est*gamma(1+1/gamma.est),1))
 message("AIC = ",round(AIC(reg),2))
 
 # Função risco estimada
 fit <- bshazard(sur ~ 1, nbin = 100)
 plot(fit, ylim = c(0,.06),xlab = "Tempo (dias)", ylab = "h(t) estimada", las = 1)

 # TTT plot
 ttt <- EstimationTools::TTTE_Analytical(Surv(x,d) ~ 1, method = "censored", scaled = FALSE)



 # Modelo de regressão exponencial
 
 # Dados
 
 # Tratamento A: 
 t1 <- c(1, 2, 2, 2, 3, 3, 3, 3, 4, 5, 5, 6, 7, 7, 8, 9, 
 12, 13, 14, 18, 18, 20, 20, 22, 26, 28, 30, 41, 43, 47, 
 48, 54, 56, 60, 60, 60, 60, 60, 60, 60) 
 d1 <- c(0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 
   0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 
   0, 0, 0, 0, 0, 0)
 # Tratamento B: 
 t2 <- c(1, 1, 2, 3, 3, 3, 4, 5, 6, 8, 8, 10, 11, 12, 21, 
  25, 26, 32, 33, 33, 39, 39, 46, 43, 53, 56, 58, 60, 60, 
  60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60)
 d2 <- c(1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 
   1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
   0, 0, 0, 0, 0, 0)
 #
 
 t <- c(t1,t2)
 d <- c(d1,d2)
 Treatment <- c(rep("A",length(t1)),rep("B",length(t2)))
 
 survfit2(Surv(t,d) ~ Treatment) %>% 
   ggsurvfit(linewidth = 1.5) +
   labs(x = "Time in months",y = "S(t)") + 
   add_censor_mark() + 
   add_pvalue(caption = "Log-rank {p.value}", rho=0) + 
   scale_y_continuous(limits = c(0,1)) + add_risktable() 

 sur <- Surv(time = t, d)
 reg <- survreg(sur ~ Treatment, dist = "exponential")  # Exponential regression
 summary(reg)
 AIC(reg)
 
 # TTT plot
 ttt <- EstimationTools::TTTE_Analytical(Surv(t,d) ~ Treatment, method = "censored", scaled = FALSE)


 t <- c(3, 5, 6, 7, 8, 9, 10, 10, 12, 15, 15, 18, 19, 20, 22, 25, 28, 30, 40, 45)
 # vector of censoring indicator. 0 - censored, 1 - uncensored
 d <- c(1, 1, 1, 1, 1, 1,  1,  0,  1,  1,  0,  1,  1,  1,  1,  1,  1,  1,  1,  0)
 # Kaplan-Meier
 sur <- Surv(time = t, d)
 km_fit <- survfit(sur ~ 1)   
 km_fit               # Kaplan Meier
 summary(km_fit)
 # Gráfico de Kaplan-Meier
 plot(km_fit, las=1, bty="l", col="red", lwd=1, xlab="Tempo (semanas)", ylab="S(t) estimada")

 # TTT plot
 ttt <- EstimationTools::TTTE_Analytical(Surv(t,d) ~ 1, method = "censored", scaled = FALSE)

 # Função risco estimada
 fit <- bshazard(sur ~ 1, nbin = 100)
 plot(fit, ylim = c(0,0.4), xlab = "Tempo (dias)", ylab = "h(t) estimada", las = 1)

