# Examples of Survival Data

# Recorded remission times given in months from bladder cancer patients, reported by Lee and Wang (2003).
# Lee, E. T. & Wang, J. Statistical Methods for Survival Data Analysis Vol. 476 (Wiley, 2003).
t1 <- c(
0.08,	1.35,	2.46,	3.25,	3.88,	4.98,	5.62,	7.26,	8.26,	10.34,	12.63,	17.12,	25.82,
0.2,	1.4,	2.54,	3.31,	4.18,	5.06,	5.71,	7.28,	8.37,	10.66,	13.11,	17.14,	26.31,
0.4,	1.46,	2.62,	3.36,	4.23,	5.09,	5.85,	7.32,	8.53,	10.75,	13.29,	17.36,	32.15,
0.5,	1.76,	2.64,	3.36,	4.26,	5.17,	6.25,	7.39,	8.65,	11.25,	13.8,	18.1,	34.26,
0.51,	2.02,	2.69,	3.48,	4.33,	5.32,	6.54,	7.59,	8.66,	11.64,	14.24,	19.13,	36.66,
0.81,	2.02,	2.69,	3.52,	4.34,	5.32,	6.76,	7.62,	9.02,	11.79,	14.76,	20.28,	43.01,
0.9,	2.07,	2.75,	3.57,	4.4,	5.34,	6.93,	7.63,	9.22,	11.98,	14.77,	21.73,	46.12,
1.05,	2.09,	2.83,	3.64,	4.5,	5.41,	6.94,	7.66,	9.47,	12.02,	14.83,	22.69,	79.05,
1.19,	2.23,	2.87,	3.7,	4.51,	5.41,	6.97,	7.87,	9.74,	12.03,	15.96,	23.63,	
1.26,	2.26,	3.02,	3.82,	4.87,	5.49,	7.09,	7.93,	10.06,	12.07,	16.62,	25.74)

# Survival times given in days of guinea pigs infected with virulent tubercle bacilli, summarized by Bjerkedal (1960).
# Bjerkedal, T. Acquisition of resistance in guinea pies infected with different doses of virulent tubercle bacilli. 
# Am. J. Hyg. 72(1), 130–48 (1960).
t2 <- c(
0.1,	0.74,	1.00,	1.08,	1.16,	1.3,	1.53,	1.71,	1.97,	2.3,	2.54,	3.47,
0.33,	0.77,	1.02,	1.08,	1.2,	1.34,	1.59,	1.72,	2.02,	2.31,	2.54,	3.61,
0.44,	0.92,	1.05,	1.09,	1.21,	1.36,	1.6,	1.76,	2.13,	2.4,	2.78,	4.02,
0.56,	0.93,	1.07,	1.12,	1.22,	1.39,	1.63,	1.83,	2.15,	2.45,	2.93,	4.32,
0.59,	0.96,	1.07,	1.13,	1.22,	1.44,	1.63,	1.95,	2.16,	2.51,	3.27,	4.58,
0.72,	1.00,	1.08,	1.15,	1.24,	1.46,	1.68,	1.96,	2.22,	2.53,	3.42,	5.55)

# Death times (in weeks) of patients with cancer of the tongue
###########################################################################
# A study was conducted on the effects of ploidy on the prognosis of
# patients with cancers of the mouth. Patients were selected who had
# a paraffin-embedded sample of the cancerous tissue taken at the time
# of surgery. Follow-up survival data was obtained on each patient. The
# tissue samples were examined using a flow cytometer to determine
# if the tumor had an aneuploid (abnormal) or diploid (normal) DNA
# profile using a technique discussed in Sickle-Santanello et al. (1988).
#
# Klein, J. P., & Moeschberger, M. L. (2006). Survival Analysis: Techniques 
# for Censored and Truncated Data (p. 12). New York, NY: Springer New York.
#
# Aneuploid Tumors, death times: 
t1 <- c(1, 3, 3, 4, 10, 13, 13, 16, 16, 24, 26, 27, 28, 30, 30, 32, 41, 51, 65, 67, 70,
72, 73, 77, 91, 93, 96, 100, 104, 157, 167, 61, 74, 79, 80, 81, 87, 87, 88, 89, 93, 97, 
101, 104, 108, 109, 120, 131, 150, 231, 240, 400)
d1 <- c(rep(1,31),rep(0,21))
# Diploid Tumors, death times: 
t2 <- c(1, 3, 4, 5, 5,8, 12, 13, 18, 23, 26, 27, 30, 42, 56, 62, 69, 104, 104, 112, 129, 
181, 8, 67, 76, 104, 176, 231)
d2 <- c(rep(1,22),rep(0,6))
#
t <- c(t1,t2)
d <- c(d1,d2)
g <- c(rep(0,length(t1)),rep(1,length(t2)))
#
survfit2(Surv(t,d) ~ g) %>% 
   ggsurvfit(linewidth = 1.5) +
   labs(x = "Tempo (semanas)",
        y = "S(t) estimada") + 
   add_censor_mark() + 
   add_pvalue(caption = 
        "Log-rank {p.value}", rho=0) + 
   scale_y_continuous(limits = c(0,1)) +    
   add_risktable()


