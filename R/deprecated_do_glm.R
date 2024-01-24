do_glm <- function(
    anomaly
){
  


# tar_load(anomaly)

# set up formats
anomaly$time = as.Date(paste(anomaly$year, anomaly$month, 1, sep='-'))
anomaly$month = as.factor(anomaly$month)
anomaly$mm = as.numeric(anomaly$month)

# Set up timevar for sinusoidal
timevar <- as.data.frame(names(table(anomaly$time)))
index <- 1:length(names(table(anomaly$time)))
timevar$time2 <- index / (length(index) / (length(index)/12))
names(timevar) <- c('time', 'timevar')
timevar$time <- as.Date(timevar$time)
anomaly <- merge(anomaly, timevar, by = "time")
anomaly$time <- as.numeric(anomaly$time)
anomaly$age_group <- as.factor(anomaly$age_group)
anomaly$sex <- as.factor(anomaly$sex)

anomaly$tmax_anomaly[anomaly$tmax_anomaly < 0] <- 0

# create vars for age_group, sex
anomaly$TmaxMales10_29 <- ifelse(
  anomaly$age_group == '10–29' & anomaly$sex == 'M', anomaly$tmax_anomaly, 0)
anomaly$TmaxMales30_54 <- ifelse(
  anomaly$age_group == '30–54' & anomaly$sex == 'M', anomaly$tmax_anomaly, 0)
anomaly$TmaxMales55plus <- ifelse(
  anomaly$age_group == '55+' & anomaly$sex == 'M', anomaly$tmax_anomaly, 0)

anomaly$TmaxFemales10_29 <- ifelse(
  anomaly$age_group == '10–29' & anomaly$sex == 'F', anomaly$tmax_anomaly, 0)
anomaly$TmaxFemales30_54 <- ifelse(
  anomaly$age_group == '30–54' & anomaly$sex == 'F', anomaly$tmax_anomaly, 0)
anomaly$TmaxFemales55plus <- ifelse(
  anomaly$age_group == '55+' & anomaly$sex == 'F', anomaly$tmax_anomaly, 0)

# > summary(gam_disagg)
# Approximate significance of smooth terms:
#                        edf Ref.df Chi.sq  p-value
# s(TmaxMales10_29)    1.162  1.297  0.131    0.774
# s(TmaxMales30_54)    1.000  1.001  0.028    0.867
# s(TmaxMales55plus)   1.001  1.002  2.010    0.157
# s(TmaxFemales10_29)  1.082  1.157  2.054    0.158
# s(TmaxFemales30_54)  1.001  1.002  0.252    0.617
# s(TmaxFemales55plus) 1.001  1.002  2.039    0.154
# s(month)             2.000  2.000 43.903 2.93e-10 ***
#   ---


## question to ivan: given that smoothed terms are not significant (except month),
# could this indicate that linear terms would suffice?

tmaxglm <- glm(
  deaths ~
    sin(timevar*2*pi) + cos(timevar*2*pi) # capture periodic patterns
    + TmaxMales10_29
    + TmaxMales30_54
    + TmaxMales55plus
    + TmaxFemales10_29
    + TmaxFemales30_54
    + TmaxFemales55plus
    + age_group
    + state
    + sex
    + age_group*sex*ns(year,3)
    + offset(log(pop)),
  data=anomaly,
  family=poisson)

# GS Medalla
# original S code from http://www.math.yorku.ca/Who/Faculty/Monette/S-news/0422.html
# The formula for pseudo-R^2 is taken from G. S. Maddalla,
# Limited-dependent and Qualitative Variables in Econometrics, Cambridge:Cambridge Univ. Press, 1983. page 40, equation 2.50.
Rsquared.glm.gsm <- function(o) {
  n <- length(o$residuals)
  ll <- logLik(o)[1]
  ll_0 <- logLik(update(o,~1))[1]
  R2 <- (1 - exp(((-2*ll) - (-2*ll_0))/n))/(1 - exp( - (-2*ll_0)/n))
  names(R2) <- 'pseudo.Rsquared'
  R2
}

Rsquared.glm.gsm(tmaxglm)
# good fit:
# pseudo.Rsquared
# 0.9814214

retunr(tmaxglm)
}