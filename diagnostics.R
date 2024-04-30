## QAIC

QAIC <- function(fit){
  c <- sum(residuals(fit, type = "pearson")^2)/fit$df.residual
  llik <- -0.5 * fit$deviance
  k <- length(coef(fit))
  qaic <- ((-2 * llik) / c) + (2 * k)
  return(qaic)
}

gam_global <- gam(
  deaths ~ 
    s(tmax_anomaly)+
    age_group * sex * ns(year,3), 
  data = anomaly,
  family = poisson
)

gam_inter <- gam(
  deaths ~ 
    s(TmaxMales10_29, k=3) + 
    s(TmaxMales30_54, k=3) + 
    s(TmaxMales55plus, k=3) +
    s(TmaxFemales10_29, k=3) + 
    s(TmaxFemales30_54, k=3) + 
    s(TmaxFemales55plus, k=3) +
    age_group * sex * ns(year,3) +
    gcc +
    s(month, k=3, fx=T, bs='cc') + 
    offset(log(pop)),
  data = anomaly,
  family = poisson
)


qaic_global <- QAIC(gam_global) 
qaic_inter <- QAIC(gam_inter)
delta_qaic <- qaic_global - qaic_inter 





## overdispersion test
sum(resid(gam_sex, type = "pearson")^2) / gam_sex$df.residual
# [1] 1.088172

# Function to compute dispersion statistic
compute_dispersion <- function(
    model,
    data
){
  pearson_resid <- residuals(
    model,
    type = "pearson")
  return(sum(pearson_resid^2) / model$df.residual)
}

# Bootstrapping the dispersion statistic
set.seed(123)
bootstrap_dispersion <- replicate(
  1000,
  {
    # Resample
    resampled_data <- anomaly[sample(nrow(anomaly), replace = TRUE), ]
    # Refit
    model_boot <- update(gam_sex, data = resampled_data)
    # Compute disp stats for the boot model
    compute_dispersion(model_boot, resampled_data)
  }
)

# Compare
observed_dispersion <- compute_dispersion(gam_sex, data)
p_value <- mean(bootstrap_dispersion >= observed_dispersion)
p_value
# [1] 0.2
