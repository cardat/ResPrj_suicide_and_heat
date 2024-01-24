#  deprecated - not using

# attrdl_gam <- function(x, cases, model=NULL, exposure_var, type="af",
#                        tot=TRUE, range=NULL, sim=FALSE, nsim=5000, conf.level=0.95) {
#   options(scipen = 999)
#   type <- match.arg(type, c("an", "af"))
#   
#   if(!is.null(range)) x[x < range[1] | x > range[2]] <- mean(x, na.rm = TRUE)
#   
#   if(NCOL(cases) > 1L) {
#     cases <- rowMeans(cases)
#   }
#   
#   coef <- NULL
#   se <- NULL
#   if(!is.null(model)) {
#     coef <- coef(model)[exposure_var]  
#     vcov <- vcov(model)[exposure_var, exposure_var]  
#     se <- sqrt(sum(diag(vcov)))  
#   }
#   
#   af <- 1- exp(-sum(as.vector(x * coef)))
#   an <- af * sum(cases)
#   
#   if(tot) {
#     isna <- is.na(an)
#     an <- sum(an, na.rm = TRUE)
#     af <- an / sum(cases[!isna], na.rm = TRUE)
#   }
#   
# 
#   res <- if(type == "an") an else af
#   
#   if(sim) {
#     # Set the number of bootstrap samples
#     n_bootstrap <- 100000
#     # Initialise a vector to store the bootstrap AN estimates
#     bootstrap_an <- numeric(n_bootstrap)
#         # Perform bootstrapping to estimate the CI
#     for (i in 1:n_bootstrap) {
#       # Sample with replacement from the data
#       sampled_indices <- sample(length(cases), replace = TRUE)
#       sampled_x <- x[sampled_indices]
#       sampled_cases <- cases[sampled_indices]
#       
#       # Calculate the AN for the bootstrapped sample
#       af <- exp(-sum(as.vector(sampled_x * coef)))
#       bootstrap_an[i] <- af * sum(sampled_cases)
#     }
#     
#     # Calculate the 95% CI
#     lower_ci <- quantile(bootstrap_an, probs = 0.025)
#     upper_ci <- quantile(bootstrap_an, probs = 0.975)
#     
#     # Print the 95% CI
#     # Print the AN estimate
#     cat("Estimated Attributable Deaths (AN):", an," (95% CI ", lower_ci, "-", upper_ci,")", "\n")
#   } else {
#     cat("Estimated Attributable Fraction (AF):", af, "\n")
#   }
# }
