attrdl_gam <- function(x, cases, model=NULL, exposure_var, type="af",
                       tot=TRUE, range=NULL, sim=FALSE, nsim=5000) {
  type <- match.arg(type, c("an", "af"))
  
  if(!is.null(range)) x[x < range[1] | x > range[2]] <- mean(x, na.rm = TRUE)  
  
  if(NCOL(cases) > 1L) {
    cases <- rowMeans(cases)
  }
  
  coef <- NULL
  se <- NULL
  if(!is.null(model)) {
    coef <- coef(model)[exposure_var]  # Extract only the coefficient for the exposure variable
    vcov <- vcov(model)[exposure_var, exposure_var]  # Extract only the variance for the exposure variable
    se <- sqrt(sum(diag(vcov)))  # Calculate the standard error
  }
  
  af <- exp(-sum(as.vector(x * coef)))
  an <- af * sum(cases)
  
  if(tot) {
    isna <- is.na(an)
    an <- sum(an, na.rm = TRUE)
    af <- an / sum(cases[!isna], na.rm = TRUE)
  }
  
  # Calculate the lower and upper confidence intervals
  lci <- if(type == "an") an - (1.96 * se * an) else af - (1.96 * se * af)
  uci <- if(type == "an") an + (1.96 * se * an) else af + (1.96 * se * af)
  
  if(!tot && sim) {
    sim <- FALSE
    warning("simulation samples only returned for tot=T")
  }
  
  if(sim) {
    k <- length(coef)
    eigen <- eigen(vcov)
    X <- matrix(rnorm(length(coef) * nsim), nsim)
    coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values), k) %*% t(X)
    ansim <- sum((1 - exp(-sum(as.vector(x * coefsim)))) * cases, na.rm = TRUE)
    afsim <- ansim / sum(cases[!isna], na.rm = TRUE)
  }
  
  res <- if(sim) {
    if(type == "an") ansim else afsim
  } else {
    if(type == "an") an else af    
  }
  
  cat("Estimate:", res, "(95% CI", lci, "to", uci, ")\n")
}

