# use `SimDesign::SimFunctions()` to create the template

library(SimDesign)

Design <- createDesign(
  treatment_effect     = c(0.0, 0.2, 0.5),
  pre_post_correlation = c(0.0, 0.5, 0.7)
)

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL) {
  
  Attach(condition)
  
  covariance_matrix <- matrix(c(
    1,                    pre_post_correlation,
    pre_post_correlation,                    1
  ), nrow = 2, ncol = 2, byrow = TRUE)
  
  treatment_control      <- MASS::mvrnorm(n = 50, mu = c(0, 0),                Sigma = covariance_matrix)
  treatment_experimental <- MASS::mvrnorm(n = 50, mu = c(0, treatment_effect), Sigma = covariance_matrix)
  
  dat <- rbind(
    data.frame(
      "pre"       = treatment_control[,1],
      "post"      = treatment_control[,2],
      "treatment" = "control"
    ),
    data.frame(
      "pre"       = treatment_experimental[,1],
      "post"      = treatment_experimental[,2],
      "treatment" = "experimental"
    )
  )
  dat$treatment <- factor(dat$treatment, levels = c("control", "experimental"))
  
  return(dat)
}

Analyse <- function(condition, dat, fixed_objects = NULL) {
  
  ANCOVA       <- lm(post ~ pre + treatment, data = dat)
  change_score <- lm(post ~ offset(pre) + treatment, data = dat)
  post_score   <- lm(post ~ treatment, data = dat)
  
  ANCOVA.summary       <- summary(ANCOVA)$coefficients
  change_score.summary <- summary(change_score)$coefficients
  post_score.summary   <- summary(post_score)$coefficients
  
  ret <- nc(
    ANCOVA.est    = ANCOVA.summary["treatmentexperimental", "Estimate"],
    ANCOVA.est_se = ANCOVA.summary["treatmentexperimental", "Std. Error"],
    ANCOVA.pval   = ANCOVA.summary["treatmentexperimental", "Pr(>|t|)"],
    
    change_score.est    = change_score.summary["treatmentexperimental", "Estimate"],
    change_score.est_se = change_score.summary["treatmentexperimental", "Std. Error"],
    change_score.pval   = change_score.summary["treatmentexperimental", "Pr(>|t|)"],
    
    post_score.est    = post_score.summary["treatmentexperimental", "Estimate"],
    post_score.est_se = post_score.summary["treatmentexperimental", "Std. Error"],
    post_score.pval   = post_score.summary["treatmentexperimental", "Pr(>|t|)"]
  )
  
  return(ret)
}

Summarise <- function(condition, results, fixed_objects = NULL) {

  Attach(condition)
  Attach(results)
  
  EDR_MCSE  <- function(pval) sqrt(EDR(pval) * (1-EDR(pval)) / length(pval))
  bias_MCSE <- function(est)  var(est) / sqrt(length(est))
  
  ret <- c(
    ANCOVA.EDR             = EDR(ANCOVA.pval),
    ANCOVA.EDR_MCSE        = EDR_MCSE(ANCOVA.pval),
    ANCOVA.bias            = bias(ANCOVA.est, treatment_effect),
    ANCOVA.bias_MCSE       = bias_MCSE(ANCOVA.est),
    ANCOVA.est.var         = var(ANCOVA.est),
    
    change_score.EDR       = EDR(change_score.pval),
    change_score.EDR_MCSE  = EDR_MCSE(change_score.pval),
    change_score.bias      = bias(change_score.est, treatment_effect),
    change_score.bias_MCSE = bias_MCSE(change_score.est),
    change_score.est.var   = var(change_score.est),
    
    post_score.EDR         = EDR(post_score.pval),
    post_score.EDR_MCSE    = EDR_MCSE(post_score.pval),
    post_score.bias        = bias(post_score.est, treatment_effect),
    post_score.bias_MCSE   = bias_MCSE(post_score.est),
    post_score.est.var     = var(post_score.est)
  )
  
  return(ret)
}

#-------------------------------------------------------------------

# run pilot to determine power
res_pilot <- runSimulation(
  design       = Design,
  replications = 100,
  generate     = Generate,
  analyse      = Analyse,
  summarise    = Summarise
)

res_pilot$ANCOVA.est.var       / 0.001^2
res_pilot$change_score.est.var / 0.001^2
res_pilot$post_score.est.var   / 0.001^2
