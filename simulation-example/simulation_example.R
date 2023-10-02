# use `SimDesign::SimFunctions()` to create the template

library(SimDesign)

Design <- createDesign(
  treatment_effect     = c(0.0, 0.2, 0.5),
  pre_post_correlation = c(0.0, 0.5, 0.7),
  n                    = 50
)

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects = NULL) {
  
  treatment_effect     <- condition[["treatment_effect"]]
  pre_post_correlation <- condition[["pre_post_correlation"]]
  n                    <- condition[["n"]]
  
  covariance_matrix <- matrix(c(
    1,                    pre_post_correlation,
    pre_post_correlation,                    1
  ), nrow = 2, ncol = 2, byrow = TRUE)
  
  treatment_control      <- MASS::mvrnorm(n = n, mu = c(0, 0),                Sigma = covariance_matrix)
  treatment_experimental <- MASS::mvrnorm(n = n, mu = c(0, treatment_effect), Sigma = covariance_matrix)
  
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
  
  ANCOVA_summary       <- summary(ANCOVA)$coefficients
  change_score_summary <- summary(change_score)$coefficients
  post_score_summary   <- summary(post_score)$coefficients
  
  ret <- nc(
    ANCOVA_est    = ANCOVA_summary["treatmentexperimental", "Estimate"],
    ANCOVA_est_se = ANCOVA_summary["treatmentexperimental", "Std. Error"],
    ANCOVA_pval   = ANCOVA_summary["treatmentexperimental", "Pr(>|t|)"],
    
    change_score_est    = change_score_summary["treatmentexperimental", "Estimate"],
    change_score_est_se = change_score_summary["treatmentexperimental", "Std. Error"],
    change_score_pval   = change_score_summary["treatmentexperimental", "Pr(>|t|)"],
    
    post_score_est    = post_score_summary["treatmentexperimental", "Estimate"],
    post_score_est_se = post_score_summary["treatmentexperimental", "Std. Error"],
    post_score_pval   = post_score_summary["treatmentexperimental", "Pr(>|t|)"]
  )
  
  return(ret)
}

Summarise <- function(condition, results, fixed_objects = NULL) {

  Attach(condition)
  Attach(results)
  
  EDR_MCSE  <- function(pval) sqrt(EDR(pval) * (1 - EDR(pval)) / length(pval))
  bias_MCSE <- function(est)  var(est) / sqrt(length(est))
  
  ret <- c(
    ANCOVA_EDR             = EDR(ANCOVA_pval),
    ANCOVA_EDR_MCSE        = EDR_MCSE(ANCOVA_pval),
    ANCOVA_bias            = bias(ANCOVA_est, treatment_effect),
    ANCOVA_bias_MCSE       = bias_MCSE(ANCOVA_est),
    ANCOVA_est_var         = var(ANCOVA_est),
    
    change_score_EDR       = EDR(change_score_pval),
    change_score_EDR_MCSE  = EDR_MCSE(change_score_pval),
    change_score_bias      = bias(change_score_est, treatment_effect),
    change_score_bias_MCSE = bias_MCSE(change_score_est),
    change_score_est_var   = var(change_score_est),
    
    post_score_EDR         = EDR(post_score_pval),
    post_score_EDR_MCSE    = EDR_MCSE(post_score_pval),
    post_score_bias        = bias(post_score_est, treatment_effect),
    post_score_bias_MCSE   = bias_MCSE(post_score_est),
    post_score_est_var     = var(post_score_est)
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

res_pilot$ANCOVA_est_var       / 0.005^2
res_pilot$change_score_est_var / 0.005^2
res_pilot$post_score_est_var   / 0.005^2
