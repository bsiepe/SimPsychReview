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
  
  ret <- c(
    ANCOVA.EDR         = EDR(ANCOVA.pval),
    ANCOVA.bias        = mean(ANCOVA.est) - treatment_effect,
    
    change_score.EDR   = EDR(change_score.pval),
    change_score.bias  = mean(change_score.est) - treatment_effect,
    
    post_score.EDR     = EDR(post_score.est),
    post_score.bias    = mean(post_score.pval) - treatment_effect
  )
  
  return(ret)
}

#-------------------------------------------------------------------

res <- runSimulation(design=Design, replications=1000, generate=Generate, 
                     analyse=Analyse, summarise=Summarise)
res