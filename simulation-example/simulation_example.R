library(SimDesign)
library(mvtnorm)
library(here)

# simulation conditions
Design <- createDesign(
  treatment_effect     = c(0.0, 0.2, 0.5),
  pre_post_correlation = c(0.0, 0.5, 0.7)
)

# data generation function
Generate <- function(condition, fixed_objects = NULL) {
  
  treatment_effect     <- condition[["treatment_effect"]]
  pre_post_correlation <- condition[["pre_post_correlation"]]
  n                    <- 50
  
  covariance_matrix <- matrix(c(
    1,                    pre_post_correlation,
    pre_post_correlation,                    1
  ), nrow = 2, ncol = 2, byrow = TRUE)
  
  treatment_control      <- mvtnorm::rmvnorm(n = n, mean = c(0, 0),                sigma = covariance_matrix)
  treatment_experimental <- mvtnorm::rmvnorm(n = n, mean = c(0, treatment_effect), sigma = covariance_matrix)
  
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

# data analysis function
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

# simulation summary function
Summarise <- function(condition, results, fixed_objects = NULL) {

  treatment_effect     <- condition[["treatment_effect"]]
  pre_post_correlation <- condition[["pre_post_correlation"]]
  
  ANCOVA_est    <- results[["ANCOVA_est"]]
  ANCOVA_est_se <- results[["ANCOVA_est_se"]]
  ANCOVA_pval   <- results[["ANCOVA_pval"]]
  
  change_score_est    <- results[["change_score_est"]]
  change_score_est_se <- results[["change_score_est_se"]]
  change_score_pval   <- results[["change_score_pval"]]
  
  post_score_est    <- results[["post_score_est"]]
  post_score_est_se <- results[["post_score_est_se"]]
  post_score_pval   <- results[["post_score_pval"]]
  
  EDR_MCSE  <- function(pval) sqrt(EDR(pval) * (1 - EDR(pval)) / length(pval))
  bias_MCSE <- function(est)  sd(est) / sqrt(length(est))
  
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

## ## run pilot simulation to determine empirical variance of parameter
## ## estimates to compute required number of repetitions for bias estimation
## set.seed(1)
## res_pilot <- runSimulation(
##   design       = Design,
##   replications = 100,
##   generate     = Generate,
##   analyse      = Analyse,
##   summarise    = Summarise
## )
## mcse <- 0.005
## ceiling(max(res_pilot$ANCOVA_est_var)       / mcse^2) # 1986
## ceiling(max(res_pilot$change_score_est_var) / mcse^2) # 3812
## ceiling(max(res_pilot$post_score_est_var)   / mcse^2) # 1996
## nsim <- 3812

## determine number of repetitions to achieve 0.005 MCSE in "worst-case" when
## the rejection rate is 0.5 (which leads to maximum MCSE for fixed nsim)
mcse <- 0.005
nsim <- 0.5^2/mcse^2 # 10000

## run full simulation study
set.seed(42)
sim_full <- runSimulation(
  design       = Design,
  replications = nsim,
  generate     = Generate,
  analyse      = Analyse,
  summarise    = Summarise,
  parallel     = TRUE,
  save_results = TRUE # save to HD to avoid RAM issues
)

## save sessionInfo
library(here)
library(sessioninfo)
session_info(to_file = here("simulation-example/session-info.txt"),
             include_base = TRUE, info = "all", dependencies = TRUE)

## save summary data
write.csv(sim_full, file = "simulation-example/simulation-summaries.csv",
          row.names = FALSE)

## save full data
sim_full_results <- SimResults(results = sim_full)
simdata <- do.call("rbind", lapply(X = sim_full_results, FUN = function(x) {
    data.frame(x$condition, x$results)
}))
write.csv(simdata, file = here("simulation-example/simulation-data.csv"),
          row.names = FALSE)
