df <- read.csv("simulation-example/simulation-summaries.csv")

library("xtable")
library("ggplot2")


# pivot wide to long
settings <- c("treatment_effect", "pre_post_correlation")
methods  <- c("ANCOVA", "change_score", "post_score")
dfl      <- do.call(rbind, lapply(methods, function(method){
  temp <- df[,(colnames(df) %in% settings) | grepl(method, colnames(df))]
  colnames(temp) <- gsub(paste0(method, "_"), "", colnames(temp))
  temp$method <- method
  return(temp)
}))

# makes table
print_result <- function(est, mcse, decimals = 4){
  sprintf(paste0("%1$.", decimals, "f (%2$.", decimals, "f)"), est, mcse)
}

table_EDR <- data.frame(
  "Correlation"   = df$pre_post_correlation,
  "Effect"        = df$treatment_effect,
  "ANCOVA"        = print_result(df$ANCOVA_EDR, df$ANCOVA_EDR_MCSE),
  "Change_score"  = print_result(df$change_score_EDR, df$change_score_EDR_MCSE),
  "Post_score"    = print_result(df$post_score_EDR, df$post_score_EDR_MCSE)
)
xtable::xtable(table_EDR)


table_bias <- data.frame(
  "Correlation"   = df$pre_post_correlation,
  "Effect"        = df$treatment_effect,
  "ANCOVA"        = print_result(df$ANCOVA_bias, df$ANCOVA_bias_MCSE),
  "Change_score"  = print_result(df$change_score_bias, df$change_score_bias_MCSE),
  "Post_score"    = print_result(df$post_score_bias, df$post_score_bias_MCSE)
)
xtable::xtable(table_bias)

# make figures
# Sam: I would say facet by correlation, put on x axis the effect size, and then use color for the different methods with position_dodge

# use Bjorn's theme
theme_set(theme_bw() +
            theme(legend.position = "top",
                  panel.grid.minor = element_blank()))
## pal <- "Harmonic" # change palette here
## ## colorspace::hcl_palettes("qualitative", plot = TRUE)
cols <- c("ANCOVA" = "#E69F00", "change_score" = "#009E73", "post_score" = "#0072B2")

# Alternative font
theme_bs <- function(){
  # add google font
  sysfonts::font_add_google("News Cycle", "news")
  # use showtext
  showtext::showtext_auto()
  # theme
  ggplot2::theme_bw(base_family = "news") + 
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      
      # Title and Axis Texts
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.1), hjust = 0.5),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1.25)),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10))
    )
}
theme_set(theme_bs())

plot_EDR <- ggplot(dfl, aes(x = treatment_effect, y = EDR, group = method, ymin = EDR - EDR_MCSE, ymax = EDR + EDR_MCSE)) + 
  geom_point(aes(color = method), position = position_dodge(width = 0.15)) + 
  geom_errorbar(aes(color = method), position=position_dodge(width = 0.15)) +
  scale_x_continuous("Treatment effect", breaks = c(0, 0.2, 0.5)) +
  scale_y_continuous("Power / Type I. error", limits = c(0, 1)) + 
  facet_grid(. ~ pre_post_correlation)
plot_EDR

scale <- 0.94
ggsave("plot_EDR.pdf", plot_EDR, path = here("figures/"), width = scale*17, height = scale*10)


plot_bias <- ggplot(dfl, aes(x = treatment_effect, y = bias, group = method, ymin = bias - bias_MCSE, ymax = bias + bias_MCSE)) + 
  geom_point(aes(color = method), position = position_dodge(width = 0.15)) + 
  geom_errorbar(aes(color = method), position=position_dodge(width = 0.15)) +
  scale_x_continuous("Treatment effect", breaks = c(0, 0.2, 0.5)) +
  scale_y_continuous("Bias", limits = c(-0.01, 0.01)) + 
  facet_grid(. ~ pre_post_correlation)
plot_bias

scale <- 0.94
ggsave("plot_bias.pdf", plot_bias, path = here("figures/"), width = scale*17, height = scale*10)
