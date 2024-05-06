library("xtable")
library("ggplot2")
library("here")
library("tidyverse")
library("scales")
library("ggpubr")

df <- read.csv(here("simulation-example/simulation-summaries.csv"))

# pivot wide to long
settings <- c("treatment_effect", "pre_post_correlation")
methods  <- c("ANCOVA", "change_score", "post_score")
dfl      <- do.call(rbind, lapply(methods, function(method){
  temp <- df[,(colnames(df) %in% settings) | grepl(method, colnames(df))]
  colnames(temp) <- gsub(paste0(method, "_"), "", colnames(temp))
  temp$method <- method
  return(temp)
}))

# Tables
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

# Figures
# Bjorn's theme
theme_set(theme_bw() +
            theme(legend.position = "top",
                  panel.grid.minor = element_blank()))
cols <- c("ANCOVA" = "#E69F00", "Change Score" = "#009E73", "Post Score" = "#0072B2")

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
      panel.grid.major.x = element_blank(),
      
      # Title and Axis Texts
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.1), hjust = 0.5),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.2)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1.25)),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
      
      # Legend Styling
      legend.text = ggplot2::element_text(size = rel(1.2)),
      legend.title = ggplot2::element_text(size = rel(1.2)),
      
      # Facetting
      strip.text = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2)),
      strip.background = ggplot2::element_rect(fill = NA)
    )
}
theme_set(theme_bs())

plot_EDR <- dfl %>% 
    mutate(method = gsub("change_score", "Change Score", method)) %>%
    mutate(method = gsub("post_score", "Post Score", method)) %>%
    mutate(treatment_effect = paste0("Effect = ", treatment_effect)) %>% 
    ggplot(aes(x = factor(pre_post_correlation), y = EDR,
               group = method,
               ymin = EDR - EDR_MCSE, ymax = EDR + EDR_MCSE)) +
    geom_vline(xintercept = c(1.5, 2.5), alpha = 0.1) +
    geom_hline(linetype = "dashed", yintercept = 0.05, col = "black", alpha = 0.3, show.legend = FALSE)+
    geom_point(aes(color = method), position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(color = method), position = position_dodge(width = 0.7), width = 0.5) +
    scale_x_discrete("Pre-Post Correlation") +
    scale_y_continuous("Rejection Rate (Type I Error Rate / Power)", limits = c(0, 1),
                       breaks = c(0, 0.05, 0.25, 0.5, 0.75, 1), labels = scales::percent) +
    facet_wrap(. ~ treatment_effect)+
    scale_color_manual(values = cols)+
    labs(group = "Method", color = "Method")
plot_EDR
scale <- 0.94
ggsave("plot_EDR.pdf", plot_EDR, path = here("figures/"), width = scale*10, height = scale*7)


plot_bias <- dfl %>% 
    mutate(method = gsub("change_score", "Change Score", method)) %>%
    mutate(method = gsub("post_score", "Post Score", method)) %>%
    mutate(treatment_effect = paste0("Effect = ", treatment_effect)) %>% 
    ggplot(aes(x = factor(pre_post_correlation), y = bias,
               group = method,
               ymin = bias - bias_MCSE, ymax = bias + bias_MCSE)) +
    geom_vline(xintercept = c(1.5, 2.5), alpha = 0.1) +
    geom_hline(linetype = "dashed", yintercept = 0, col = "grey50", show.legend = FALSE)+
    geom_point(aes(color = method), position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(color = method), position = position_dodge(width = 0.7), width = 0.5) +
    scale_x_discrete("Pre-Post Correlation") +
    scale_y_continuous("Bias", limits = c(-0.01, 0.01)) +
    facet_wrap(. ~ treatment_effect)+
    scale_color_manual(values = cols)+
    labs(group = "Method", color = "Method")
plot_bias
scale <- 0.94
ggsave("plot_bias.pdf", plot_bias, path = here("figures/"), width = scale*10, height = scale*7)


plot_sim_combined <- ggarrange(plot_EDR, plot_bias, ncol = 1,
                               common.legend = TRUE, align = "v")
ggsave("plot_sim_combined.pdf", plot_sim_combined, path = here("figures/"), width = scale*10, height = scale*10)

