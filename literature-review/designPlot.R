library(dplyr)
library(ggplot2)
library(here)

## Björn's theme
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

      # Legen Styling
      legend.text = ggplot2::element_text(size = rel(1.2)),
      legend.title = ggplot2::element_text(size = rel(1.2)),

      # Facetting
      strip.text = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2)),
      # BS: I don't like strip backgrounds, but feel free to uncomment
      # the following line of you do
      strip.background = ggplot2::element_rect(fill = NA)
    )
}
theme_set(theme_bs())

n <- c(20, 40, 60, 80)
p <- c(15, 20, 25, 30)
df1 <- expand.grid(n = n, p = p, included = "Condition included in study design", type = "Fully factorial")
df2 <- df1 %>%
    filter(n >= p) %>%
    mutate(type = "Partially factorial")
df3 <- data.frame(n = n, p = c(15, 25, 30, 20),
                  included = "Condition included in study design", type = "One-at-a-time")
df <- rbind(df1, df2, df3)
ggplot(data = df, aes(x = factor(n), y = factor(p))) +
    facet_wrap(~ type) +
    geom_tile(aes(fill = included), col = 1) +
    labs(x = "Factor 1: Sample size", y = "Factor 2: Number of variables", fill = "") +
    scale_fill_manual(values = c("#009E73")) +
    coord_fixed() +
    theme(panel.grid = element_blank())
ggsave("designPlot.pdf", path = here("figures/"), width = 8, height = 4)
