source("synthesis.R")
library(ggplot2)

u1 <- U1()
s3 <- Sn(3)
s3 %>%
  ggplot(aes(x = message, y = prob, fill = message)) +
  geom_col(alpha = .7) +
  facet_grid(world ~ QuD) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank()
  )

