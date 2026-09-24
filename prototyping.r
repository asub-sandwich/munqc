devtools::document()
devtools::install()
library(tidyverse)
library(munqc)
data(example_scans)
example_scans <- compute_error(example_scans)

### --- Histogram --- ###
plot(example_scans, binwidth = 0.07, facet_books = FALSE) +
  # ggtitle("Figure 1") +
  ylab("No. of Color Chips") +
  theme_classic(base_size = 24) +
  theme(
    # General
    text = element_text(family = "Futura-Book"),
    # plot.title = element_text(size = 20, hjust = 0.5),
    # Panel
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    panel.background = element_rect(
      fill = "#F5F4EE",
      color = NA
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    # Legend
    legend.key.spacing.y = unit(0.1, "in"),
    legend.position = "inside",
    legend.justification = c("left", "top"),
    legend.position.inside = c(0.025, 0.975),
    legend.text = element_text(size = 18),
    legend.background = element_rect(
      # color = "gray80",
      fill = "transparent",
      # linetype = "dotted"
    )
  )
ggsave(
  "~/research/color_books/sssa_figures/histogram_all.png",
  height = 8,
  width = 9,
  bg = "transparent"
)

### --- Page Space Plot --- ###
ranked <- arrange(summary(example_scans)$book, fail_frac)
books <- c(head(ranked, 3)$book_id, tail(ranked, 3)$book_id)
bestworst <- subset_ids(example_scans, books)

plot(bestworst, type = "page_space", facet_books = FALSE) +
  # ggtitle("Figure 2") +
  coord_fixed(xlim = c(0.5, 9), ylim = c(1.5, 8.5)) +
  theme_classic(base_size = 24) +
  theme(
    # General
    text = element_text(family = "Futura-Book"),
    # plot.title = element_text(size = 20, hjust = 0.5),
    # Panel
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    panel.background = element_rect(
      fill = "#F5F4EE",
      color = NA
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = NA
    ),
    # Facet grid title
    strip.background = element_blank(),
    strip.text = element_blank(),
    # Legend
    legend.position = "inside",
    legend.justification = c("right", "bottom"),
    legend.box = "horizontal",
    legend.position.inside = c(0.975, 0.025),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.background = ggfun::element_roundrect(
      color = "#999999",
      fill = "transparent",
      linetype = "dotted",
      linewidth = 0.5,
      r = 0.1
    )
  )
ggsave(
  "~/research/color_books/sssa_figures/pagespace_best_worst.png",
  height = 8,
  width = 9,
  bg = "transparent"
)
