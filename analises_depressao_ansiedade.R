# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(ggstatsplot)
library(patchwork)

# Plots formato
theme_manuscript <- function(base_size = 16) {
  theme_classic(base_size = base_size) +
    theme(
      text = element_text(colour = "black"),
      axis.title = element_text(size = base_size, colour = "black"),
      axis.text = element_text(size = base_size - 1, colour = "black"),
      axis.line = element_line(linewidth = 0.7, colour = "black"),
      axis.ticks = element_line(linewidth = 0.6, colour = "black"),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.position = "right",
      plot.tag = element_text(size = base_size + 6, face = "bold"),
      plot.margin = margin(8, 12, 8, 8)
    )
}

theme_bar_manuscript <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.45),
      axis.title = element_text(size = base_size, colour = "black"),
      axis.text = element_text(size = base_size - 1, colour = "black"),
      axis.line.x = element_line(linewidth = 0.7, colour = "black"),
      axis.line.y = element_line(linewidth = 0.7, colour = "black"),
      axis.ticks = element_line(linewidth = 0.6, colour = "black"),
      legend.position = "none",
      plot.tag = element_text(size = base_size + 6, face = "bold"),
      plot.margin = margin(8, 12, 8, 8)
    )
}
# Download dataset ----------------------------------------------------------------------------
df <- read_rds("data/df_para_analise.rds")

# Corrigindo idade
df <-
  df |>
  mutate(idade = case_when(
    idade > 100 ~ 19,
    .default = idade)) |>
  filter(!abep_score > 40) # retirando CEP fora da região Cidade Dutra

# new variables -------------------------------------------------------------------------------
## ABEP classes A, B, C and DE
df <-
  df |>
  mutate(
    abep_4class = case_when(
      abep_class == "A" ~ "A",
      abep_class == "B1" ~ "B",
      abep_class == "B2" ~ "B",
      abep_class == "C1" ~ "C",
      abep_class == "C2" ~ "C",
      abep_class == "DE" ~ "DE",
    )
  )

## Boxplot dos dados continuos de depressão
dep_boxplot <-
  ggbetweenstats(
    data = df,
    x = abep_4class,
    y = depressao,
    type = "parametric",
    pairwise.display = "none",
    results.subtitle = FALSE
  ) +
  theme_classic() +
  xlab("") +
  ylab(" Beck Depression Inventory Score (a.u.)") +
  theme(legend.title = element_blank(),
        # But customize labels for the horizontal axis
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))  # remove legend label

dep_boxplot+theme_manuscript()

### Frequência de sintomas depressao
df |>
  filter(depressao_cat %in% c("Moderate","Severe")) |>
  group_by(abep_4class) |>
  count(depressao_cat)


df |>
  count(abep_4class)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c("B", "C", "DE")

data <- data.frame(
  count = c(11, 7, 4),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"
purple <- "#7570B3"


dep_categ <-
  data |>
  ggplot(mapping = aes(x = names, y = count)) +
  geom_bar(
    stat = "identity",
    fill = c(orange, green,purple),
    colour = "black",
    alpha = .6,
    width = .6
  ) +
  scale_y_continuous(
    limits = c(0, 18),
    breaks = seq(0, 18, by = 6),
    expand = c(0, 0),
    # The horizontal axis does not extend to either side
  ) +
  xlab("") +
  theme_classic() +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  ylab("Frequency of individuals showing moderate/severe\n symptoms of depression (%)") +
  scale_x_discrete(limits = c("B", "C", "DE"))

dep_categ+theme_bar_manuscript()


dep_boxplot+dep_categ

## Boxplot dos dados continuos de ansiedade
ans_boxplot <-
  ggbetweenstats(
    data = df,
    x = abep_4class,
    y = ansiedade,
    type = "parametric",
    pairwise.display = "none",
    results.subtitle = FALSE
  ) +
  theme_classic() +
  xlab("") +
  ylab(" Beck Anxiety Inventory Score (a.u.)") +
  theme(legend.title = element_blank(),
        # But customize labels for the horizontal axis
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))  # remove legend label

ans_boxplot+theme_manuscript()

### Frequência de sintomas ansiedade
df |>
  filter(ansiedade_cat %in% c("Moderate","Severe")) |>
  group_by(abep_4class) |>
  count(ansiedade_cat)


df |>
  count(abep_4class)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c("B", "C", "DE")

data <- data.frame(
  count = c(8, 6, 4),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"
purple <- "#7570B3"


ans_categ <-
  data |>
  ggplot(mapping = aes(x = names, y = count)) +
  geom_bar(
    stat = "identity",
    fill = c(orange, green,purple),
    colour = "black",
    alpha = .6,
    width = .6
  ) +
  scale_y_continuous(
    limits = c(0, 18),
    breaks = seq(0, 18, by = 6),
    expand = c(0, 0),
    # The horizontal axis does not extend to either side
  ) +
  xlab("") +
  theme_classic() +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  ylab("Frequency of individuals showing moderate/severe\n symptoms of anxiety (%)") +
  scale_x_discrete(limits = c("B", "C", "DE"))

ans_categ+theme_bar_manuscript()


ans_boxplot+ans_categ

## Final layout

fig1 <- (dep_boxplot+dep_categ)/(ans_boxplot+ans_categ)


ggsave(
  filename = "figures/Figure_1_depression_anxiety_600dpi.tiff",
  plot = fig1,
  width = 14,
  height = 8.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)




