# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(ggstatsplot)
library(patchwork)

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

## poor QoL
df <-
  df |>
  mutate(
    poor_physical = case_when(
      whoqol_fisico_escore_100 < 50 ~ "poor",
      whoqol_fisico_escore_100 >= 50 ~ "good",
    )
  ) |>
  mutate(
    poor_psy = case_when(
      whoqol_psicol_escore_100 < 50 ~ "poor",
      whoqol_psicol_escore_100 >= 50 ~ "good",
    )
  ) |>
  mutate(
    poor_social = case_when(
      whoqol_social_escore_100 < 50 ~ "poor",
      whoqol_social_escore_100 >= 50 ~ "good",
    )
  ) |>
  mutate(
    poor_ambiente = case_when(
      whoqol_ambiente_escore_100 < 50 ~ "poor",
      whoqol_ambiente_escore_100 >= 50 ~ "good",
    )
  )

## Boxplot dos dados continuos de who fisico
fisico_boxplot <-
  ggbetweenstats(
    data = df,
    x = abep_4class,
    y = whoqol_fisico_escore_100,
    type = "parametric",
    pairwise.display = "none",
    results.subtitle = FALSE
  ) +
  theme_classic() +
  xlab("") +
  ylab("WHO-QoL score\nPhysical Domain (a.u.)") +
  theme(legend.title = element_blank())  # remove legend label

fisico_boxplot

### Frequência de sintomas fator de poor WHOQOL fisico
df |>
  filter(poor_physical %in% c("poor")) |>
  group_by(abep_4class) |>
  count(poor_physical)


df |>
  count(abep_4class)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c("B", "C", "DE")

data <- data.frame(
  count = c(6, 5, 8),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"
purple <- "#7570B3"


fisico_categ <-
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
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
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
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  ylab("Frequency of individuals with poor quality of life\n Physical Domain (%)") +
  scale_x_discrete(limits = c("B", "C", "DE"))

fisico_categ


fisico_boxplot+fisico_categ

## Boxplot dos dados continuos de who psicologico
psy_boxplot <-
  ggbetweenstats(
    data = df,
    x = abep_4class,
    y = whoqol_psicol_escore_100,
    type = "parametric",
    pairwise.display = "none",
    results.subtitle = FALSE
  ) +
  theme_classic() +
  xlab("") +
  ylab("WHO-QoL score\nPsychological Domain (a.u.)") +
  theme(legend.title = element_blank())  # remove legend label

psy_boxplot

### Frequência de sintomas fator de poor WHOQOL psicologico
df |>
  filter(poor_psy %in% c("poor")) |>
  group_by(abep_4class) |>
  count(poor_psy)


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


psy_categ <-
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
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
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
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  ylab("Frequency of individuals with poor quality of life\n Psychological Domain (%)") +
  scale_x_discrete(limits = c("B", "C", "DE"))

psy_categ


psy_boxplot+psy_categ

## Boxplot dos dados continuos de who social
social_boxplot <-
  ggbetweenstats(
    data = df,
    x = abep_4class,
    y = whoqol_social_escore_100,
    type = "parametric",
    pairwise.display = "none",
    results.subtitle = FALSE
  ) +
  theme_classic() +
  xlab("") +
  ylab("WHO-QoL score\nSocial Domain (a.u.)") +
  theme(legend.title = element_blank())  # remove legend label

social_boxplot

### Frequência de sintomas fator de poor WHOQOL social
df |>
  filter(poor_social %in% c("poor")) |>
  group_by(abep_4class) |>
  count(poor_social)


df |>
  count(abep_4class)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c("B", "C", "DE")

data <- data.frame(
  count = c(7, 9, 8),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"
purple <- "#7570B3"


social_categ <-
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
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
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
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  ylab("Frequency of individuals with poor quality of life\nSocial Domain ($)") +
  scale_x_discrete(limits = c("B", "C", "DE"))

social_categ


social_boxplot+social_categ

## Boxplot dos dados continuos de who ambiente
ambiente_boxplot <-
  ggbetweenstats(
    data = df,
    x = abep_4class,
    y = whoqol_ambiente_escore_100,
    type = "parametric",
    pairwise.display = "none",
    results.subtitle = FALSE
  ) +
  theme_classic() +
  xlab("") +
  ylab("WHO-QoL score\nEnvironment Domain (a.u.)") +
  theme(legend.title = element_blank())  # remove legend label

ambiente_boxplot

### Frequência de sintomas fator de poor WHOQOL ambiente
df |>
  filter(poor_ambiente %in% c("poor")) |>
  group_by(abep_4class) |>
  count(poor_ambiente)


df |>
  count(abep_4class)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c("B", "C", "DE")

data <- data.frame(
  count = c(10, 11, 20),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"
purple <- "#7570B3"


ambiente_categ <-
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
    limits = c(0, 24),
    breaks = seq(0, 24, by = 6),
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
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  ylab("Frequency of individuals with poor quality of life\nEnvironment Domain (%)") +
  scale_x_discrete(limits = c("B", "C", "DE"))

ambiente_categ


ambiente_boxplot+ambiente_categ

## Final layout

(fisico_boxplot+fisico_categ)/(psy_boxplot+psy_categ)
(social_boxplot+social_categ)/(ambiente_boxplot+ambiente_categ)







