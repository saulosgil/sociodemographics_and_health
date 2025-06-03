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

## corrigir valores discrepantes
### ICQ
df <-
  df |>
  mutate(
  icq = case_when(
    icq > 1.8 ~ median(icq),
    icq < 0.5 ~ median(icq),
    .default = icq
  )
) |>
### imc
  mutate(
    imc = case_when(
      imc < 15 ~ median(imc),
      .default = imc
  )
)


## cardiovascular risk
df <-
  df |>
  mutate(
    increased_cv = case_when(
      icq >=0.90 & genero == "masculino" ~ "increased",
      icq < 0.90 & genero == "masculino" ~ "no",
      icq >=0.85 & genero == "feminino" ~ "increased",
      icq < 0.85 & genero == "feminino" ~ "no",
    )
  ) |>
  mutate(
    obesidade = case_when(
      imc >= 30 ~ "obesidade",
      imc < 30 ~ "normal",
    )
  )


## Boxplot dos dados continuos de fator de risco cv
icq_boxplot <-
  ggbetweenstats(
    data = df,
    x = abep_4class,
    y = icq,
    type = "parametric",
    pairwise.display = "none",
    results.subtitle = FALSE
  ) +
  theme_classic() +
  xlab("") +
  ylab(" Waist-to-hip ratio (a.u.)") +
  theme(legend.title = element_blank())  # remove legend label

icq_boxplot

### Frequência de sintomas fator de risco cv aumentado
df |>
  filter(increased_cv %in% c("increased")) |>
  group_by(abep_4class) |>
  count(increased_cv)


df |>
  count(abep_4class)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c("B", "C", "DE")

data <- data.frame(
  count = c(55, 48, 56),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"
purple <- "#7570B3"


icq_categ <-
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
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
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
  ylab("Frequency of individuals with increased risk of\n cardiometabolic diseases (%)") +
  scale_x_discrete(limits = c("B", "C", "DE"))

icq_categ


icq_boxplot+icq_categ

## Boxplot dos dados continuos de bmi
bmi_boxplot <-
  ggbetweenstats(
    data = df,
    x = abep_4class,
    y = imc,
    type = "parametric",
    pairwise.display = "none",
    results.subtitle = FALSE
  ) +
  theme_classic() +
  xlab("") +
  ylab(" Body Mass Index") +
  theme(legend.title = element_blank())  # remove legend label

bmi_boxplot

### Frequência de sintomas bmi
df |>
  filter(obesidade %in% "obesidade") |>
  group_by(abep_4class) |>
  count(obesidade)


df |>
  count(abep_4class)

# Name is an ordered factor. We do this to ensure the bars are sorted.
names <- c("B", "C", "DE")

data <- data.frame(
  count = c(42, 45, 40),
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)

# The colors
green <- "#1b9e77"
orange <- "#d95f02"
purple <- "#7570B3"


bmi_categ <-
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
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
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
  ylab("Frequency of individuals with obesity (%)") +
  scale_x_discrete(limits = c("B", "C", "DE"))

bmi_categ


bmi_boxplot+bmi_categ

## Final layout

(icq_boxplot+icq_categ)/(bmi_boxplot+bmi_categ)







