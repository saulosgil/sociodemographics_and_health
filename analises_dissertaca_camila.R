# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(ggstatsplot)

# Download dataset ----------------------------------------------------------------------------
df <- read_rds("data/df_para_analise.rds")


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
  theme(legend.title = element_blank())  # remove legend label

dep_boxplot

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
  theme(legend.title = element_blank())  # remove legend label

ans_boxplot

## Dotplot dos dados continuos de depressao vs ABEP escore
df |>
  select(abep_score, depressao) |>
  ggplot() +
  geom_point(mapping = aes(x = abep_score,y = depressao)) +
  geom_smooth(mapping = aes(x = abep_score,y = depressao),
              method = "lm") +
  theme_classic()

cor(df$abep_score, df$depressao,method = "p")

## Dotplot dos dados continuos de ansiedade vs ABEP escore
df |>
  select(abep_score, ansiedade) |>
  ggplot() +
  geom_point(mapping = aes(x = abep_score,y = ansiedade)) +
  geom_smooth(mapping = aes(x = abep_score,y = ansiedade),
              method = "lm") +
  theme_classic()

cor(df$abep_score, df$ansiedade,method = "p")

