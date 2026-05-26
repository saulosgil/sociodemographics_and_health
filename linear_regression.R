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

# adjusted regressions ----------------------------------------------------
# depressao
dep_model <- lm(depressao ~ abep_4class, data = df)
sjPlot::tab_model(dep_model)

dep_model_ajustado <- lm(depressao ~ abep_4class + idade + genero + raca + estado_civil + has + dm, data = df)
sjPlot::tab_model(dep_model_ajustado)

# ansiedade
ans_model <- lm(ansiedade ~ abep_4class, data = df)
sjPlot::tab_model(ans_model)

ans_model_ajustado <- lm(depressao ~ abep_4class + idade + genero + raca + estado_civil + has + dm, data = df)
sjPlot::tab_model(ans_model_ajustado)

# imc
imc_model <- lm(imc ~ abep_4class, data = df)
sjPlot::tab_model(imc_model)

imc_model_ajustado <- lm(imc ~ abep_4class + idade + genero + raca + estado_civil + has + dm, data = df)
sjPlot::tab_model(imc_model_ajustado)

# icq
icq_model <- lm(icq ~ abep_4class, data = df)
sjPlot::tab_model(icq_model)

icq_model_ajustado <- lm(icq ~ abep_4class + idade + genero + raca + estado_civil + has + dm, data = df)
sjPlot::tab_model(icq_model_ajustado)

# who fisico
whoqol_fisico_escore_100_model <- lm(whoqol_fisico_escore_100 ~ abep_4class, data = df)
sjPlot::tab_model(whoqol_fisico_escore_100_model)

whoqol_fisico_escore_100_model_ajustado <- lm(whoqol_fisico_escore_100 ~ abep_4class + idade + genero + raca + estado_civil + has + dm, data = df)
sjPlot::tab_model(whoqol_fisico_escore_100_model_ajustado)

# who psicologico
whoqol_psicol_escore_100_model <- lm(whoqol_psicol_escore_100 ~ abep_4class, data = df)
sjPlot::tab_model(whoqol_psicol_escore_100_model)

whoqol_psicol_escore_100_model_ajustado <- lm(whoqol_psicol_escore_100 ~ abep_4class + idade + genero + raca + estado_civil + has + dm, data = df)
sjPlot::tab_model(whoqol_psicol_escore_100_model_ajustado)

# who social
whoqol_social_escore_100_model <- lm(whoqol_social_escore_100 ~ abep_4class, data = df)
sjPlot::tab_model(whoqol_social_escore_100_model)

whoqol_social_escore_100_model_ajustado <- lm(whoqol_social_escore_100 ~ abep_4class + idade + genero + raca + estado_civil + has + dm, data = df)
sjPlot::tab_model(whoqol_social_escore_100_model_ajustado)

# who ambiental
whoqol_ambiente_escore_100_model <- lm(whoqol_ambiente_escore_100 ~ abep_4class, data = df)
sjPlot::tab_model(whoqol_ambiente_escore_100_model)

whoqol_ambiente_escore_100_model_ajustado <- lm(whoqol_ambiente_escore_100 ~ abep_4class + idade + genero + raca + estado_civil + has + dm, data = df)
sjPlot::tab_model(whoqol_ambiente_escore_100_model_ajustado)




























