# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)

# Download dataset ----------------------------------------------------------------------------
df <- read_rds("data/df.rds")

# Cleaning dataset ----------------------------------------------------------------------------
## Selecionar caracteristicas
caracteristicas <-
  df |>
  select(
    nome,
    data_de_nascimento_ex_dia_mes_ano_17_10_2022,
    genero,
    raca,
    estado_civil,
    renda_familiar_considere_a_renda_de_todas_as_pessoas_que_moral_na_sua_casa,
    comorbidades_13,
    medicamentos_de_uso_continuo_em_caso_de_nenhum_escrever_nenhum
  )

## Selecionar preditor - Sociodemograficos
sociodemografico <- df[, 4:18]

## Selecionar preditor - Insegurança alimentar
inseguranca_alimentar <-
  df |>
  select(
    starts_with("x")
  )

## Selecionar fator de risco CV - peso, estatura, circunf. cintura e quadril
fator_risco_cv <-
  df |>
  select(
    peso_em_kg_ex_71_8_separador,
    estatura_em_cm_ex_170,
    circunferencia_cintura_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador,
    circunferencia_quadril_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador
    )

## Selecionar preditor - Sintomas de ansiedade
sintomas_ansiedade <-
  df |>
  select(
    starts_with('inventario_de_ansiedade')
)

## Selecionar preditor - Sintomas de depressão
sintomas_depressao <-
  df |>
  select(
    starts_with('inventario_de_depressao')
)

## Selecionar preditor - Indicadores de Qol
whoqol <-
  df |>
  select(
    starts_with('the_world')
)

## Juntar todas as variáveis
df <-
  bind_cols(
  caracteristicas,
  sociodemografico,
  inseguranca_alimentar,
  fator_risco_cv,
  sintomas_ansiedade,
  sintomas_depressao,
  whoqol
)

# verifica as colunas do dataset
glimpse(df)

# tratamento das variáveis que aparecem como list ---------------------------------------------
# data de nascimento
# tira da lista e cria um vetor de datas de nascimento (porém as linhas erradas foram excluidas)
d <- unlist(df$data_de_nascimento_ex_dia_mes_ano_17_10_2022,
            use.names = TRUE)

# Muda a barra para hífen para depois converter para data
d <- str_replace_all(d,  "/", "-")

# converte em tibble
d <- as_tibble(d)
d

# cria um vetor de NA para incluir nas linhas erradas
c_d <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_d <- as_tibble(c_d)
c_d

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
data_nasc <- bind_rows(d, c_d)
data_nasc

# Muda o vetor original de data de nascimento para o vetor ajustado do tipo Date
df_ajustado <-
  df |>
  mutate(
    data_de_nascimento_ex_dia_mes_ano_17_10_2022 = data_nasc$value
  ) |>
  rename(
    data_nasc = data_de_nascimento_ex_dia_mes_ano_17_10_2022
  ) |>
  mutate(
    data_nasc = as.Date(data_nasc, format = "%d-%m-%Y")
  )

# Peso
# tira da lista e cria um vetor com os pesos (porém as linhas erradas foram excluidas)
p <- unlist(df$peso_em_kg_ex_71_8_separador,
            use.names = TRUE)

# converte em tibble
p <- as_tibble(p)
p

# cria um vetor de NA para incluir nas linhas erradas
c_p <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_p <- as_tibble(c_p)
c_p

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
peso <- bind_rows(p, c_p)
peso

# Muda o vetor original de peso para o vetor ajustado do tipo Double
# df <-
df_ajustado <-
  df_ajustado |>
  mutate(
    peso_em_kg_ex_71_8_separador = peso$value
  ) |>
  rename(
    peso = peso_em_kg_ex_71_8_separador
  ) |>
  mutate(
    peso = as.double(peso)
  )

# Estatura
# tira da lista e cria um vetor com as estaturas em cm (porém as linhas erradas foram excluidas)
e <- unlist(df$estatura_em_cm_ex_170,
            use.names = TRUE)

# converte em tibble
e <- as_tibble(e)
e

# cria um vetor de NA para incluir nas linhas erradas
c_e <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_e <- as_tibble(c_e)
c_e

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
estatura <- bind_rows(e, c_e)
estatura

# Muda o vetor original de estatura para o vetor ajustado do tipo Double
# df <-
df_ajustado <-
  df_ajustado |>
  mutate(
    estatura_em_cm_ex_170 = estatura$value
  ) |>
  rename(
    estatura = estatura_em_cm_ex_170
  ) |>
  mutate(
    estatura = as.double(estatura)
  )

# Circunferencia de cintura
# tira da lista e cria um vetor com as circunferencias da cintura em cm (porém as linhas erradas foram excluidas)
cc <- unlist(df$circunferencia_cintura_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador,
            use.names = TRUE)

# converte em tibble
cc <- as_tibble(cc)
cc

# cria um vetor de NA para incluir nas linhas erradas
c_cc <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_cc <- as_tibble(c_cc)
c_cc

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
circ_cintura <- bind_rows(cc, c_cc)
circ_cintura

# Muda o vetor original da circunf. da cintura para o vetor ajustado do tipo String
# df <-
df_ajustado <-
  df_ajustado |>
  mutate(
    circunferencia_cintura_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador = circ_cintura$value
  ) |>
  rename(
    circ_cintura = circunferencia_cintura_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador
  )

# Circunferencia do quadril
# tira da lista e cria um vetor com as circunferencias da cintura em cm (porém as linhas erradas foram excluidas)
q <- unlist(df$circunferencia_quadril_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador,
            use.names = TRUE)

# converte em tibble
q <- as_tibble(q)
q

# cria um vetor de NA para incluir nas linhas erradas
c_q <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_q <- as_tibble(c_q)
c_q

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
circ_quadril <- bind_rows(q, c_q)
circ_quadril

# Muda o vetor original da circunf. da cintura para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    circunferencia_quadril_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador = circ_quadril$value
  ) |>
  rename(
    circ_quadril = circunferencia_quadril_colocar_todas_as_medidas_separado_por_barra_ex_84_83_84_separador
  )

# inventario_de_ansiedade_de_beck_demencia_ou_formigamento
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans1 <- unlist(df$inventario_de_ansiedade_de_beck_demencia_ou_formigamento,
               use.names = TRUE)

# converte em tibble
ans1 <- as_tibble(ans1)
ans1

# cria um vetor de NA para incluir nas linhas erradas
c_ans1 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans1 <- as_tibble(c_ans1)
c_ans1

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
v_ans1 <- bind_rows(ans1, c_ans1)
v_ans1

# Muda o vetor original da inventario_de_ansiedade_de_beck_demencia_ou_formigamento para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_demencia_ou_formigamento = v_ans1$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_demencia_ou_formigamento = as.integer(inventario_de_ansiedade_de_beck_demencia_ou_formigamento)
  )

# inventario_de_ansiedade_de_beck_sensacao_de_calor
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans2 <- unlist(df$inventario_de_ansiedade_de_beck_sensacao_de_calor,
               use.names = TRUE)

# converte em tibble
ans2 <- as_tibble(ans2)
ans2 <-
  ans2 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans2 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans2 <- as_tibble(c_ans2)
c_ans2

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
v_ans2 <- bind_rows(ans2, c_ans2)
v_ans2

# Muda o vetor original da inventario_de_ansiedade_de_beck_sensacao_de_calor para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_sensacao_de_calor = v_ans2$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_sensacao_de_calor = as.integer(inventario_de_ansiedade_de_beck_sensacao_de_calor)
  )

# inventario_de_ansiedade_de_beck_tremores_nas_pernas
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans3 <- unlist(df$inventario_de_ansiedade_de_beck_tremores_nas_pernas,
               use.names = TRUE)

# converte em tibble
ans3 <- as_tibble(ans3)
ans3 <-
  ans3 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans3 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans3 <- as_tibble(c_ans3)
c_ans3

# Empilha as linhas para ter um vetor de 319 linhas e, assim, poder juntar no df original
v_ans3 <- bind_rows(ans3, c_ans3)
v_ans3

# Muda o vetor original da inventario_de_ansiedade_de_beck_tremores_nas_pernas para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_pernas = v_ans3$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_pernas = as.integer(inventario_de_ansiedade_de_beck_tremores_nas_pernas)
  )

# inventario_de_ansiedade_de_beck_incapaz_de_relaxar
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans4 <- unlist(df$inventario_de_ansiedade_de_beck_incapaz_de_relaxar,
               use.names = TRUE)

# converte em tibble
ans4 <- as_tibble(ans4)
ans4 <-
  ans4 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans4 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans4 <- as_tibble(c_ans4)
c_ans4

# Empilha as linhas para ter um vetor de 419 linhas e, assim, poder juntar no df original
v_ans4 <- bind_rows(ans4, c_ans4)
v_ans4

# Muda o vetor original da inventario_de_ansiedade_de_beck_incapaz_de_relaxar para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_incapaz_de_relaxar = v_ans4$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_incapaz_de_relaxar = as.integer(inventario_de_ansiedade_de_beck_incapaz_de_relaxar)
  )

# inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans5 <- unlist(df$inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior,
               use.names = TRUE)

# converte em tibble
ans5 <- as_tibble(ans5)
ans5 <-
  ans5 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans5 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans5 <- as_tibble(c_ans5)
c_ans5

# Empilha as linhas para ter um vetor de 519 linhas e, assim, poder juntar no df original
v_ans5 <- bind_rows(ans5, c_ans5)
v_ans5

# Muda o vetor original da inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior = v_ans5$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior = as.integer(inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior)
  )

# inventario_de_ansiedade_de_beck_sem_equilibrio
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans6 <- unlist(df$inventario_de_ansiedade_de_beck_sem_equilibrio,
               use.names = TRUE)

# converte em tibble
ans6 <- as_tibble(ans6)
ans6 <-
  ans6 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans6 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans6 <- as_tibble(c_ans6)
c_ans6

# Empilha as linhas para ter um vetor de 619 linhas e, assim, poder juntar no df original
v_ans6 <- bind_rows(ans6, c_ans6)
v_ans6

# Muda o vetor original da inventario_de_ansiedade_de_beck_sem_equilibrio para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_sem_equilibrio = v_ans6$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_sem_equilibrio = as.integer(inventario_de_ansiedade_de_beck_sem_equilibrio)
  )

# inventario_de_ansiedade_de_beck_nervoso
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans7 <- unlist(df$inventario_de_ansiedade_de_beck_nervoso,
               use.names = TRUE)

# converte em tibble
ans7 <- as_tibble(ans7)
ans7 <-
  ans7 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans7 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans7 <- as_tibble(c_ans7)
c_ans7

# Empilha as linhas para ter um vetor de 719 linhas e, assim, poder juntar no df original
v_ans7 <- bind_rows(ans7, c_ans7)
v_ans7

# Muda o vetor original da inventario_de_ansiedade_de_beck_nervoso para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_nervoso = v_ans7$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_nervoso = as.integer(inventario_de_ansiedade_de_beck_nervoso)
  )

# inventario_de_ansiedade_de_beck_sensacao_de_sufocacao
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans8 <- unlist(df$inventario_de_ansiedade_de_beck_sensacao_de_sufocacao,
               use.names = TRUE)

# converte em tibble
ans8 <- as_tibble(ans8)
ans8 <-
  ans8 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans8 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans8 <- as_tibble(c_ans8)
c_ans8

# Empilha as linhas para ter um vetor de 819 linhas e, assim, poder juntar no df original
v_ans8 <- bind_rows(ans8, c_ans8)
v_ans8

# Muda o vetor original da inventario_de_ansiedade_de_beck_sensacao_de_sufocacao para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_sensacao_de_sufocacao = v_ans8$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_sensacao_de_sufocacao = as.integer(inventario_de_ansiedade_de_beck_sensacao_de_sufocacao)
  )

# inventario_de_ansiedade_de_beck_tremores_nas_maos
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans9 <- unlist(df$inventario_de_ansiedade_de_beck_tremores_nas_maos,
               use.names = TRUE)

# converte em tibble
ans9 <- as_tibble(ans9)
ans9 <-
  ans9 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans9 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans9 <- as_tibble(c_ans9)
c_ans9

# Empilha as linhas para ter um vetor de 919 linhas e, assim, poder juntar no df original
v_ans9 <- bind_rows(ans9, c_ans9)
v_ans9

# Muda o vetor original da inventario_de_ansiedade_de_beck_tremores_nas_maos para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_maos = v_ans9$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_maos = as.integer(inventario_de_ansiedade_de_beck_tremores_nas_maos)
  )

# inventario_de_ansiedade_de_beck_tremores_nas_maos
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
ans10 <- unlist(df$inventario_de_ansiedade_de_beck_tremores_nas_maos,
                use.names = TRUE)

# converte em tibble
ans10 <- as_tibble(ans10)
ans10 <-
  ans10 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_ans10 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_ans10 <- as_tibble(c_ans10)
c_ans10

# Empilha as linhas para ter um vetor de 10110 linhas e, assim, poder juntar no df original
v_ans10 <- bind_rows(ans10, c_ans10)
v_ans10

# Muda o vetor original da inventario_de_ansiedade_de_beck_tremores_nas_maos para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_maos = v_ans10$value
  ) |>
  mutate(
    inventario_de_ansiedade_de_beck_tremores_nas_maos = as.integer(inventario_de_ansiedade_de_beck_tremores_nas_maos)
  )
# inventario_de_depressao_de_beck_questao_11
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
dep1 <- unlist(df$inventario_de_depressao_de_beck_questao_11,
               use.names = TRUE)

# converte em tibble
dep1 <- as_tibble(dep1)
dep1 <-
  dep1 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_dep1 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_dep1 <- as_tibble(c_dep1)
c_dep1

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_dep1 <- bind_rows(dep1, c_dep1)
v_dep1

# Muda o vetor original da inventario_de_depressao_de_beck_questao_11 para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_depressao_de_beck_questao_11 = v_dep1$value
  ) |>
  mutate(
    inventario_de_depressao_de_beck_questao_11 = as.integer(inventario_de_depressao_de_beck_questao_11)
  )

# inventario_de_depressao_de_beck_questao_12
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
dep2 <- unlist(df$inventario_de_depressao_de_beck_questao_12,
               use.names = TRUE)

# converte em tibble
dep2 <- as_tibble(dep2)
dep2 <-
  dep2 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_dep2 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_dep2 <- as_tibble(c_dep2)
c_dep2

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_dep2 <- bind_rows(dep2, c_dep2)
v_dep2

# Muda o vetor original da inventario_de_depressao_de_beck_questao_12 para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_depressao_de_beck_questao_12 = v_dep2$value
  ) |>
  mutate(
    inventario_de_depressao_de_beck_questao_12 = as.integer(inventario_de_depressao_de_beck_questao_12)
  )

# inventario_de_depressao_de_beck_questao_19
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
dep3 <- unlist(df$inventario_de_depressao_de_beck_questao_19,
               use.names = TRUE)

# converte em tibble
dep3 <- as_tibble(dep3)
dep3 <-
  dep3 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_dep3 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_dep3 <- as_tibble(c_dep3)
c_dep3

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_dep3 <- bind_rows(dep3, c_dep3)
v_dep3

# Muda o vetor original da inventario_de_depressao_de_beck_questao_19 para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    inventario_de_depressao_de_beck_questao_19 = v_dep3$value
  ) |>
  mutate(
    inventario_de_depressao_de_beck_questao_19 = as.integer(inventario_de_depressao_de_beck_questao_19)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_como_voce_avaliaria_sua_qualidade_de_vida
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who1 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_como_voce_avaliaria_sua_qualidade_de_vida,
               use.names = TRUE)

# converte em tibble
who1 <- as_tibble(who1)
who1 <-
  who1 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who1 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who1 <- as_tibble(c_who1)
c_who1

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who1 <- bind_rows(who1, c_who1)
v_who1

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_como_voce_avaliaria_sua_qualidade_de_vida para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_como_voce_avaliaria_sua_qualidade_de_vida = v_who1$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_como_voce_avaliaria_sua_qualidade_de_vida = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_como_voce_avaliaria_sua_qualidade_de_vida)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_a_sua_saude
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who2 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_a_sua_saude,
               use.names = TRUE)

# converte em tibble
who2 <- as_tibble(who2)
who2 <-
  who2 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who2 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who2 <- as_tibble(c_who2)
c_who2

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who2 <- bind_rows(who2, c_who2)
v_who2

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_a_sua_saude para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_a_sua_saude = v_who2$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_a_sua_saude = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_a_sua_saude)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_sua_dor_fisica_impede_voce_de_fazer_o_que_voce_precisa
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who3 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_sua_dor_fisica_impede_voce_de_fazer_o_que_voce_precisa,
               use.names = TRUE)

# converte em tibble
who3 <- as_tibble(who3)
who3 <-
  who3 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who3 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who3 <- as_tibble(c_who3)
c_who3

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who3 <- bind_rows(who3, c_who3)
v_who3

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_sua_dor_fisica_impede_voce_de_fazer_o_que_voce_precisa para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_sua_dor_fisica_impede_voce_de_fazer_o_que_voce_precisa = v_who3$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_sua_dor_fisica_impede_voce_de_fazer_o_que_voce_precisa = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_sua_dor_fisica_impede_voce_de_fazer_o_que_voce_precisa)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_precisa_de_algum_tratamento_medico_para_levar_sua_vida_diaria
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who4 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_precisa_de_algum_tratamento_medico_para_levar_sua_vida_diaria,
               use.names = TRUE)

# converte em tibble
who4 <- as_tibble(who4)
who4 <-
  who4 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who4 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who4 <- as_tibble(c_who4)
c_who4

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who4 <- bind_rows(who4, c_who4)
v_who4

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_precisa_de_algum_tratamento_medico_para_levar_sua_vida_diaria para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_precisa_de_algum_tratamento_medico_para_levar_sua_vida_diaria = v_who4$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_precisa_de_algum_tratamento_medico_para_levar_sua_vida_diaria = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_precisa_de_algum_tratamento_medico_para_levar_sua_vida_diaria)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_aproveita_a_vida
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who5 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_aproveita_a_vida,
               use.names = TRUE)

# converte em tibble
who5 <- as_tibble(who5)
who5 <-
  who5 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who5 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who5 <- as_tibble(c_who5)
c_who5

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who5 <- bind_rows(who5, c_who5)
v_who5

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_aproveita_a_vida para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_aproveita_a_vida = v_who5$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_aproveita_a_vida = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_aproveita_a_vida)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_a_sua_vida_tem_sentido
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who6 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_a_sua_vida_tem_sentido,
               use.names = TRUE)

# converte em tibble
who6 <- as_tibble(who6)
who6 <-
  who6 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who6 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who6 <- as_tibble(c_who6)
c_who6

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who6 <- bind_rows(who6, c_who6)
v_who6

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_a_sua_vida_tem_sentido para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_a_sua_vida_tem_sentido = v_who6$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_a_sua_vida_tem_sentido = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_a_sua_vida_tem_sentido)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_consegue_se_concentrar
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who7 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_consegue_se_concentrar,
               use.names = TRUE)

# converte em tibble
who7 <- as_tibble(who7)
who7 <-
  who7 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who7 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who7 <- as_tibble(c_who7)
c_who7

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who7 <- bind_rows(who7, c_who7)
v_who7

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_consegue_se_concentrar para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_consegue_se_concentrar = v_who7$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_consegue_se_concentrar = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_consegue_se_concentrar)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_voce_e_capaz_de_aceitar_sua_aparencia_fisica
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who8 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_voce_e_capaz_de_aceitar_sua_aparencia_fisica,
               use.names = TRUE)

# converte em tibble
who8 <- as_tibble(who8)
who8 <-
  who8 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who8 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who8 <- as_tibble(c_who8)
c_who8

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who8 <- bind_rows(who8, c_who8)
v_who8

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_voce_e_capaz_de_aceitar_sua_aparencia_fisica para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_voce_e_capaz_de_aceitar_sua_aparencia_fisica = v_who8$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_voce_e_capaz_de_aceitar_sua_aparencia_fisica = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_voce_e_capaz_de_aceitar_sua_aparencia_fisica)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_voce_tem_dinheiro_suficiente_para_satisfazer_suas_necessidades
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who9 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_voce_tem_dinheiro_suficiente_para_satisfazer_suas_necessidades,
               use.names = TRUE)

# converte em tibble
who9 <- as_tibble(who9)
who9 <-
  who9 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who9 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who9 <- as_tibble(c_who9)
c_who9

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who9 <- bind_rows(who9, c_who9)
v_who9

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_voce_tem_dinheiro_suficiente_para_satisfazer_suas_necessidades para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_voce_tem_dinheiro_suficiente_para_satisfazer_suas_necessidades = v_who9$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_voce_tem_dinheiro_suficiente_para_satisfazer_suas_necessidades = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_voce_tem_dinheiro_suficiente_para_satisfazer_suas_necessidades)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_quao_disponiveis_para_voce_estao_as_informacoes_que_precisa_no_seu_dia_a_dia
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who10 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_quao_disponiveis_para_voce_estao_as_informacoes_que_precisa_no_seu_dia_a_dia,
                use.names = TRUE)

# converte em tibble
who10 <- as_tibble(who10)
who10 <-
  who10 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who10 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who10 <- as_tibble(c_who10)
c_who10

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who10 <- bind_rows(who10, c_who10)
v_who10

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_quao_disponiveis_para_voce_estao_as_informacoes_que_precisa_no_seu_dia_a_dia para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_disponiveis_para_voce_estao_as_informacoes_que_precisa_no_seu_dia_a_dia = v_who10$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_disponiveis_para_voce_estao_as_informacoes_que_precisa_no_seu_dia_a_dia = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_quao_disponiveis_para_voce_estao_as_informacoes_que_precisa_no_seu_dia_a_dia)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_quao_bem_voce_e_capaz_de_se_locomover
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who11 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_quao_bem_voce_e_capaz_de_se_locomover,
                use.names = TRUE)

# converte em tibble
who11 <- as_tibble(who11)
who11 <-
  who11 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who11 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who11 <- as_tibble(c_who11)
c_who11

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who11 <- bind_rows(who11, c_who11)
v_who11

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_quao_bem_voce_e_capaz_de_se_locomover para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_bem_voce_e_capaz_de_se_locomover = v_who11$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_bem_voce_e_capaz_de_se_locomover = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_quao_bem_voce_e_capaz_de_se_locomover)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_o_seu_sono
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who12 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_o_seu_sono,
                use.names = TRUE)

# converte em tibble
who12 <- as_tibble(who12)
who12 <-
  who12 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who12 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who12 <- as_tibble(c_who12)
c_who12

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who12 <- bind_rows(who12, c_who12)
v_who12

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_o_seu_sono para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_o_seu_sono = v_who12$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_o_seu_sono = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_o_seu_sono)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_capacidade_de_desempenhar_as_atividades_do_seu_dia_a_dia
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who13 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_capacidade_de_desempenhar_as_atividades_do_seu_dia_a_dia,
                use.names = TRUE)

# converte em tibble
who13 <- as_tibble(who13)
who13 <-
  who13 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who13 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who13 <- as_tibble(c_who13)
c_who13

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who13 <- bind_rows(who13, c_who13)
v_who13

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_capacidade_de_desempenhar_as_atividades_do_seu_dia_a_dia para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_capacidade_de_desempenhar_as_atividades_do_seu_dia_a_dia = v_who13$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_capacidade_de_desempenhar_as_atividades_do_seu_dia_a_dia = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_capacidade_de_desempenhar_as_atividades_do_seu_dia_a_dia)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_suas_relacoes_pessoais_amigos_parentes_conhecidos_colegas
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who14 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_suas_relacoes_pessoais_amigos_parentes_conhecidos_colegas,
                use.names = TRUE)

# converte em tibble
who14 <- as_tibble(who14)
who14 <-
  who14 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who14 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who14 <- as_tibble(c_who14)
c_who14

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who14 <- bind_rows(who14, c_who14)
v_who14

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_suas_relacoes_pessoais_amigos_parentes_conhecidos_colegas para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_suas_relacoes_pessoais_amigos_parentes_conhecidos_colegas = v_who14$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_suas_relacoes_pessoais_amigos_parentes_conhecidos_colegas = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_suas_relacoes_pessoais_amigos_parentes_conhecidos_colegas)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_vida_sexual
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who15 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_vida_sexual,
                use.names = TRUE)

# converte em tibble
who15 <- as_tibble(who15)
who15 <-
  who15 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who15 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who15 <- as_tibble(c_who15)
c_who15

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who15 <- bind_rows(who15, c_who15)
v_who15

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_vida_sexual para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_vida_sexual = v_who15$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_vida_sexual = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_vida_sexual)
  )

# the_world_health_organization_quality_of_life_whoqol_bref_com_que_frequencia_voce_tem_sentimentos_negativos_tais_como_mau_humor_desespero_ansiedade_depressao
# tira da lista e cria um vetor(porém as linhas erradas foram excluidas)
who16 <- unlist(df$the_world_health_organization_quality_of_life_whoqol_bref_com_que_frequencia_voce_tem_sentimentos_negativos_tais_como_mau_humor_desespero_ansiedade_depressao,
                use.names = TRUE)

# converte em tibble
who16 <- as_tibble(who16)
who16 <-
  who16 |>
  mutate(value = as.character(value))

# cria um vetor de NA para incluir nas linhas erradas
c_who16 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")

# converte em tibble
c_who16 <- as_tibble(c_who16)
c_who16

# Empilha as linhas para ter um vetor de 111 linhas e, assim, poder juntar no df original
v_who16 <- bind_rows(who16, c_who16)
v_who16

# Muda o vetor original da the_world_health_organization_quality_of_life_whoqol_bref_com_que_frequencia_voce_tem_sentimentos_negativos_tais_como_mau_humor_desespero_ansiedade_depressao para o vetor ajustado do tipo String
df_ajustado <-
  df_ajustado |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_com_que_frequencia_voce_tem_sentimentos_negativos_tais_como_mau_humor_desespero_ansiedade_depressao = v_who16$value
  ) |>
  mutate(
    the_world_health_organization_quality_of_life_whoqol_bref_com_que_frequencia_voce_tem_sentimentos_negativos_tais_como_mau_humor_desespero_ansiedade_depressao = as.integer(the_world_health_organization_quality_of_life_whoqol_bref_com_que_frequencia_voce_tem_sentimentos_negativos_tais_como_mau_humor_desespero_ansiedade_depressao)
  )

# Ajustando as colunas circunferencia de cintura e quadril
df_ajustado <-
  df_ajustado |>
  # separando as tentativas do circunferencia cintura
  separate(
    col = circ_cintura,
    into = c("circ_cintura_1","circ_cintura_2","circ_cintura_3"),
    sep = "/",
    convert = TRUE
  ) |>
  # convertendo o circunferencia cintura para numerico
  mutate(circ_cintura_1 = as.numeric(circ_cintura_1),
         circ_cintura_2 = as.numeric(circ_cintura_2),
         circ_cintura_3 = as.numeric(circ_cintura_3)) |>
  # separando as tentativas do circunferencia quadril
  separate(
    col = circ_quadril,
    into = c("circ_quadril_1","circ_quadril_2","circ_quadril_3"),
    sep = "/",
    convert = TRUE
  ) |>
  # convertendo o circunferencia quadril para numerico
  mutate(circ_quadril_1 = as.numeric(circ_quadril_1),
         circ_quadril_2 = as.numeric(circ_quadril_2),
         circ_quadril_3 = as.numeric(circ_quadril_3))

# Ajustando as variáveis HAS e DM
has <- str_detect(string = df_ajustado$comorbidades_13,
                  pattern = "hipertensao arterial")

dm <- str_detect(string = df_ajustado$comorbidades_13,
                 pattern = "diabetes")

df_ajustado <-
  df_ajustado |>
  mutate(
    has = case_when(has == TRUE ~ 1,
                    has != TRUE ~ 0),
    dm = case_when(dm == TRUE ~ 1,
                   dm != TRUE ~ 0),
  ) |>
  mutate(
    has = as.integer(has),
    dm = as.integer(dm)
  )


# Ajustando as variáveis medicamentos
metformina = str_detect(string = df_ajustado$medicamentos_de_uso_continuo_em_caso_de_nenhum_escrever_nenhum,
                        pattern = "met")

losartana = str_detect(string = df_ajustado$medicamentos_de_uso_continuo_em_caso_de_nenhum_escrever_nenhum,
                        pattern = "losa")

hidroclorotiazida = str_detect(string = df_ajustado$medicamentos_de_uso_continuo_em_caso_de_nenhum_escrever_nenhum,
                       pattern = "hidro")

enalapril = str_detect(string = df_ajustado$medicamentos_de_uso_continuo_em_caso_de_nenhum_escrever_nenhum,
                       pattern = "ena")

atenolol = str_detect(string = df_ajustado$medicamentos_de_uso_continuo_em_caso_de_nenhum_escrever_nenhum,
                       pattern = "aten")


df_ajustado <-
  df_ajustado |>
  mutate(
    metformina = case_when(metformina == TRUE ~ 1,
                           metformina != TRUE ~ 0,
                           is.na(metformina) ~ 0),
    losartana = case_when(losartana == TRUE  ~ 1,
                          losartana != TRUE  ~ 0,
                          is.na(losartana) ~ 0),
    hidroclorotiazida = case_when(hidroclorotiazida == TRUE  ~ 1,
                                  hidroclorotiazida != TRUE  ~ 0,
                                  is.na(hidroclorotiazida) ~ 0),
    enalapril = case_when(enalapril == TRUE  ~ 1,
                          enalapril != TRUE  ~ 0,
                          is.na(enalapril) ~ 0),
    atenolol = case_when(atenolol == TRUE  ~ 1,
                         atenolol != TRUE  ~ 0,
                         is.na(atenolol) ~ 0)
  ) |>
  mutate(
    metformina = as.integer(metformina),
    losartana = as.integer(losartana),
    hidroclorotiazida = as.integer(hidroclorotiazida),
    enalapril = as.integer(enalapril),
    atenolol = as.integer(atenolol)
  ) |>
  select(-comorbidades_13,
         -medicamentos_de_uso_continuo_em_caso_de_nenhum_escrever_nenhum)

### Ajustando variáveis para insegurança alimentar
df_ajustado <-
  df_ajustado |>
  mutate(
    x1_nos_ultimos_tres_meses_os_moradores_deste_domicilio_tiveram_preocupacao_de_que_os_alimentos_acabassem_antes_de_poderem_comprar_ou_receber_mais_comida = case_when(x1_nos_ultimos_tres_meses_os_moradores_deste_domicilio_tiveram_preocupacao_de_que_os_alimentos_acabassem_antes_de_poderem_comprar_ou_receber_mais_comida == "sim" ~ 1,
                                                                                                                                                                         x1_nos_ultimos_tres_meses_os_moradores_deste_domicilio_tiveram_preocupacao_de_que_os_alimentos_acabassem_antes_de_poderem_comprar_ou_receber_mais_comida != "sim" ~ 0),
    x2_nos_ultimos_tres_meses_os_alimentos_acabaram_antes_que_os_moradores_deste_domicilio_tivessem_dinheiro_para_comprar_mais_comida = case_when(x2_nos_ultimos_tres_meses_os_alimentos_acabaram_antes_que_os_moradores_deste_domicilio_tivessem_dinheiro_para_comprar_mais_comida == "sim" ~ 1,
                                                                                                                                                  x2_nos_ultimos_tres_meses_os_alimentos_acabaram_antes_que_os_moradores_deste_domicilio_tivessem_dinheiro_para_comprar_mais_comida != "sim" ~ 0),
    x3_nos_ultimos_tres_meses_os_moradores_deste_domicilio_ficaram_sem_dinheiro_para_ter_uma_alimentacao_saudavel_e_variada = case_when(x3_nos_ultimos_tres_meses_os_moradores_deste_domicilio_ficaram_sem_dinheiro_para_ter_uma_alimentacao_saudavel_e_variada == "sim" ~ 1,
                                                                                                                                        x3_nos_ultimos_tres_meses_os_moradores_deste_domicilio_ficaram_sem_dinheiro_para_ter_uma_alimentacao_saudavel_e_variada != "sim" ~ 0),
    x4_nos_ultimos_tres_meses_os_moradores_deste_domicilio_comeram_apenas_alguns_alimentos_que_ainda_tinham_porque_o_dinheiro_acabou = case_when(x4_nos_ultimos_tres_meses_os_moradores_deste_domicilio_comeram_apenas_alguns_alimentos_que_ainda_tinham_porque_o_dinheiro_acabou == "sim" ~ 1,
                                                                                                                                                 x4_nos_ultimos_tres_meses_os_moradores_deste_domicilio_comeram_apenas_alguns_alimentos_que_ainda_tinham_porque_o_dinheiro_acabou != "sim" ~ 0),
    x5_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_deixou_de_fazer_uma_refeicao_porque_nao_havia_dinheiro_para_comprar_comida = case_when(x5_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_deixou_de_fazer_uma_refeicao_porque_nao_havia_dinheiro_para_comprar_comida == "sim" ~ 1,
                                                                                                                                                               x5_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_deixou_de_fazer_uma_refeicao_porque_nao_havia_dinheiro_para_comprar_comida != "sim" ~ 0),
    x6_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_comeu_menos_do_que_devia_porque_nao_havia_dinheiro_para_comprar_comida = case_when(x6_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_comeu_menos_do_que_devia_porque_nao_havia_dinheiro_para_comprar_comida == "sim" ~ 1,
                                                                                                                                                                      x6_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_comeu_menos_do_que_devia_porque_nao_havia_dinheiro_para_comprar_comida != "sim" ~ 0),
    x7_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_sentiu_fome_mas_nao_comeu_porque_nao_havia_dinheiro_para_comprar_comida = case_when(x7_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_sentiu_fome_mas_nao_comeu_porque_nao_havia_dinheiro_para_comprar_comida == "sim" ~ 1,
                                                                                                                                                                       x7_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_sentiu_fome_mas_nao_comeu_porque_nao_havia_dinheiro_para_comprar_comida != "sim" ~ 0),
    x8_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_fez_apenas_uma_refeicao_ao_dia_ou_ficou_um_dia_inteiro_sem_comer_porque_nao_havia_dinheiro_para_comprar_comida = case_when(x8_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_fez_apenas_uma_refeicao_ao_dia_ou_ficou_um_dia_inteiro_sem_comer_porque_nao_havia_dinheiro_para_comprar_comida == "sim" ~ 1,
                                                                                                                                                                                                              x8_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_fez_apenas_uma_refeicao_ao_dia_ou_ficou_um_dia_inteiro_sem_comer_porque_nao_havia_dinheiro_para_comprar_comida != "sim" ~ 0),
    x9_nos_ultimos_tres_meses_algum_morador_com_menos_de_18_anos_de_idade_alguma_vez_deixou_de_ter_uma_alimentacao_saudavel_e_variada_porque_nao_havia_dinheiro_para_comprar_comida = case_when(x9_nos_ultimos_tres_meses_algum_morador_com_menos_de_18_anos_de_idade_alguma_vez_deixou_de_ter_uma_alimentacao_saudavel_e_variada_porque_nao_havia_dinheiro_para_comprar_comida == "sim" ~ 1,
                                                                                                                                                                                                x9_nos_ultimos_tres_meses_algum_morador_com_menos_de_18_anos_de_idade_alguma_vez_deixou_de_ter_uma_alimentacao_saudavel_e_variada_porque_nao_havia_dinheiro_para_comprar_comida != "sim" ~ 0),
    x10_nos_ultimos_tres_meses_algum_morador_com_menos_de_18_anos_de_idade_alguma_vez_nao_comeu_quantidade_suficiente_de_comida_porque_nao_havia_dinheiro_para_comprar_comida = case_when(x10_nos_ultimos_tres_meses_algum_morador_com_menos_de_18_anos_de_idade_alguma_vez_nao_comeu_quantidade_suficiente_de_comida_porque_nao_havia_dinheiro_para_comprar_comida == "sim" ~ 1,
                                                                                                                                                                                          x10_nos_ultimos_tres_meses_algum_morador_com_menos_de_18_anos_de_idade_alguma_vez_nao_comeu_quantidade_suficiente_de_comida_porque_nao_havia_dinheiro_para_comprar_comida != "sim" ~ 0),
    x11_nos_ultimos_tres_meses_alguma_vez_foi_diminuida_a_quantidade_de_alimentos_das_refeicoes_de_algum_morador_com_menos_de_18_anos_de_idade_porque_nao_havia_dinheiro_para_comprar_comida = case_when(x11_nos_ultimos_tres_meses_alguma_vez_foi_diminuida_a_quantidade_de_alimentos_das_refeicoes_de_algum_morador_com_menos_de_18_anos_de_idade_porque_nao_havia_dinheiro_para_comprar_comida == "sim" ~ 1,
                                                                                                                                                                                                         x11_nos_ultimos_tres_meses_alguma_vez_foi_diminuida_a_quantidade_de_alimentos_das_refeicoes_de_algum_morador_com_menos_de_18_anos_de_idade_porque_nao_havia_dinheiro_para_comprar_comida != "sim" ~ 0),
    x12_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_deixou_de_fazer_alguma_refeicao_porque_nao_havia_dinheiro_para_comprar_comida = case_when(x12_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_deixou_de_fazer_alguma_refeicao_porque_nao_havia_dinheiro_para_comprar_comida == "sim" ~ 1,
                                                                                                                                                                                x12_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_deixou_de_fazer_alguma_refeicao_porque_nao_havia_dinheiro_para_comprar_comida != "sim" ~ 0),
    x13_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_sentiu_fome_mas_nao_comeu_porque_nao_havia_dinheiro_para_comprar_comida = case_when(x13_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_sentiu_fome_mas_nao_comeu_porque_nao_havia_dinheiro_para_comprar_comida == "sim" ~ 1,
                                                                                                                                                                          x13_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_sentiu_fome_mas_nao_comeu_porque_nao_havia_dinheiro_para_comprar_comida != "sim" ~ 0),
    x14_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_fez_apenas_uma_refeicao_ao_dia_ou_ficou_sem_comer_por_um_dia_inteiro_porque_nao_havia_dinheiro_para_comprar_comida = case_when(x14_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_fez_apenas_uma_refeicao_ao_dia_ou_ficou_sem_comer_por_um_dia_inteiro_porque_nao_havia_dinheiro_para_comprar_comida == "sim" ~ 1,
                                                                                                                                                                                                                     x14_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_fez_apenas_uma_refeicao_ao_dia_ou_ficou_sem_comer_por_um_dia_inteiro_porque_nao_havia_dinheiro_para_comprar_comida != "sim" ~ 0)
  ) |>
  mutate(
    x1_nos_ultimos_tres_meses_os_moradores_deste_domicilio_tiveram_preocupacao_de_que_os_alimentos_acabassem_antes_de_poderem_comprar_ou_receber_mais_comida = as.integer(x1_nos_ultimos_tres_meses_os_moradores_deste_domicilio_tiveram_preocupacao_de_que_os_alimentos_acabassem_antes_de_poderem_comprar_ou_receber_mais_comida),
    x2_nos_ultimos_tres_meses_os_alimentos_acabaram_antes_que_os_moradores_deste_domicilio_tivessem_dinheiro_para_comprar_mais_comida = as.integer(x2_nos_ultimos_tres_meses_os_alimentos_acabaram_antes_que_os_moradores_deste_domicilio_tivessem_dinheiro_para_comprar_mais_comida),
    x3_nos_ultimos_tres_meses_os_moradores_deste_domicilio_ficaram_sem_dinheiro_para_ter_uma_alimentacao_saudavel_e_variada = as.integer(x3_nos_ultimos_tres_meses_os_moradores_deste_domicilio_ficaram_sem_dinheiro_para_ter_uma_alimentacao_saudavel_e_variada),
    x4_nos_ultimos_tres_meses_os_moradores_deste_domicilio_comeram_apenas_alguns_alimentos_que_ainda_tinham_porque_o_dinheiro_acabou = as.integer(x4_nos_ultimos_tres_meses_os_moradores_deste_domicilio_comeram_apenas_alguns_alimentos_que_ainda_tinham_porque_o_dinheiro_acabou),
    x5_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_deixou_de_fazer_uma_refeicao_porque_nao_havia_dinheiro_para_comprar_comida = as.integer(x5_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_deixou_de_fazer_uma_refeicao_porque_nao_havia_dinheiro_para_comprar_comida),
    x6_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_comeu_menos_do_que_devia_porque_nao_havia_dinheiro_para_comprar_comida = as.integer(x6_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_comeu_menos_do_que_devia_porque_nao_havia_dinheiro_para_comprar_comida),
    x7_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_sentiu_fome_mas_nao_comeu_porque_nao_havia_dinheiro_para_comprar_comida = as.integer(x7_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_sentiu_fome_mas_nao_comeu_porque_nao_havia_dinheiro_para_comprar_comida),
    x8_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_fez_apenas_uma_refeicao_ao_dia_ou_ficou_um_dia_inteiro_sem_comer_porque_nao_havia_dinheiro_para_comprar_comida = as.integer(x8_nos_ultimos_tres_meses_algum_morador_de_18_anos_ou_mais_de_idade_alguma_vez_fez_apenas_uma_refeicao_ao_dia_ou_ficou_um_dia_inteiro_sem_comer_porque_nao_havia_dinheiro_para_comprar_comida),
    x9_nos_ultimos_tres_meses_algum_morador_com_menos_de_18_anos_de_idade_alguma_vez_deixou_de_ter_uma_alimentacao_saudavel_e_variada_porque_nao_havia_dinheiro_para_comprar_comida = as.integer(x9_nos_ultimos_tres_meses_algum_morador_com_menos_de_18_anos_de_idade_alguma_vez_deixou_de_ter_uma_alimentacao_saudavel_e_variada_porque_nao_havia_dinheiro_para_comprar_comida),
    x10_nos_ultimos_tres_meses_algum_morador_com_menos_de_18_anos_de_idade_alguma_vez_nao_comeu_quantidade_suficiente_de_comida_porque_nao_havia_dinheiro_para_comprar_comida = as.integer(x10_nos_ultimos_tres_meses_algum_morador_com_menos_de_18_anos_de_idade_alguma_vez_nao_comeu_quantidade_suficiente_de_comida_porque_nao_havia_dinheiro_para_comprar_comida),
    x11_nos_ultimos_tres_meses_alguma_vez_foi_diminuida_a_quantidade_de_alimentos_das_refeicoes_de_algum_morador_com_menos_de_18_anos_de_idade_porque_nao_havia_dinheiro_para_comprar_comida = as.integer(x11_nos_ultimos_tres_meses_alguma_vez_foi_diminuida_a_quantidade_de_alimentos_das_refeicoes_de_algum_morador_com_menos_de_18_anos_de_idade_porque_nao_havia_dinheiro_para_comprar_comida),
    x12_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_deixou_de_fazer_alguma_refeicao_porque_nao_havia_dinheiro_para_comprar_comida = as.integer(x12_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_deixou_de_fazer_alguma_refeicao_porque_nao_havia_dinheiro_para_comprar_comida),
    x13_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_sentiu_fome_mas_nao_comeu_porque_nao_havia_dinheiro_para_comprar_comida = as.integer(x13_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_sentiu_fome_mas_nao_comeu_porque_nao_havia_dinheiro_para_comprar_comida),
    x14_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_fez_apenas_uma_refeicao_ao_dia_ou_ficou_sem_comer_por_um_dia_inteiro_porque_nao_havia_dinheiro_para_comprar_comida = as.integer(x14_nos_ultimos_tres_meses_alguma_vez_algum_morador_com_menos_de_18_anos_de_idade_fez_apenas_uma_refeicao_ao_dia_ou_ficou_sem_comer_por_um_dia_inteiro_porque_nao_havia_dinheiro_para_comprar_comida)
  ) |>
  # ajuste devido o escore de ansiedade estar vindo de 1 a 4 e não 0 a 3 como deveria!
  mutate(
        inventario_de_ansiedade_de_beck_demencia_ou_formigamento = inventario_de_ansiedade_de_beck_demencia_ou_formigamento - 1,
        inventario_de_ansiedade_de_beck_sensacao_de_calor = inventario_de_ansiedade_de_beck_sensacao_de_calor - 1,
        inventario_de_ansiedade_de_beck_tremores_nas_pernas = inventario_de_ansiedade_de_beck_tremores_nas_pernas - 1,
        inventario_de_ansiedade_de_beck_incapaz_de_relaxar = inventario_de_ansiedade_de_beck_incapaz_de_relaxar - 1,
        inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior = inventario_de_ansiedade_de_beck_medo_que_aconteca_o_pior - 1,
        inventario_de_ansiedade_de_beck_atordoado_ou_tonto = inventario_de_ansiedade_de_beck_atordoado_ou_tonto - 1,
        inventario_de_ansiedade_de_beck_palpitacao_ou_aceleracao_do_coracao = inventario_de_ansiedade_de_beck_palpitacao_ou_aceleracao_do_coracao -1,
        inventario_de_ansiedade_de_beck_sem_equilibrio = inventario_de_ansiedade_de_beck_sem_equilibrio -1,
        inventario_de_ansiedade_de_beck_aterrorizado = inventario_de_ansiedade_de_beck_aterrorizado -1,
        inventario_de_ansiedade_de_beck_nervoso = inventario_de_ansiedade_de_beck_nervoso -1,
        inventario_de_ansiedade_de_beck_sensacao_de_sufocacao = inventario_de_ansiedade_de_beck_sensacao_de_sufocacao -1,
        inventario_de_ansiedade_de_beck_tremores_nas_maos = inventario_de_ansiedade_de_beck_tremores_nas_maos -1,
        inventario_de_ansiedade_de_beck_tremulo = inventario_de_ansiedade_de_beck_tremulo -1,
        inventario_de_ansiedade_de_beck_medo_de_perder_o_controle = inventario_de_ansiedade_de_beck_medo_de_perder_o_controle -1,
        inventario_de_ansiedade_de_beck_dificuldade_para_respirar = inventario_de_ansiedade_de_beck_dificuldade_para_respirar -1,
        inventario_de_ansiedade_de_beck_medo_de_morrer = inventario_de_ansiedade_de_beck_medo_de_morrer -1,
        inventario_de_ansiedade_de_beck_assustado = inventario_de_ansiedade_de_beck_assustado -1,
        inventario_de_ansiedade_de_beck_indisgestao_ou_desconforto_no_abdome = inventario_de_ansiedade_de_beck_indisgestao_ou_desconforto_no_abdome - 1,
        inventario_de_ansiedade_de_beck_sensacao_de_desmaio = inventario_de_ansiedade_de_beck_sensacao_de_desmaio - 1,
        inventario_de_ansiedade_de_beck_rosto_afogueado = inventario_de_ansiedade_de_beck_rosto_afogueado - 1,
        inventario_de_ansiedade_de_beck_suor_nao_devido_ao_calor = inventario_de_ansiedade_de_beck_suor_nao_devido_ao_calor - 1
  )

# Calculo das variáveis -----------------------------------------------------------------------
### idade
### IMC
### ICQ
### ebia score e ebia categorico
### ansiedade score e ebia categorico
### depressao score e ebia categorico

df_ajustado <-
  df_ajustado |>
  # calcular idade
  mutate(
    idade = 2025 - year(data_nasc),
  # calcular IMC
    imc = peso/((round(estatura)/100)^2),
  # encontrar maior valor obtido da circunferencia da cintura
    circ_cintura = apply(df_ajustado[, 23:25], MARGIN = 1,FUN = max),
  # encontrar maior valor obtido da circunferencia da quadril
    circ_quadril = apply(df_ajustado[, 26:28], MARGIN = 1,FUN = max),
  # calculo do Indice cintura quadril - ICQ
    icq = round(circ_cintura/circ_quadril,digits = 2),
  # insegurança alimentar - soma dos scores (1 ou 0 para cada uma das 14 perguntas)
    ebia =  apply(df_ajustado[, 7:20], MARGIN = 1,FUN = sum),
  # insegurança alimentar - Categorico
    ebia_class = case_when(
      ebia == 0 ~ "SA",
      ebia > 0 & ebia <= 3 ~ "IL",
      ebia >= 4 & ebia <= 5 ~ "IM",
      ebia >= 6 & ebia <= 8 ~ "IG"
    ),
  # score de ansiedade
    ansiedade =  apply(df_ajustado[, 29:49], MARGIN = 1,FUN = sum),
  # ansiedade - categorica
  ansiedade_cat = case_when(
    ansiedade <  08 ~ "Minimal",
    ansiedade >= 08 & ansiedade < 16 ~ "Mild",
    ansiedade >= 16 & ansiedade < 26 ~ "Moderate",
    ansiedade >= 26 ~ "Severe"
  ),
  # score de depressao
    depressao =  apply(df_ajustado[, 50:70], MARGIN = 1,FUN = sum),
  # depressao categorica
    depressao_cat = case_when(
      depressao <  14 ~ "Minimal",
      depressao >= 14 & depressao < 20 ~ "Mild",
      depressao >= 20 & depressao < 29 ~ "Moderate",
      depressao >= 29 ~ "Severe"
    )
  ) |>
  # remove colunas que nao serao usadas
  select(-data_nasc,
         -circ_cintura_1,
         -circ_cintura_2,
         -circ_cintura_3,
         -circ_quadril_1,
         -circ_quadril_2,
         -circ_quadril_3,
         -starts_with(match = "x"),
         -starts_with(match = "inventario_de_ansiedade"),
         -starts_with(match = "inventario_de_depressao")
         )

# calculo dos WHOQoL --------------------------------------------------------------------------
df_ajustado <-
  df_ajustado |>
  # renomeando para facilitar o calculo dos scores da qualidade de vida
  mutate(
    who_qol1 = the_world_health_organization_quality_of_life_whoqol_bref_como_voce_avaliaria_sua_qualidade_de_vida,
    who_qol2 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_a_sua_saude,
    who_qol3 = the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_sua_dor_fisica_impede_voce_de_fazer_o_que_voce_precisa,
    who_qol4 = the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_precisa_de_algum_tratamento_medico_para_levar_sua_vida_diaria,
    who_qol5 = the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_aproveita_a_vida,
    who_qol6 = the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_acha_que_a_sua_vida_tem_sentido,
    who_qol7 = the_world_health_organization_quality_of_life_whoqol_bref_o_quanto_voce_consegue_se_concentrar,
    who_qol8 = the_world_health_organization_quality_of_life_whoqol_bref_quao_seguro_a_voce_se_sente_em_sua_vida_diaria,
    who_qol9 = the_world_health_organization_quality_of_life_whoqol_bref_quao_saudavel_e_o_seu_ambiente_fisico_clima_barulho_poluicao_atrativos,
    who_qol10 = the_world_health_organization_quality_of_life_whoqol_bref_voce_tem_energia_suficiente_para_seu_dia_a_dia,
    who_qol11 = the_world_health_organization_quality_of_life_whoqol_bref_voce_e_capaz_de_aceitar_sua_aparencia_fisica,
    who_qol12 = the_world_health_organization_quality_of_life_whoqol_bref_voce_tem_dinheiro_suficiente_para_satisfazer_suas_necessidades,
    who_qol13 = the_world_health_organization_quality_of_life_whoqol_bref_quao_disponiveis_para_voce_estao_as_informacoes_que_precisa_no_seu_dia_a_dia,
    who_qol14 = the_world_health_organization_quality_of_life_whoqol_bref_em_que_medida_voce_tem_oportunidades_de_atividade_de_lazer,
    who_qol15 = the_world_health_organization_quality_of_life_whoqol_bref_quao_bem_voce_e_capaz_de_se_locomover,
    who_qol16 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_o_seu_sono,
    who_qol17 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_capacidade_de_desempenhar_as_atividades_do_seu_dia_a_dia,
    who_qol18 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_capacidade_para_o_trabalho,
    who_qol19 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_consigo_mesmo,
    who_qol20 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_suas_relacoes_pessoais_amigos_parentes_conhecidos_colegas,
    who_qol21 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_sua_vida_sexual,
    who_qol22 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_o_apoio_que_voce_recebe_de_seus_amigos,
    who_qol23 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_as_condicoes_do_local_onde_mora,
    who_qol24 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_o_seu_acesso_aos_servicos_de_saude,
    who_qol25 = the_world_health_organization_quality_of_life_whoqol_bref_quao_satisfeito_a_voce_esta_com_o_seu_meio_de_transporte,
    who_qol26 = the_world_health_organization_quality_of_life_whoqol_bref_com_que_frequencia_voce_tem_sentimentos_negativos_tais_como_mau_humor_desespero_ansiedade_depressao
  ) |>
  select(-starts_with(match = "the_world_health"))|>
  # ajustando as questões do WHOQoL
  mutate(who_qol3 = case_when(who_qol3 == 1 ~ 5,
                              who_qol3 == 2 ~ 4,
                              who_qol3 == 3 ~ 3,
                              who_qol3 == 4 ~ 2,
                              who_qol3 == 5 ~ 1),
         who_qol4 = case_when(who_qol4 == 1 ~ 5,
                              who_qol4 == 2 ~ 4,
                              who_qol4 == 3 ~ 3,
                              who_qol4 == 4 ~ 2,
                              who_qol4 == 5 ~ 1),
         who_qol26 = case_when(who_qol26 == 1 ~ 5,
                               who_qol26 == 2 ~ 4,
                               who_qol26 == 3 ~ 3,
                               who_qol26 == 4 ~ 2,
                               who_qol26 == 5 ~ 1))
### Calculando os dominios
# físico
whoqol_fisico <-
  df_ajustado |>
  select(who_qol3,
         who_qol4,
         who_qol10,
         who_qol15,
         who_qol6,
         who_qol17,
         who_qol18)

whoqol_fisico <-
  whoqol_fisico |>
  mutate(whoqol_fisico = apply(whoqol_fisico[,1:6],MARGIN = 1,FUN = mean)*4) |>
  select(whoqol_fisico)

# verificando os valores - range 0-20
min(whoqol_fisico, na.rm = TRUE)
max(whoqol_fisico, na.rm = TRUE)

# psicologico
whoqol_psicol <-
  df_ajustado |>
  select(who_qol5,
         who_qol6,
         who_qol7,
         who_qol11,
         who_qol19,
         who_qol26)


whoqol_psicol <-
  whoqol_psicol |>
  mutate(whoqol_psicol = apply(whoqol_psicol[,1:6],MARGIN = 1,FUN = mean)*4) |>
  select(whoqol_psicol)

# verificando os valores - range 0-20
min(whoqol_psicol, na.rm = TRUE)
max(whoqol_psicol, na.rm = TRUE)

# Relações sociais
whoqol_social <-
  df_ajustado |>
  select(who_qol20,
         who_qol21,
         who_qol22)

whoqol_social <-
  whoqol_social |>
  mutate(whoqol_social = apply(whoqol_social[,1:3],MARGIN = 1,FUN = mean)*4) |>
  select(whoqol_social)

# verificando os valores - range 0-20
min(whoqol_social, na.rm = TRUE)
max(whoqol_social, na.rm = TRUE)

# Meio ambiente
whoqol_ambiente <-
  df_ajustado |>
  select(who_qol8,
         who_qol9,
         who_qol12,
         who_qol13,
         who_qol14,
         who_qol23,
         who_qol24,
         who_qol25)

whoqol_ambiente <-
  whoqol_ambiente |>
  mutate(whoqol_ambiente = apply(whoqol_ambiente[,1:3],MARGIN = 1,FUN = mean)*4) |>
  select(whoqol_ambiente)

# verificando os valores - range 0-20
min(whoqol_ambiente, na.rm = TRUE)
max(whoqol_ambiente, na.rm = TRUE)

# Juntando os dominios do WHOQol com a base ---------------------------------------------------
#juntado os dominios
fis_psi <- bind_cols(whoqol_fisico, whoqol_psicol)
fis_psi_soc <- bind_cols(fis_psi,whoqol_social)
todos_dominios <- bind_cols(fis_psi_soc,whoqol_ambiente)

# transformar scores em escala de 0-100
todos_dominios <-
  todos_dominios |>
  mutate(
    whoqol_fisico_escore_100 = (whoqol_fisico - 4)*(100/16),
    whoqol_psicol_escore_100 = (whoqol_psicol - 4)*(100/16),
    whoqol_social_escore_100 = (whoqol_social - 4)*(100/16),
    whoqol_ambiente_escore_100 = (whoqol_ambiente - 4)*(100/16),
  )

# Juntando os dominios do WHOQoL com a base e removendo as colunas isoladas
df_ajustado_final <-
  bind_cols(df_ajustado, todos_dominios) |>
  select(-starts_with(match = "who_"))

# Tratando os missing -------------------------------------------------------------------------
# Verifricando os missing
DataExplorer::plot_missing(df_ajustado_final)

# Imputação using pmm
tempData <- mice::mice(df_ajustado_final,m=5,maxit=52,method='pmm',seed=500)

# Base imputada
df <- complete(tempData,1)
DataExplorer::plot_missing(df)
glimpse(df)

# tratando os missings
df <-
  df |>
  mutate(
    # ansiedade - categorica
    ansiedade_cat = case_when(
      ansiedade <  08 ~ "Minimal",
      ansiedade >= 08 & ansiedade < 16 ~ "Mild",
      ansiedade >= 16 & ansiedade < 26 ~ "Moderate",
      ansiedade >= 26 ~ "Severe"
    ),
    # depressao categorica
    depressao_cat = case_when(
      depressao <  14 ~ "Minimal",
      depressao >= 14 & depressao < 20 ~ "Mild",
      depressao >= 20 & depressao < 29 ~ "Moderate",
      depressao >= 29 ~ "Severe"
    ),
    # insegurança alimentar - Categorico
    ebia_class = case_when(
      ebia == 0 ~ "SA",
      ebia > 0 & ebia <= 3 ~ "IL",
      ebia >= 4 & ebia <= 5 ~ "IM",
      ebia >= 6 & ebia <= 8 ~ "IG",
      ebia >= 9 ~ "IG"
    ),
    # substituindo pelo valor mais frequente
    estado_civil = case_when(
      is.na(estado_civil) ~ "casado",
      .default = as.character(estado_civil)
    ),
    # substituindo pelo valor mais frequente
    raca = case_when(
      is.na(raca) ~ "branco",
      .default = as.character(raca)
    ),
    # substituindo pelo valor mais frequente
    genero = case_when(
      is.na(genero) ~ "feminino",
      .default = as.character(genero)
    ),
    # substituindo pelo valor mais frequente
    ebia = case_when(
      is.na(ebia) ~ "branco",
      .default = as.character(ebia)
    ),
    # substituindo pelo valor mais frequente
    renda_familiar = case_when(
      is.na(renda_familiar_considere_a_renda_de_todas_as_pessoas_que_moral_na_sua_casa) ~ "ate 1 salario minimo",
      .default = as.character(renda_familiar_considere_a_renda_de_todas_as_pessoas_que_moral_na_sua_casa)))

# verificando os missings
DataExplorer::plot_missing(df)

# Arrumando a coluna IMC ----------------------------------------------------------------------
df <-
  df |>
  filter(idade >= 18) |>
  mutate(
    peso = case_when(peso == 0 ~ mean(peso), .default = as.double(peso)),
    estatura = case_when(estatura < 50 ~ mean(estatura), .default = as.double(estatura)),
    imc = peso / ((round(estatura) / 100) ^ 2)
  ) |>
  mutate(
    whoqol_fisico_escore_100 = case_when(is.na(whoqol_fisico_escore_100) ~ mean(whoqol_fisico_escore_100, na.rm = TRUE), .default = as.numeric(whoqol_fisico_escore_100)),
    whoqol_psicol_escore_100 = case_when(is.na(whoqol_psicol_escore_100) ~ mean(whoqol_psicol_escore_100, na.rm = TRUE), .default = as.numeric(whoqol_psicol_escore_100)),
    whoqol_social_escore_100 = case_when(is.na(whoqol_social_escore_100) ~ mean(whoqol_social_escore_100, na.rm = TRUE), .default = as.numeric(whoqol_social_escore_100)),
    whoqol_ambiente_escore_100 = case_when(is.na(whoqol_ambiente_escore_100) ~ mean(whoqol_ambiente_escore_100, na.rm = TRUE), .default = as.numeric(whoqol_ambiente_escore_100))
  ) |>
  # renda_familiar_considere_a_renda_de_todas_as_pessoas_que_moral_na_sua_casa
  select(
    -renda_familiar_considere_a_renda_de_todas_as_pessoas_que_moral_na_sua_casa,
    -whoqol_fisico,
    -whoqol_psicol,
    -whoqol_social,
    -whoqol_ambiente)

# verificando os missings
DataExplorer::plot_missing(df)

# visualizando a base final -------------------------------------------------------------------
glimpse(df)

# tabela para analise -------------------------------------------------------------------------
write_rds(x = df,file =  "df_para_analise.rds") # tirar o comentário para salvar
# write_csv2(x = df,file =  "dfParaAnaliseCris.csv") # tirar o comentário para salvar
