# Vacinas no município de São Paulo --------------------------------------------
#
# Objetivo: Análise da disponibilidade de vacinas nos postos de saúde ----------
#
# Inspirado no código de Ana Carolina Moreno
# Realizado por: Haydee Svab em 30/08/2022
# ------------------------------------------------------------------------------

# SETUP INICIAL ----------------------------------------------------------------

# Pacotes

# install.packages("tidyverse")
library(tidyverse)

# Verificar / setar working directory
setwd("/cloud/project/Aula4_files")

# LEITURA DE ARQUIVOS-----------------------------------------------------------
df_vacinas <- read.csv("data/vacinas-modificado.csv")

# COMPREENSÃO DO ARQUIVO -------------------------------------------------------

# Para abrir o dataframe
View(df_vacinas)

# Para ver um resumo do dataframe
df_vacinas %>% glimpse()

# Também pode ser assim:
glimpse(df_vacinas)

# MANIPULAÇÃO COM FILTER E SELECT ----------------------------------------------

# Para saber os nomes das variáveis
names(df_vacinas)

# Para ver as categorias da variável status_fila
levels(as.factor(df_vacinas$status_fila))

# Retornar nome e endereço dos postos volantes, megapostos e postos em parque que não estão funcionando
df_postos_funcionando <- df_vacinas %>%
  filter((tipo_posto == "PARQUES" |
           tipo_posto == "MEGAPOSTO" |
           tipo_posto == "POSTO VOLANTE") &
           (status_fila == "NÃO FUNCIONANDO")) %>% 
  select(equipamento, endereco)
df_postos_funcionando

# MANIPULAÇÃO COM IF_ELSE e CASE_WHEN ----------------------------------------------

# Criando a variável falta_coronavac a partir de análise condicional com if_else
df_vacinas_tratado <- df_vacinas %>%
  mutate(falta_coronavac = if_else(
    coronavac == 0,
    "postos_SEM_coronavac",
    "postos_COM_coronavac",
    "sem informação"))

# Criando a variável falta_coronavac a partir de análise condicional com case_when
df_vacinas_tratado <- df_vacinas_tratado %>%
  mutate(falta_pfizer = case_when(
    pfizer == 0 ~ "postos_SEM_pfizer",
    pfizer == 1 ~ "postos_COM_pfizer",
    TRUE ~ "sem informação"))

# MANIPULAÇÃO COM RENAME E MUTATE ----------------------------------------------

# Para ver as categorias da variável astrazeneca
levels(as.factor(df_vacinas$astrazeneca))

# Para criar nova coluna "falta_az" indicando se há ou não a vacina aztrazeneca disponível
df_vacinas_tratado <- df_vacinas_tratado %>%
  mutate(falta_az = case_when(
    # A variável falta_az recebe texto "postos_SEM_astrazeneca" se a variável astrazeneca for igual a zero
    astrazeneca == 0 ~ "postos_SEM_astrazeneca",
    # A variável falta_az recebe texto "postos_COM_astrazeneca" se a variável astrazeneca for igual a um
    astrazeneca == 1 ~ "postos_COM_astrazeneca",
    T ~ "sem informação"))

# O nome falta_az ficou meio obscuro, vamos renomear para falta_astrazeneca
df_vacinas_tratado <- df_vacinas_tratado %>%
  rename (falta_astrazeneca = falta_az)

# MANIPULAÇÃO COM GROUP_BY E SUMMARISE -----------------------------------------

# Cria um df específico para esta análise
df_postos_por_distrito <- df_vacinas_tratado %>%
  # filtra apenas as informações de postos que estão funcionando
  filter(status_fila != "NÃO FUNCIONANDO") %>%
  # cria uma variável que sinaliza se a observação corresponde a um equipamento
  # existente (excluindo campos nulos)
  mutate(equipamento_existe = case_when(
    !is.na(equipamento) ~ 1,
    T ~ 0))

# Determina o total de postos por distrito
df_postos_por_distrito <- df_postos_por_distrito %>%
  # agrupa por distritos
  group_by(distrito) %>%
  # e faz a soma
  summarise(qtde_postos_total_por_distrito = sum(equipamento_existe)) 


# DESAFIO ----------------------------------------------------------------------

# 1) Crie nova coluna "falta_janssen" indicando se há ou não a vacina janssen disponível
df_vacinas_tratado <- df_vacinas_tratado %>%
  mutate(falta_janssen = case_when(
    janssen == 0 ~ "postos_SEM_janssen",
    janssen == 1 ~ "postos_COM_janssen",
    T ~ "sem informação"))

# 2) Calcular o total de postos por distrito com janssen
df_postos_com_janssen_por_distrito <- df_vacinas_tratado %>%
  filter(status_fila != "NÃO FUNCIONANDO") %>%
  mutate(equipamento_existe = case_when(
    !is.na(equipamento) ~ 1,
    T ~ 0)) %>% 
  # agrupa por distritos e por falta_janssen
  group_by(distrito, falta_janssen) %>%
  # e faz a soma
  summarise(qtde_postos_com_janssen_por_distrito = sum(equipamento_existe)) %>% 
  # filtra apenas os postos que têm janssen
  filter(falta_janssen == "postos_COM_janssen")

# 3) Calcular a porcentagem de postos com janssen, por distrito
df_postos_com_janssen_por_distrito <- df_postos_com_janssen_por_distrito  %>% 
  right_join(df_postos_por_distrito, by = "distrito") %>% 
  mutate(porcentagem_janssen = (qtde_postos_com_janssen_por_distrito/qtde_postos_total_por_distrito)*100,
         porcentagem_janssen = if_else(is.na(porcentagem_janssen), 0, porcentagem_janssen)) %>%
  select(distrito, porcentagem_janssen)