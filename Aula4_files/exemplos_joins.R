# Estados Top 10 População e IDH -----------------------------------------------
#
# Realizado por: Haydee Svab em 15/08/2022
# Realizado por: Haydee Svab em 31/08/2022
# ------------------------------------------------------------------------------

# SETUP INICIAL ----------------------------------------------------------------

# Pacotes

# install.packages("tidyverse")
library(tidyverse)

# Verificar / setar working directory
setwd("/cloud/project/Aula4_files")

# LEITURA DE ARQUIVOS-----------------------------------------------------------
df_populacao <- read.csv("data/populacao.csv")
df_idhm <- read.csv("data/idhm.csv")

# LEFT_JOIN --------------------------------------------------------------------
df_resultante_left <- df_populacao %>% 
  left_join(df_idhm, by = "UF")

# RIGHT_JOIN -------------------------------------------------------------------
df_resultante_right <- df_populacao %>% 
  right_join(df_idhm, by = "UF")

# INNER_JOIN -------------------------------------------------------------------
df_resultante_inner <- df_populacao %>% 
  inner_join(df_idhm, by = "UF")

# FULL_JOIN -------------------------------------------------------------------
df_resultante_full <- df_populacao %>% 
  full_join(df_idhm, by = "UF")
