# Eleições 2022 ----------------------------------------------------------------
#
# Objetivo: Cruzar dados de bens com informações de perfil de candidatos(as)
#
# Criado por: Haydee Svab em 01/09/2022
# Atualizado por: Haydee Svab em 12/09/2022
# ------------------------------------------------------------------------------

# SETUP INICIAL ----------------------------------------------------------------

# Carrega pacote tidyverse
library(tidyverse)

# Configura diretório de trabalho
setwd("/cloud/project/Aula5_files")

# LEITURA DE ARQUIVOS-----------------------------------------------------------

# Arquivo com todas informações de candidatos(as) de São Paulo
info_candidatos_sp <- data.table::fread(file = "./data/consulta_cand_2022_SP.csv",
                                             sep = ";",
                                             dec = ",",
                                             encoding = "Latin-1")


# Arquivo com todas informações de bens candidatos(as) de São Paulo
info_bens_candidatos_sp <- data.table::fread(file = "./data/bem_candidato_2022_SP.csv",
                                           sep = ";",
                                           dec = ",",
                                           encoding = "Latin-1")

# TRATAMENTO DE DADOS ----------------------------------------------------------

# Total de bens por candidato(a)
df_total_bens_por_candidato <- info_bens_candidatos_sp %>%
  group_by(SQ_CANDIDATO) %>% 
  summarise(VR_BEM_CANDIDATO = sum(VR_BEM_CANDIDATO))

# Junção da informação de bens na tabela de candidatos
df_candidatos_com_bens <- info_candidatos_sp %>%
  left_join(df_total_bens_por_candidato, by = "SQ_CANDIDATO")

# Cálculo de estatística básicas (mínimo, máximo, média, mediana, variância e
# desvio padrão) dos bens declarados
resumo_sp <- df_candidatos_com_bens %>% 
  summarise(minimo = min(VR_BEM_CANDIDATO, na.rm = TRUE),
            maximo = max(VR_BEM_CANDIDATO, na.rm = TRUE),
            media = mean(VR_BEM_CANDIDATO, na.rm = TRUE),
            mediana = median(VR_BEM_CANDIDATO, na.rm = TRUE),
            variancia = var(VR_BEM_CANDIDATO, na.rm = TRUE),
            desvio_padrao = sd(VR_BEM_CANDIDATO, na.rm = TRUE)) %>% 
  mutate(UF = "SP")

# Visualizção de outliers utilizando boxplot (diagrama de caixa) para variável "idade na posse"
boxplot(df_candidatos_com_bens$NR_IDADE_DATA_POSSE)

  
# DESAFIO ----------------------------------------------------------------------

# 1) Junte as seguintes informações de candidatos, do Acre, na tabela de bens

# Arquivo com todas informações de candidatos(as) do Acre
info_candidatos_ac <- data.table::fread(file = "./data/consulta_cand_2022_AC.csv",
                                        sep = ";",
                                        dec = ",",
                                        encoding = "Latin-1")

# Arquivo com todas informações de bens candidatos(as) de São Paulo
info_bens_candidatos_ac <- data.table::fread(file = "./data/bem_candidato_2022_AC.csv",
                                             sep = ";",
                                             dec = ",",
                                             encoding = "Latin-1")

# Junção de algumas informações de candidatos(as) na tabela de bens
info_bens_candidatos_ac <- info_candidatos_ac %>%
  select(DS_CARGO, SQ_CANDIDATO, NR_CANDIDATO, NM_CANDIDATO, NM_URNA_CANDIDATO,
         NM_SOCIAL_CANDIDATO, DS_SITUACAO_CANDIDATURA, SG_PARTIDO, NR_IDADE_DATA_POSSE,
         DS_GENERO, DS_GRAU_INSTRUCAO, DS_ESTADO_CIVIL, DS_COR_RACA, DS_OCUPACAO) %>% 
  right_join(info_bens_candidatos_ac, by = "SQ_CANDIDATO")

# 2) Calcule estatísticas básicas (mínimo, máximo, média, mediana, variância e
# desvio padrão) dos bens declarados de candidatos(as) do Acre

# Cálculo de estatística básicas (mínimo, máximo, média, mediana, variância e
# desvio padrão) dos bens declarados
resumo_ac <- info_bens_candidatos_ac %>% 
  summarise(minimo = min(VR_BEM_CANDIDATO, na.rm = TRUE),
            maximo = max(VR_BEM_CANDIDATO, na.rm = TRUE),
            media = mean(VR_BEM_CANDIDATO, na.rm = TRUE),
            mediana = median(VR_BEM_CANDIDATO, na.rm = TRUE),
            variancia = var(VR_BEM_CANDIDATO, na.rm = TRUE),
            desvio_padrao = sd(VR_BEM_CANDIDATO, na.rm = TRUE)) %>% 
  mutate(UF = "AC")

# 3) Gere um arquivo resumo-estatisticas.csv de saída com as informações de 
# São Paulo e Acre numa tabela só

# Junção de linhas que têm colunas idênticas
resumo <- bind_rows(resumo_ac, resumo_sp)

# Geração de arquivo de saída do tipo csv
data.table::fwrite(x = resumo, file = "data/resumo-estatisticas.csv")

