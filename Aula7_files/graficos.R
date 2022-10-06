# Eleições 2022 ----------------------------------------------------------------
#
# Objetivo: Cruzar dados de bens com informações de perfil de candidatos(as)
#
# Criado por: Haydee Svab em 19/09/2022
# Atualizado por: Haydee Svab em 20/09/2022
# ------------------------------------------------------------------------------

# SETUP INICIAL ----------------------------------------------------------------

# Instala pacote ggplot2
install.packages("ggplot2")

# Carrega pacote ggplot2
library(ggplot2)

# Configura diretório de trabalho
setwd("/cloud/project/Aula6_files")

# CARREGANDO DATASETS ----------------------------------------------------------

# Lista de datasets de todos os pacotes carregados
data()

# Lista de datasets de todos os pacotes instalados, mesmo não carregados
data(package = .packages(all.available = TRUE))

# Lista de datasets de um pacote específico
data(package = "dplyr")

# TRABALHANDO COM GGPLOT -------------------------------------------------------

# Histograma--------------------------------------------------------------------

# Carga da base de dados
diamantes <- ggplot2::diamonds

# Gráfico com ggplot
ggplot(data = diamantes, aes(x=price)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Preço (em dolares)", y = "Frequência", title = "Distribuição dos valores dos diamantes")

# Gráfico de barras ------------------------------------------------------------

# Gráfico com ggplot - geom_bar()

# Carga da base de dados
library(MASS)
carros <- Cars93

# Fazer um gráfico de barras dos tipos de carro (Type), usando a base de dados 
# `Cars93`, disponível no pacote MASS,
ggplot(data = carros, aes(x = Type)) +
  geom_bar() +
  theme_classic() +
  labs(x = "Tipo de carro", y = "Frequência", title = "Tipo de carro na base Cars93") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

# Gráfico com ggplot - geom_col()

# Leitura do arquivo com todas informações de candidatos(as) e bens de São Paulo
df_candidatos_com_bens_sp <- data.table::fread(file = "./data/candidatos_com_bens_sp.csv",
                                               encoding = "Latin-1")

# Cálculo da quantidade de candidatos(as) por partido
df_candidatos_partido <- df_candidatos_com_bens_sp %>%
  group_by(NM_PARTIDO) %>% 
  summarise(QUANTIDADE_CANDIDATOS = n())

# Fazer um gráfico de barras que mostre a quantidade de candidatos por partido em SP
ggplot(data = df_candidatos_partido, aes(x = reorder(NM_PARTIDO, QUANTIDADE_CANDIDATOS),
                                         y = QUANTIDADE_CANDIDATOS)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Partidos",
       y = "Quantidade de candidatos(as)",
       title = "Quantidade de Candidatos(a) por Partido em SP")+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  geom_text(aes(label=QUANTIDADE_CANDIDATOS), hjust=-0.5)
  
# Gráfico de pontos ------------------------------------------------------------

# Carga da base de dados
dados_economia_combustivel <- mpg

# Gráfico com ggplot passando os parâmetros mínimos e o tipo do geom
ggplot(data = dados_economia_combustivel, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

# Desafio 1 --------------------------------------------------------------------

# Leitura do arquivo com todas informações de candidatos(as) e bens de São Paulo
df_candidatos_com_bens_sp <- data.table::fread(file = "./data/candidatos_com_bens_sp.csv",
                                               encoding = "Latin-1")

# Fazer o histograma mostrando a distribuição dos valores dos bens dos(as) candidatos(as) de SP
ggplot(data = df_candidatos_com_bens_sp, aes(x = NR_IDADE_DATA_POSSE)) +
  geom_histogram() +
  theme_minimal() +
  labs(x = "Idade", y = "Frequência",
       title = "Distribuição da Idade na Data de Posse em SP")


# TRABALHANDO COM ESQUISSE -----------------------------------------------------

# Instalação e carregamento do pacote
install.packages("esquisse")
library(esquisse)


# Desafio 2 --------------------------------------------------------------------

ggplot(df_candidatos_com_bens_sp) +
  aes(
    x = VR_BEM_CANDIDATO,
    y = NR_IDADE_DATA_POSSE,
    colour = DS_GENERO
  ) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(DS_COR_RACA))







