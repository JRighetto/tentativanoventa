# bom, acabou o espaço no meu computador C, tive que ir para o D,
# e por alguma razão perdi todo o conteúdo do meu script.


# Exercicios aula 2 - tidyr -----------------------------------------------

imdb <- readr::read_rds("data/imdb.rds")
library(tidyr)
library(dplyr)

# 1. Crie 5 novas colunas de gêneros na base imdb, cada uma com um dos gêneros contidos na coluna generos.
# Para os filmes com menos de 5 gêneros, substitua os valores NA pela string “inexistente”.

imdb %>% separate (
  col = generos,
  into = c("genero1","genero2","genero3","genero4","genero5"),
  sep = "\\|",
  extra = "drop"
) %>%
  replace_na(list(genero1 = "inexistente", genero2 = "inexistente",
                  genero3 = "inexistente",genero4 = "inexistente",genero5 = "inexistente")) %>%
View()

# 2. Substitua os “????” no código abaixo para criar uma tabela do lucro médio dos filmes ao longo dos anos
# de 2000 a 2016, com cada ano sendo uma coluna da base.

imdb %>%
  mutate(lucro = receita - orcamento) %>%
  filter(ano > 1999 & ano < 2017) %>%
  group_by(ano) %>%
  summarise(lucro_medio = mean(lucro,na.rm = T)) %>%
  pivot_wider(names_from = ano, values_from = lucro_medio) %>%
  View()

#Para os exercícios 3, 4 e 5, vamos utilize a base pokemon, disponível no pacote basesCursoR.

pokemon <- readr::read_rds("data/pokemon.rds")

#3. Utilize a função unite() para juntar as duas colunas de tipo em uma única coluna na base pokemon.

pokemon %>% unite("tipos",c("tipo_1","tipo_2"),sep="|",remove = T) %>% View()
