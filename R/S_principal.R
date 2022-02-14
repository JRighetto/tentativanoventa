# bom, acabou o espaço no meu computador C, tive que ir para o D,
# e por alguma razão perdi todo o conteúdo do meu script.


# Exercicios aula 2 - tidyr -----------------------------------------------

imdb <- readr::read_rds("data/imdb.rds")
library(tidyr)
library(dplyr)
library(ggplot2)

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

#4. Utilize a função unite() para juntar as três colunas de cor em uma única coluna na base pokemon.
#Faça isso sem remover as 3 colunas originais.

pokemon %>% unite("cores",c("cor_1","cor_2","cor_final"),sep="|",remove = F) %>% View()

#5.
#a.Utilize a função pivot_longer() para criar uma única coluna de tipo na base pokemon.

pokemon %>%
  pivot_longer(cols= starts_with("tipo"),
             names_to = "n_tipo",
             values_to = "tipo") %>%
  View()

#b. Utilize a base criada no item (a) e escreva um código para descobrir qual o tipo mais
#frequente na base, independentemente se ele é primário (tipo_1) ou secundário (tipo_2).

pokemon %>%
  pivot_longer(cols= starts_with("tipo"),
               names_to = "n_tipo",
               values_to = "tipo") %>%
  group_by(tipo) %>%
  summarise(contagem = length(tipo)) %>%
  arrange(desc(contagem))

#6. Escreva uma função que receba uma base qualquer e o nome de uma coluna numérica dessa base
# e retorne uma figura com um gráfico de dispersão da coluna escolhida contra cada uma das outras
# variáveis numéricas da base.

imdb <- readr::read_rds("data/imdb.rds")
imdb_nest <- imdb %>%
  group_by(ano) %>%
  nest() %>%
  arrange(ano)

head(imdb_nest, 8)

imdb_nest$data[[10]] %>% View()

fazer_grafico_dispersao <- function(tab) {
  tab %>%
    ggplot(aes(x = orcamento, y = receita)) +
    geom_point()
}

imdb_graficos <- imdb_nest %>%
  mutate(
    grafico = purrr::map(data, fazer_grafico_dispersao)
  )

head(imdb_graficos, 6)
imdb_graficos$grafico[[74]]

#

midia <- data.frame(mês = c("Jan", "Fev", "Mar","Abr","Mai","Jun"),
                           invest = c(150000, 10000, 10000,8000,85000,6000),
                           upgrades = c(1300, 1200, 1100,1000,1100,990),
                           stringsAsFactors = FALSE)
midia_nest <- midia %>%
  group_by(mês) %>%
  nest()

fazer_grafico_dispersao <- function(frame,base,variavel) {
  frame %>%
    ggplot(aes(x = invest, y = upgrades)) +
    geom_point()
}

midia_nest <- midia_nest %>%
  mutate(
    grafico = purrr::map(data, fazer_grafico_dispersao)
  )

midia_nest$grafico[[1]]

# 1. O CPF é um número de 11 dígitos, por exemplo: 54491651884. No entanto para facilitar a visualização
# costumamos mostrá-lo com separadores a cada 3 casas: 544.916.518-84. Crie uma função que transforma um número
# de 11 dígitos em uma string com as separações, como um CPF.

install.packages("stringr")
library(stringr)

ajustar_cpf <- function(cpf){
  arg1 = str_sub(cpf, end=3)
  arg2 = str_sub(cpf, start=4, end=6)
  arg3 = str_sub(cpf, start=7, end=9)
  arg4 = str_sub(cpf, start=10, end=11)
  cpf_final = paste(arg1,".",arg2,".",arg3,"-",arg4,sep="")
  cpf_final
}

ajustar_cpf(cpf)
cpf <- "34511378878"
str_sub(cpf, end=3)

#2. Transforme o vetor de strings abaixo em "01 - Alto" "02 - Médio" "03 - Baixo".

n <- c("01 - ","02 - ","03 - ")
s <- c('Alto', 'Médio', 'Baixo')
str_c(n,s)
# assim eu não sei se a intenção era algo tão simples quanto isso kkkkk mas enfim.

# 3. Crie uma regex que capture múltiplas versões da palavra ‘casa’.
# Ela deve funcionar com as palavras ‘Casa’, ‘CASA’, ‘CaSa’, ‘CAsa’. Teste-a usando a função str_detect().

s <- c('Casa', 'CASA', 'CaSa', 'CAsa')
str_detect(string = s, pattern = "(?i)casa")

# 4. Imagine que a seguinte string é a parte final de uma URL.
# /ac/rio-branco/xpto-xyz-1-0-1fds2396-5
# Transforme-a em “AC - Rio Branco” utilizando funções do pacote {stringr}.

url <- c('/ac/rio-branco/xpto-xyz-1-0-1fds2396-5')
str_replace(url,"/ac/rio-branco","AC - Rio Branco")
print("/ac/rio-branco")

#eu to confusa com os lados das barras que são considerados regexs.
#até porque eu não acho que o exercicio era tão simples assim.

