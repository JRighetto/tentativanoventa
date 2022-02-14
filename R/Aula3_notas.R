# Aula sobre REGEX porque eu comecei a fazer os exercícios e percebi que não sei muito bem como ... encaixar
# as coisas.

print("está 10\u00BAC lá fora")
cat("está 10\u00BAC lá fora") #tirou as aspas e o [1]; um mostra estrutura, e outro o texto.

cat("ele disse \"escapar\"") #o escapar diz que o caractere a frente não faz o que ele faz normalmente, é só texto

library(stringr)

abc <- c("a","b","c")
str_c("pre-",abc,"-pós")

#e funciona como vetor porque foi isso que eu usei no exercicio proposto

def <- c("d","e","f")
str_c(abc,def)

frutas <- c("banana","TANGERINA","maçã","lima")
str_detect(frutas,"na")
str_detect(frutas,"ma")
str_detect(frutas,"^ma")
str_detect(frutas,"$ma") #não deu certo assim
str_detect(frutas,"ma$")

# + (1 ou mais vezes), *(0 ou mais vezes), {m,n} entre m e n vezes
ois <- c("oi","oii","oiii!","oioioi!")
str_extract(ois,"i+")
str_detect(ois,"i+")

#[]é um conjunto e () é um conjunto INQUEBRAVEL
