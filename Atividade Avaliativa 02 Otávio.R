## Machine Learning - Atividade Avaliativa 02
## ALuno: Otávio Augusto Alves

if (! "ISLR" %in% installed.packages()) install.packages("ISLR")
if (! "MASS" %in% installed.packages()) install.packages("MASS")
if (! "dplyr" %in% installed.packages()) install.packages("dplyr")
if (! "ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (! "readr" %in% installed.packages()) install.packages("readr")
if (! "texreg" %in% installed.packages()) install.packages("texreg")

library(ISLR)
library(MASS)
library(dplyr)
library(ggplot2)
library(readr)
library(texreg)

#Questão 1 [10] 
# a) produza algumas descrições gráficas e numéricas para o banco Weekly. Há algum padrão?

str(Weekly)
head(Weekly)
pairs(Weekly)
summary(Weekly)
cor(Weekly[ , -9])

# a partir da função cor pode-se observar uma relação entre o ano e o volume, já que a correlação é próxima de 1. Todas as outras 
# são muito fracas (próximas de 0, tanto positivas ou negativas), inexistindo correlação.


# b) Use o banco de dados completo para fazer uma regressão logística da direção como variável resposta
# e das 5 variáveis lag mais volume como preditoras. Use summary para mostrar as respostas. Algum
# dos preditores parece ser estatisticamente significante? Se sim, qual?

regl1 = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly, family=binomial)
summary(regl1)

# apenas Lag2 é estatisticamente significativo, já que seu p-valor é menor do que 0.05,
# sendo 0.0296.

# c) Calcule a confusion matrix e overall fraction das predições corretas. Explique.

attach(Weekly)
glm.probs = predict(regl1, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)

557 + 54
557 + 430 + 48 + 54
611/1089

# 611 predições corretas em 1089 predições, um total de 56.10652% de acerto.


## d) Agora faça um modelo de regressão logística usando um dado de treino para 
# o período de 1990 a 2008, com Lag2 como o único preditor...

train = (Year < 2009)
Weeklyx = Weekly[!train, ]
glm.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
glm.probs = predict(glm.fit, Weeklyx, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
Direction.x = Direction[!train]
table(glm.pred, Direction.x)
14 + 34 + 56

65/104

# observa-se um acerto de 0.625.



## e) Repita a d usando LDA

lda.fit = lda(Direction ~ Lag2, data=Weekly, subset=train)
lda.fit
lda.pred=predict(lda.fit, Weeklyx)
lda.class=lda.pred$class
table(lda.class, Direction.x)

#observa-se o mesmo resultado quando realizado o LDA, um acerto de 62.5%.

## f) Repita fazendo QDA.

qda.fit = qda(Direction ~ Lag2, data=Weekly, subset=train)
qda.fit
qda.class=predict(qda.fit, Weeklyx)$class
table(qda.class, Direction.x)

61/(61+43)

#temos um acerto de 58.65.

## h) Qual dos métodos utilizados parece ser o melhor?

# a regressão logística e o LDA apresentaram os melhores resultados, de 62.5

## Exercício 2 [11]

# a) Crie uma variável binária...

attach(Auto)
median(mpg, data=Auto)

mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)

## b) explore os dados graficamente para investigar a associação entre mpg01 e outras variáveis.

cor(Auto[, -9])
pairs(Auto)

## A correlação é forte (negativa) com cylinders, displacement, horsepower e weight (valores próximos de -1). 
## Há uma correlação positiva forte com mpg também (valor próximo de 1).

## c) Divida os dados em grupo de teste e um de treino.

train = (year%%2 == 0)
test = !train
Auto.train = Auto[train, ]
Auto.test = Auto[test, ]
mpg01.test = mpg01[test]

# d) Faça um LDA para os dados de teste e treino da letra anterior.

lda1 = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data=Auto, subset=train)
lda.prev = predict(lda1, Auto.test)
mean(lda.prev$class != mpg01.test)

# a taxa do erro teste é de 12.64%.

# e) Faça por QDA

qda01 = qda(mpg01 ~ cylinders + displacement + horsepower + weight, data=Auto, subset= train)
qda.prev = predict(qda01, Auto.test)
mean(qda.prev$class != mpg01.test)

# o erro de teste por QDA é 13.18%.

## f) faça uma regressão logística.

regl02 = glm(mpg01 ~ cylinders + displacement + horsepower + weight, data=Auto, subset= train, family = binomial)
glm.probs = predict(regl02, type = "response", Auto.test)
glm.pred=rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, mpg01.test)
mean(glm.pred != mpg01.test)
# o erro de teste é igual a 12.08791%.

# Exercício 3 [13].

# Acesse o banco de dados Boston e faça QDA, Regressão Logística e LDA para prever se um determinado
# bairro tem uma taxa de crime acima ou abaixo da mediana.

attach(Boston)
str(Boston)
median(crim)

crim01 = rep(0, length(crim))
crim01[crim > median(crim)] = 1
Boston = data.frame(Boston, crim01)
train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crim01.test = crim01[test]

# regressão logística

reglcrim = glm(crim01 ~ age + black + chas + dis + indus + lstat + medv + nox + ptratio + rad + rm + tax + zn, data=Boston,
               family = binomial, subset=train)
glm.probs = predict(reglcrim, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crim01.test)

# o erro de teste é 18,18.

# LDA

ldacrim = lda(crim01 ~ age + black + chas + dis + indus + lstat + medv + nox + ptratio + rad + rm + tax + zn, data=Boston,
              family = binomial, subset=train)
lda.prev = predict(ldacrim, Boston.test)
mean(lda.prev$class != crim01.test)

# o erro de teste é 13.43874%.


# pode-se observar que os modelos não geraram boas predições, na medida em que conseguiram explicar menos de 20% das observações, isto é,
# se o bairro possui uma taxa de crime acima ou abaixo da mediana baseado nas variáveis disponíveis. nesse sentido, 
# aponta-se que a correlação entre as variáveis não está bem explicada isto é, que algumas não possuem tanto peso na taxa de crime.