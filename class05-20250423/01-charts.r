library(tidyverse)

# or
library(dplyr)
library(ggplot2)

main_path <- getwd()

imdb <- read_rds("DSAT5-IR\\data\\imdb_rds.rds")
imdb <- imdb %>% mutate(lucro = receita - orcamento)

colnames(imdb)

# line chart

# about ggplot: we can storage it on a variable

myChart <- imdb %>% ggplot()
myChart

imdb %>%
    ggplot() +
    geom_point(aes(x = num_avaliacoes , y = nota_imdb ))

# adding a simple regression line in the chart
# simple regression fuction
model <- lm(nota_imdb ~ num_avaliacoes, data = imdb)
model

imdb %>%
    ggplot() +
    geom_point(aes(x = num_avaliacoes , y = nota_imdb )) +
    geom_abline(intercept = 5.489, slope = 4.608-100^(-6), color = "red")

# performing a linear regression to log(num_avaliacoes)
model_log <- lm(nota_imdb ~ log(num_avaliacoes), data = imdb)
model_log

imdb %>%
    ggplot() +
    geom_point(aes(x = log(num_avaliacoes) , y = nota_imdb )) +
    geom_abline(intercept = 3.7134, slope = 0.2652, color = "red")

# bonus: geom_smooth
imdb %>%
    ggplot() +
    geom_point(aes(x = num_avaliacoes , y = nota_imdb )) +
    geom_smooth(aes(x = num_avaliacoes , y = nota_imdb ))


imdb %>%
    ggplot() +
    geom_line(aes(x = orcamento , y = receita ))

imdb %>%
    filter(ano > 1920) %>%
    group_by(ano) %>%
    ggplot() +
    geom_line(aes(x = orcamento , y = receita ))
