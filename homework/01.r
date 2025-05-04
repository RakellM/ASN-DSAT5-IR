# Homework 01

# List of items in the workspace
ls()

# Delete ecverything from your workspace
rm(list=ls())


# LIBRARY
# install.packages("abjData")

library(tidyverse)
library(abjData)

# DATA

data_pnud <- pnud_min
view(data_pnud)

data_pnud_2010 <- filter(data_pnud, ano == 2010)
view(data_pnud_2010)


# Q1. Count of rows per UF

data_pnud_2010 %>%
    count(uf_sigla)


# Q2. Count of rows per Region

data_pnud_2010 %>%
    count(regiao_nm)


# Q3. Count of rows per Region sorted ascending

data_pnud_2010 %>%
    count(regiao_nm) %>%
    arrange(n)

# Q4. Select County, State and Population

data_pnud_2010 %>%
    select(muni_nm, uf_sigla, pop)

# Q5. Select County, State and Population and filter population greater than 1MM

data_pnud_2010 %>%
    select(muni_nm, uf_sigla, pop) %>%
    filter(pop > 1000000)

# Q6. Select County, State and Population, filter population greater than 1MM then sorted descending.

data_pnud_2010 %>%
    select(muni_nm, uf_sigla, pop) %>%
    filter(pop > 1000000) %>%
    arrange(desc(pop))

# Q7. Select County, State and Population, filter population greater than 1MM then sorted descending. Then create a new column using population divided by 1 million.

data_pnud_2010 %>%
    select(muni_nm, uf_sigla, pop) %>%
    filter(pop > 1000000) %>%
    arrange(desc(pop)) %>%
    mutate(pop_milhoes = pop / 1000000)

# Q8. Select County, State and Population, filter population greater than 1MM then sorted descending. Then create a new column using population divided by 1 million, fizex 2 decimals.

data_pnud_2010 %>%
    select(muni_nm, uf_sigla, pop) %>%
    filter(pop > 1000000) %>%
    arrange(desc(pop)) %>%
    mutate(pop_milhoes = round(pop / 1000000, 2))



data_pnud_2010 %>%
    filter(pop > 1000000)
