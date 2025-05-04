# Homework 2
library(tidyverse)
library(readr)
library(patchwork)
library("likert")

imdb <- read_rds("DSAT5-IR\\data\\imdb.rds")
view(imdb)

###################################################################################################
# Q1. Create a scatter plot (dot plot) of the IMDb rating versus the movie budget.
# Tips: ggplot(), aes(), geom_point()
# Challenge: Add a vertical line indicating the average budget.
# Tip: Use geom_abline()

colnames(imdb)

imdb %>%
  select(titulo, orcamento, nota_imdb) %>%
  filter(!is.na(orcamento), !is.na(nota_imdb)) %>%
  mutate(budget_inMillions = orcamento / 1e6 ) %>%  # Convert budget to millions
  ggplot() +
  geom_point(aes(x = budget_inMillions, y = nota_imdb)) +
  geom_vline(xintercept = mean(imdb$orcamento, na.rm = TRUE) / 1e6 , 
              color = "red", 
              linewidth = 1) +
  labs(title = "IMDb Rating vs Movie Budget",
       x = "Budget (in Millions)",
       y = "IMDb Rating") +
  scale_x_continuous(labels = scales::dollar) +
  annotate("label",
           x = mean(imdb$orcamento, na.rm = TRUE) / 1e6,
           y = 0,
           label = paste0("Avg: $", round(mean(imdb$orcamento, na.rm = TRUE) / 1e6, 1), "M"),
           color = "black",
           fill = "lightyellow" ,
           size = 3) +
  theme_minimal()


###################################################################################################
# Q2. Run the code below:
diretores <- c(
  "Steven Spielberg", 
  "Quentin Tarantino", 
  "Christopher Nolan",
  "Martin Scorsese"
)

imdb %>% 
  filter(direcao %in% diretores) %>% 
  group_by(ano, direcao) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = nota_media)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(direcao))

# a) Based on the generated plot, describe what the facet_wrap() function does.
The `facet_wrap()` function creates a chart per director as the variable used was direction and we applied a filter for 4 director only.
These charts would have the same scale on both axis to facilitate the comparison between them. 

# b) Use the nrow and ncol arguments of facet_wrap() to display the four plots in a single column.
# Tip: Check the documentation for the facet_wrap() function:

imdb %>% 
  filter(direcao %in% diretores) %>% 
  group_by(ano, direcao) %>% 
  summarise(nota_media = mean(nota_imdb, na.rm = TRUE)) %>% 
  ggplot(aes(x = ano, y = nota_media)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(direcao), nrow = 4 , ncol = 1) 

###################################################################################################
# Q3. Let’s create a bar chart showing the number of movies with a rating greater than 8 over the years.
# a) Create a new column in the IMDB dataset indicating whether a movie’s rating is greater than 8.
# Tip: Use the ifelse() function.
imdb <- imdb %>%
  mutate(rating_above_8  = ifelse(nota_imdb > 8, TRUE, FALSE))


# b) Using the column created in (a), create a table with the annual number of movies with a rating above 8.
imdb_high_ratings_per_year <- imdb %>%
  filter(rating_above_8 == TRUE) %>%
  group_by(ano) %>%
  summarise(qtd_filmes = n()) %>%
  arrange(ano)


# c) Use the table created in (b) to create a bar chart showing the number of movies with a rating greater than 8 over the years.
imdb_high_ratings_per_year %>%
  ggplot(aes(x = ano, y = qtd_filmes)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of Movies with IMDb Rating > 8 per Year",
       x = "Year",
       y = "Number of Movies") +
  theme(axis.text.x = element_text(angle = 0, size=8), plot.margin = unit(c(1,1,1,1),"cm"))


###################################################################################################
# Q4. Combining different plots
# Install the patchwork package: install.packages("patchwork")
# a) Create a plot of Keanu Reeves’ movies’ average ratings over the years. Save it to the object grafico_notas.

actor = "Keanu Reeves"

grafico_notas <- imdb %>%
  filter(str_detect(elenco, actor), !is.na(nota_imdb)) %>%
  group_by(ano) %>%
  summarise(media_nota = mean(nota_imdb, na.rm = TRUE)) %>%
  ggplot(aes(x = ano, y = media_nota)) +
    geom_line(color = "darkgreen") +
    geom_point(color = "darkgreen") +
    labs(title = "Avg IMDb Rating by Year",
         x = "Year",
         y = "Avg IMDb Rating") +
    theme_minimal()

#grafico_notas

# b) Create a histogram of the profits of Keanu Reeves’ movies. Save it to the object grafico_lucro.

grafico_lucro <- imdb %>%
  mutate(lucro = receita - orcamento) %>%
  filter(str_detect(elenco, actor), !is.na(lucro)) %>%
  ggplot(aes(x = lucro / 1e6)) +
  geom_histogram(fill = "darkorange", bins = 30, colour = "white") +
  labs(title = "Movie Profit Distribution",
       x = "Profit (in Millions)",
       y = "Count") +
  theme_minimal()

#grafico_lucro

# c) Run the code below and observe the resulting figure:
# library(patchwork)
# grafico_notas + grafico_lucro

(grafico_notas + grafico_lucro) +
  plot_annotation(
    title = paste0(actor, ": Career Overview"),
    subtitle = "Ratings and Profits of Movies Over the Years",
    theme = theme(plot.title = element_text(size = 16, face = "bold"),
                  plot.subtitle = element_text(size = 12))
  )

# d) Why couldn’t we use the facet_wrap() function to generate the same figure as in (c)?
# We cannot use the facet_wrap function because it only works when we are using the same dataset and same variables, they are all subplot of the same dataset.


###################################################################################################
# Q5. Run the code below:
# install.packages("likert")
# library(likert)
# data("gap", package = "likert")
# gap_long <- gap %>%
#   tidyr::pivot_longer(-StudentId, names_to = "question", values_to = "response")
# Note: pivot_longer() is a function that “melts” the dataset, stacking columns. It’s worth studying what this function does. It often works well with ggplot2().

# Using the gap_long dataset, complete the following exercises:

data("gap", package = "likert")
gap_long <- gap %>%
  tidyr::pivot_longer(
    cols = -StudentId, 
    names_to = "questao", 
    values_to = "resposta"
    )

# a) Create a table with the count of each response for each question.

gap_summary <- gap_long %>%
  count(questao, resposta)

# b) Using the table above, create a plot with a horizontal bar for each question and the counts on the X-axis. Color the fill of the bars to represent the responses (fill).

gap_summary %>%
  ggplot() +
  geom_col(aes(x = n, y = fct_rev(questao), fill = resposta)) +
  labs(title = "Responses per Question",
       x = "Count",
       y = "Question") +
  theme_minimal()

###################################################################################################
# Q6. Using the same code from the previous exercise, and modifying categories and colors:
# a) Convert the response column to a factor using mutate().

gap_long <- gap_long %>%
  arrange(resposta) %>%
  mutate(resposta = factor(resposta))

# b) Add + scale_fill_brewer(palette = "RdBu") to the plot.

gap_long %>%
  count(questao, resposta) %>%
  ggplot(aes(x = n, y = fct_rev(questao), fill = resposta)) +
  geom_col() +
  scale_fill_brewer(palette = "RdBu") + 
  labs(title = "Responses per Question",
       x = "Count",
       y = "Question",
       fill = "Response (1–7)") +
  theme_minimal()

# c) Check whether a scale ranging from blue to red appears.
# It goes from red (1) to blue (7). Here is the same chart going from blue (1) to red (7).
gap_long %>%
  count(questao, resposta) %>%
  ggplot(aes(x = n, y = fct_rev(questao), fill = resposta)) +
  geom_col() +
  scale_fill_brewer(palette = "RdBu", direction = -1) + 
  labs(title = "Responses per Question",
       x = "Count",
       y = "Question",
       fill = "Response (1–7)") +
  theme_minimal()

 imdb |>
    mutate(decada = as.character(floor(ano/10)*10)) |>
    ggplot(aes(x = decada, y = nota_imdb, fill = decada)) +
    geom_boxplot()
