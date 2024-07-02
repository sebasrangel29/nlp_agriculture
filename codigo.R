##Código del informe "Textual Analysis in R:
##Patrons of Agricultural News in Italy in First Half of 2024""
#2 de Julio del 2024
#Henry Sebastián Rangel y Luisa Fernanda Arenas.
############ alternativa
library("NLP")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
rm(list=ls())
library(readxl)
PL <- read_excel("C:/Users/Sebastián Rangel/OneDrive - Universidad Santo Tomás Bucaramanga/Escritorio/Proyecto política agraria/Agro_periodicos1.xlsx", 
                 col_types = c("text", "text", "date", 
                               "text", "text", "text"))

colnames(PL)=c("Fuente","Título", "Fecha", "Texto",  "Link", "Ecuacion" )
attach(PL)

###revisar quitar los apostrofe y los numeros
PL$Texto=gsub(pattern ="\\’", replacement = " ", PL$Texto)
PL$Texto=gsub("[^[:alpha:] ]","",PL$Texto)
PL$Texto <- gsub("[Èàèìòùé]", "", PL$Texto)
PL$Texto[19]
###creando la variable mes y organizandolo, también puede usarse mutate parta ordenar las columnas
PL$ID=(1:length(Fecha))
PL$Mes=months(Fecha, abbreviate=TRUE)
PL=PL[, c(7,1,2,8,3,4,5,6)]

###syntax with grep Regular expression
### https://regexone.com/lesson/matching_characters
###extraer noticias con un patrón
grep(pattern = "trattori", x = PL$Texto, value = TRUE)
grep(pattern = "aprileogn", x = PL$Texto, value = TRUE)
library(tidyverse)
library(tidytext)
library("tm")

##Toquenizando por fraces y por palabras filtrando por periodico o 
### por mes
PL=PL%>%arrange((Fecha))
PL%>%
  unnest_tokens(output = "sentences", input = Texto,
                token ="regex", pattern = "trattori") %>%
  count(Mes)


## limpieza de stop words en italiano
italian=data.frame(stopwords("italian"))
colnames(italian)=c("word")

###personalizar stop words agregar nuevas
custom <- add_row(italian, word ="ue")
custom <- add_row(custom, word ="pi")
custom <- add_row(custom, word ="de")
custom <- add_row(custom, word ="d")
custom <- add_row(custom, word ="dopo")
custom <- add_row(custom, word ="gi")
custom <- add_row(custom, word ="due")
custom <- add_row(custom, word ="solo")
custom <- add_row(custom, word ="essere")
custom <- add_row(custom, word ="sempre")
custom <- add_row(custom, word ="oggi")
custom <- add_row(custom, word ="mila")
custom <- add_row(custom, word ="cos")
### Pruebas 
#PL%>% unnest_tokens(word,Texto)%>% anti_join(italian)%>%
  #count(word, sort=T)

#PL%>% unnest_tokens(word,Texto)%>% anti_join(custom)%>%
  #count(word, sort=T)
###stemmed (extraer raices)

###

PL_NSTOP=PL%>% unnest_tokens(word,Texto)%>% anti_join(custom)

stemmmed <-  PL_NSTOP%>%
  mutate(word = wordStem(word, language = "italian"))
t=stemmmed%>%
  count(word, sort=T)
print(t,n=50)
set.seed(1234)
wordcloud(words = t$word, freq = t$n, min.freq = 5,
          max.words=100, random.order=F, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))
library(wordcloud2)
set.seed(1234)
wordcloud2(data=t, size=1, color='random-dark', shape = 'pentagon')
##final data set

stemmmed

##################################################clasificación por Fuente
# Count occurrence by article_id and word

words <- stemmmed%>%
  count(Fuente, word, sort=T)
stemmmed%>%
  count( word, sort=T)


# How many different word/article combinations are there?
unique_combinations <- nrow(words)
table(PL$Fuente)
# Filter to responses with the word "protest"
words_with <- words %>%
  filter(word == "protest")
words_with
# How many articles had the word "pprotest"?
number_articles <- nrow(words_with)
number_articles
####trattor
words_with <- words %>%
  filter(word == "trattor")
words_with

words_with <- words %>%
  filter(word == "bruxelles")
words_with
# How many articles had the word "pprotest"?
number_articles <- nrow(words_with)
number_articles
##########clasificación por mes

words_mes <- stemmmed%>%
  count(Mes, word, sort=T)
stemmmed%>%
  count( word, sort=T)
#########similaridad de noticias
# How many different word/article combinations are there?
unique_combinations <- nrow(words_mes)
table(PL$Mes)
# Filter to responses with the word "protest"
words_with <- words_mes %>%
  filter(word == "protest")
words_with
####trattor
words_with <- words_mes %>%
  filter(word == "trattor")
words_with

words_with <- words_mes %>%
  filter(word == "bruxelles")
words_with
####Create a tibble with TFIDF values 

##Sin stop word pero la palabra completa
TFIDF=PL_NSTOP%>%
  count(ID, word, sort=T)%>% bind_tf_idf(word, ID, n)
##organizar de menor a mayor

TFIDF%>%
  arrange(desc(tf_idf))

# Calculate the cosine similarity by chapter, using words
library(widyr)
library(xtable)
comparisons <- TFIDF %>%
  pairwise_similarity(ID,word , n) %>%
  arrange(desc( similarity))

print(xtable(head(comparisons,10)), type = "latex", file = "output.tex")
comparisons <- TFIDF %>%
  pairwise_similarity(ID,word , tf_idf) %>%
  arrange(desc( similarity))
##eliminadas por ser ls mismas noticias
PL= PL[-c(140,139),]
#############################clasificacion
# Stem the tokens
# Create a document term matrix using TFIDF weighting
stemmmed=PL%>% unnest_tokens(word, token = "words", Texto)%>% anti_join(custom)%>%
  mutate(word = wordStem(word, language = "italian"))


matrix=stemmmed%>%
  count( ID, word) %>%
  cast_dtm(document =ID, term = word,
           value = n, weighting = tm::weightTfIdf)

# Print the matrix details 
print(matrix )

###reducir el sparse
less_sparse_matrix <-
  removeSparseTerms(matrix, sparse = 0.985)

# Print results
matrix
less_sparse_matrix
##clasificación, OJo comabia el weighting

matrix1=stemmmed%>%
  count( ID, word) %>%
  cast_dtm(document =ID, term = word,
           value = n, weighting = tm::weightTf)
matrix2=PL_NSTOP%>%
  count( ID, word) %>%
  cast_dtm(document =ID, term = word,
           value = n, weighting = tm::weightTf)

library(topicmodels)
lda <- LDA(matrix2, k = 2, method = 'Gibbs',
                       control = list(seed = 1111))
lda

betas <-
  tidy(lda, matrix = "beta")
betas


grupo1=betas %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  arrange(topic, -beta) %>%
  filter(topic == 1)

grupo2=betas %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  arrange(topic, -beta) %>%
  filter(topic == 2)


grupo2 %>% 
  mutate(name = fct_reorder(term, beta)) %>% 
  ggplot( aes(x=name, y= grupo2$beta)) +
  geom_bar(stat="identity", fill="Green", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Grup 1 of news") + ylab("Betas") +
  theme_bw()


grupo1 %>% 
  mutate(name = fct_reorder(term, beta)) %>% 
  ggplot( aes(x=name, y= grupo2$beta)) +
  geom_bar(stat="identity", fill="Blue", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Grup 2 of news") + ylab("Betas") +
  theme_bw()

### prueba para 4 grupos


# Perform Topic Modeling
sentence_lda <-
  LDA(matrix1, k = 3, method = 'Gibbs', control = list(seed = 1111))
# Extract the beta matrix 
sentence_betas <- tidy(sentence_lda, matrix = "beta")
sentence_gamma <- tidy(sentence_lda, matrix = "gamma")
sentence_gamma %>%  filter(document == 1)
sentence_gamma %>%  filter(document == 2)
sentence_gamma %>%  filter(document == 3)
print(sentence_gamma, n=300)
# Topic #2
sentence_betas %>%
  filter(topic == 1 ) %>%
  arrange(-beta)
# Topic #3
sentence_betas %>%
  filter(topic == 2) %>%
  arrange(-beta)

sentence_betas %>%
  filter(topic == 3) %>%
  arrange(-beta)


#### para evaluar los cortes
sample_size <-floor(0.90* nrow(matrix1))
set.seed(1111)
train_ind <- sample(nrow(matrix1), size = sample_size)
train <- matrix1[train_ind,]
test <- matrix1[-train_ind,]

values = c()
for(i in c(2:35)){  
  lda_model <- LDA(train, k = i, method = "Gibbs", control = list(iter = 25, seed = 1111))  
  values <- c(values, perplexity(lda_model, newdata = test))  
  }
plot(c(2:35), values, main="Perplexity for Topics",      xlab="Number of Topics", ylab="Perplexity")


# Extract the gamma matrix 
gamma_values <- tidy(lda, matrix = "gamma")
# Create grouped gamma tibble
grouped_gammas <- gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)
# Count (tally) by topic
grouped_gammas %>% 
  tally(topic, sort=TRUE)
# Average topic weight for top topic for each sentence
grouped_gammas %>% 
  summarize(avg=mean(gamma)) %>%
  arrange(desc(avg))
#########
#aumentar capacidad
library(parallel)
library(MASS)

starts <- rep(100, 40)
fx <- function(nstart) kmeans(Boston, 4, nstart=nstart)
numCores <- detectCores()
numCores
#Análisis sentimental
sentimientos_df <- get_nrc_sentiment(PL_NSTOP$word, lang="italian")
head(sentimientos_df,60)
get_sentiment_dictionary('nrc', language = "italian")
brewer.pal(n = 8, name = "Set3")

summary(sentimientos_df)

barplot(
  colSums(prop.table(sentimientos_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 1.2,
  col =brewer.pal(n = 8, name = "Dark2"),
  main = "",
  sub = "",
  xlab="Emotions", ylab = NULL,
  names.arg=c("Anger","Anticipation","Disgust","Fear","Joy", "Sadness", "Surprise", "Trust"))


barplot(colSums(prop.table(sentimientos_df[, 1:8])))

PL_NSTOP$word[sentimientos_df$trust> 0]
PL_NSTOP$word[sentimientos_df$anticipation> 0]
PL_NSTOP$word[sentimientos_df$joy> 0]
PL_NSTOP$word[sentimientos_df$fear> 0]
palabras_tristeza <- PL_NSTOP$word[sentimientos_df$sadness> 0]

palabras_tristeza_orden <- sort(table(unlist(palabras_tristeza)), decreasing = TRUE)
head(palabras_tristeza_orden, n = 12)

nube_emociones_vector <- c(
  paste(PL_NSTOP$word[sentimientos_df$trust> 0], collapse = " "),
  paste(PL_NSTOP$word[sentimientos_df$anticipation > 0], collapse = " "),
  paste(PL_NSTOP$word[sentimientos_df$sadness > 0], collapse = " "),
  paste(PL_NSTOP$word[sentimientos_df$fear >0], collapse = " "))

nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))

nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)
colnames(nube_tdm) <- c('Trust', 'Anticipation', 'Sadness', 'Fear')
head(nube_tdm)
set.seed(757) # puede ser cualquier n?mero
comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = c("green", "red", "orange", "blue"),
                 title.size = 1, max.words = 800, scale = c(1.5, 1), rot.per = 0.45)
## prueba por noticias
PL_NSTOP2=PL%>% unnest_tokens(token="sentences", word,Texto)%>% anti_join(custom)
sentimientos_df2 <- get_nrc_sentiment(PL_NSTOP2$word, lang="italian")
sentimientos_valencia2 <- (sentimientos_df2$negative *-1) + sentimientos_df2$positive
length(sentimientos_valencia2)
simple_plot(sentimientos_valencia2, title = "", legend_pos = "top")
##
sentimientos_valencia <- (sentimientos_df$negative *-1) + sentimientos_df$positive
length(sentimientos_valencia)
simple_plot(sentimientos_valencia, title = "", legend_pos = "top")
summary(sentimientos_valencia)
syuzhet_vector <- get_sentiment(PL_NSTOP$word, method="syuzhet")
syuzhet_vector2 <- get_sentiment(PL_NSTOP$word, method="nrc") 
library(syuzhet)
dct_values <- get_dct_transform(
  syuzhet_vector2, 
  low_pass_size = 5, 
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)
plot(
  dct_values, 
  type ="l", 
  main ="", 
  xlab = "Tiempo narrativo", 
  ylab = "Balance Emocional", 
  col = "green"
)
