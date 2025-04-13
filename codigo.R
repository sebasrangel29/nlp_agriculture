##Código del articulo Coyuntura de la agricultura en Italia: Metodología para un análisis textual en medios digitales
#Abril del 2025
#Henry Sebastián Rangel 
#Luisa Fernanda Arenas.
#####
## 1.Lectura y creacion del data frame (DF)
library(readr)

DF<- read_delim("https://raw.githubusercontent.com/sebasrangel29/nlp_agriculture/refs/heads/Articulo_NLP/Agro_periodicos1.csv",
                    delim = ";",
                    col_types = cols(
                      .default = col_character(),
                      "Fecha de publicación" = col_date(format = "%d/%m/%Y")
                    ))[,-7]

colnames(DF)=c("Fuente","Título", "Fecha", "Texto",  "Link", "Ecuacion" )
attach(DF)

DF$ID=(1:length(Fecha)) #Se crea un ID basado en la columna fecha
DF$Mes=months(Fecha, abbreviate=TRUE) # se crea nueva columna con el nombre del mes

#####
## 2.Limpieza de los datos
library("dplyr")
library("tm")
library("tidytext")
library("SnowballC")

#Identificar duplicados 
duplicados <- DF[duplicated(DF[c("Fuente", "Texto", "Fecha")]), ] #duplicado con igual fuente, texto y fecha
DF <- DF[!duplicated(DF[c("Fuente", "Texto")]), ] #Eliminacion de duplicados 

DF$Texto[19] #Ejemplo del articulo
#Eliminación de caracteres no deseados (Regex)
DF$Texto=gsub(pattern ="\\’", replacement = " ", DF$Texto) #Reemplaza apóstrofes por espacio
DF$Texto=gsub("[^[:alpha:] ]","",DF$Texto) #Elimina cualquier símbolo que no sea letra
DF$Texto <- gsub("[Èàèìòùé]", "", DF$Texto) #Elimina acentos italianos  

# Eliminacion de Stopwords en italiano
italian=data.frame(stopwords("italian")) # Cargar lista de stopwords en italiano
colnames(italian)=c("word")

custom_stopwords <- italian %>% # Agregar palabras personalizadas
  add_row(word = "ue") %>%
  add_row(word = "pi") %>%
  add_row(word = "de") %>%
  add_row(word = "d") %>%
  add_row(word = "dopo") %>%
  add_row(word = "gi") %>%
  add_row(word = "due") %>%
  add_row(word = "solo") %>%
  add_row(word = "essere") %>%
  add_row(word = "sempre") %>%
  add_row(word = "oggi") %>%
  add_row(word = "mila") %>%
  add_row(word = "anni") %>%
  add_row(word = "fa") %>%
  add_row(word = "pu") %>%
  add_row(word = "cos") %>%
  add_row(word = "agricola") %>%
  add_row(word = "agricole") %>%
  add_row(word = "agricolo") %>%
  add_row(word = "agricoltori") %>%
  add_row(word = "agricoltura") %>%
  add_row(word = "italia")  # Continuar con otras palabras si es necesario

DF_SS <- DF %>% unnest_tokens(word, Texto) %>% #Eliminación de stopwords del texto
  anti_join(custom_stopwords, by = "word") 

Ejemplo <- DF_SS %>% filter(ID == 19) #Ejemplo del articulo
parrafo <- Ejemplo %>% pull(word) %>% paste(collapse = " ")
print(parrafo)

# Aplicar stemming en italiano
DF_stemmed <- DF_SS %>%
  mutate(word = wordStem(word, language = "italian"))

# vector de elementos unicos (revisar si se incluye)
unicos1=DF_stemmed%>%count(word, sort=T)
unicos2=DF_SS%>%count(word, sort=T) 

#####
## 3. Descripcion de los datos
library(wordcloud)

noticias_por_mes <- DF %>%count(Mes)
noticias_por_mes <- noticias_por_mes %>% mutate(porcentaje = n / sum(n) * 100)
noticias_por_mes

DF <- DF %>% arrange(Fecha)

noticias_por_fuente <- DF %>%count(Fuente)
noticias_por_fuente <- noticias_por_fuente %>% mutate(porcentaje = n / sum(n) * 100)
noticias_por_fuente

frecuencia=DF_SS%>%count(word, sort=T)
palette_size <- min(length(frecuencia$word), 9)
set.seed(1234)
windows(width = 60, height = 30)
wordcloud(words = frecuencia$word,
          freq = frecuencia$n,
          min.freq = 10,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.2,
          scale = c(3, 0.5),
          colors = brewer.pal(palette_size, "YlOrRd"))

palabras <- DF_stemmed%>%count(Fuente, word, sort=T)
Fuentes_con_protesta <- palabras %>% filter(word == "protest")
Fuentes_con_protesta

palabras <- DF_stemmed%>%count(Fuente, word, sort=T)
Fuentes_con_tractor <- palabras %>% filter(word == "trattor")
Fuentes_con_tractor

#####
## 4. Medida de similitud de noticias
library(widyr)
library(xtable)

TFIDF=DF_SS%>%count(ID, word, sort=T)%>% bind_tf_idf(word, ID, n) #Term Frequency-Inverse Document Frequency) para cada palabra en cada documento
TFIDF%>%arrange(desc(tf_idf)) #organizar de menor a mayor

comparisons <- TFIDF %>% pairwise_similarity(ID,word , tf_idf) %>%arrange(desc( similarity)) # Calcula la similitud coseno entre los documentos
comparisons

#####
## 5.Modelización textual
library("topicmodels")
library(ggplot2)
library(forcats)

# Construcción de la matriz de términos
matrix1 <- DF_stemmed %>% count(ID, word) %>%
  cast_dtm(document = ID, term = word, value = n, weighting = tm::weightTf)

# Aplicación del modelo LDA con método Gibbs
lda <- LDA(matrix1, k = 2, method = 'Gibbs', control = list(seed = 1111, alpha = 0.1)) # Con 2 topicos
betas <- tidy(lda, matrix = "beta") # probabilidades de los términos (beta) en cada topico

# Selección de los términos más importantes para cada topico o tema
grupo1=betas %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% #10 terminos con mayor probabilidad
  arrange(topic, -beta) %>%
  filter(topic == 1)  # Topico 1

grupo2=betas %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  arrange(topic, -beta) %>%
  filter(topic == 2) # Topico 2

#Visualizacion 
grupo1 %>% 
  mutate(name = fct_reorder(term, beta)) %>% 
  ggplot( aes(x=name, y= grupo1$beta)) +
  geom_bar(stat="identity", fill="#E05D00", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Palabras grupo 1") + ylab("Betas") +
  theme_bw()

grupo2 %>% 
  mutate(name = fct_reorder(term, beta)) %>% 
  ggplot( aes(x=name, y= grupo2$beta)) +
  geom_bar(stat="identity", fill="#324DA0", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Palabras grupo 2") + ylab("Betas") +
  theme_bw()

#####
## 6.Analisis sentimental

library("syuzhet") 
library(RColorBrewer)

sentiments <- get_nrc_sentiment(DF_SS$word, lang="italian") # Cargar lexicón NRC en italiano
get_sentiment_dictionary('nrc', language = "italian")

# Gafica de sentimientos
brewer.pal(n = 8, name = "Set3")
barplot(
  colSums(prop.table(sentiments[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 1.2,
  col =brewer.pal(n = 10, name = "RdYlBu"),
  main = "",
  sub = "",
  xlab="Emociones", ylab = NULL,
  names.arg=c("Ira", "Anticipación", "Disgusto", "Miedo", "Alegría", "Tristeza", "Sorpresa", "Confianza"))

# Nube de sentimientos
library(wordcloud)

DF_SS$word[sentiments$trust> 0]
DF_SS$word[sentiments$anticipation> 0]
DF_SS$word[sentiments$joy> 0]
DF_SS$word[sentiments$fear> 0]

nube_emociones_vector <- c( 
  paste(DF_SS$word[sentiments$trust> 0], collapse = " "),
  paste(DF_SS$word[sentiments$anticipation > 0], collapse = " "),
  paste(DF_SS$word[sentiments$sadness > 0], collapse = " "),
  paste(DF_SS$word[sentiments$fear >0], collapse = " ")) # Crea un vector de sentimientos

nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))

nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)
colnames(nube_tdm) <- c('Confianza', 'Anticipación', 'Tristeza', 'Miedo')
head(nube_tdm)

set.seed(1234)
windows(width = 30, height = 30)
comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = c("orange", "#497E00", "blue", "red"),
                 title.size = 1, max.words = 800, scale = c(1.5, 1), rot.per = 0.45)

######
## 7.Balance emocional vs. el tiempo narrativo 
DF <- DF %>% arrange(variable)

Noticias_SS=DF%>% unnest_tokens(token="sentences", word,Texto)%>% anti_join(custom_stopwords) # Tokenizacion por noticias 
sentiments2 <- get_nrc_sentiment(Noticias_SS$word, lang="italian") #Sentimientos por noticias
sentimientos_valencia <- (sentiments2$negative *-1) + sentiments2$positive

#Grafico A
L=rescale_x_2(sentimientos_valencia)
  plot.ts(L$z)
  
  library(zoo)
  suavizado_ma <- rollmean(L$z, k = 20, fill = NA)
  
  plot(L$z,  type = "l", col = "black", main = "")
  lines(suavizado_ma, col = "red")
  
#Grafico B
simple_plot(sentimientos_valencia, title = "", legend_pos = "top")
  
  

