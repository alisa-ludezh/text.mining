## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions
knitr::opts_chunk$set(echo = TRUE)
library(tidytext)
library(stringr)
library(scales)
library(forcats)
library(wordcloud)
devtools::install_github("cpsievert/LDAvisData")
library(LDAvis)
library(jsonlite)
library(tm)
## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

folder = "../input/bbc news summary/BBC News Summary/"

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

txt_from_folder <- function(subfolder) {
    path = str_c(folder, subfolder)
    print(path)
    files <- list.files(path, pattern = "txt") #список файлов в папке
    
    for (i in (1:length(files))) {
        filename <- str_c(path, files[i])
        lines <- read_lines(filename,
                               locale = locale(encoding = "UTF-8"))
        lines <- lines[lines != '']
        text <- data_frame(doc_name = str_c(str_replace(files[i],".txt",""), subfolder), 
                             line = 1:length(lines), text = lines, theme = subfolder)

        if (i==1) {texts <- text}
        else {texts <- union_all(texts, text)}
     }

    texts
}

## Загрузка для начала двух тем: политика и бизнес

docs_politics <- txt_from_folder("News Articles/politics/")
docs_business <- txt_from_folder("News Articles/business/")

head(docs_politics)

full_df <- union_all(docs_business, docs_politics)

articles <- full_df %>%
  unnest_tokens(word, text)


articles %>%
  count(word, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  ungroup()

### Так, в первом десятке самых часто встречающихся слов - только слова общего назначения, всякие артикули.

#### Удалим их, используя список stop_word из пакета tidytext:

tidy_articles <- articles %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%
    anti_join(stop_words) %>%
    filter(str_detect(word, "[a-z']"))

tidy_articles %>%
  group_by(theme) %>%
  count(word, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = theme)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Most frequently words in theme",
       x = "Words", y = "Amount of words in theme",
       fill = "Papers")

### Кроме того, нам позарез необходимо построить облако весело раскрашенных тэгов:

tidy_articles %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Dark2")))


### Посчитали здесь корреляцию тем и сделали из этого какой-нибудь вывод:

frequency <- tidy_articles %>%
  count(theme, word) %>%
  group_by(theme) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(theme, proportion, fill = 0)
head(frequency)

cor.test(data = frequency, 
         ~ `News Articles/business/` + `News Articles/politics/`)

### Строим частотный график для тем до и после удаления стоп-слов:

article_words <- full_df %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  filter(!is.na(word))

word_freq <- article_words%>%
  count(theme, word) %>%
  group_by(theme) %>%
  mutate(freq = n / sum(n)) 

word_freq2 <- tidy_articles %>%
    mutate(theme = str_c(theme,  " without stop words")) %>%
  count(theme, word) %>%
  group_by(theme) %>%
  mutate(freq = n / sum(n))
    


union_all(word_freq, word_freq2) %>% 
  ggplot(aes(freq, fill = theme)) +
  geom_histogram(show.legend = FALSE, bins = 40) +
  #xlim(NA, 0.006) +
  facet_wrap(~theme, ncol = 2) +
  labs(title = "Word frequency distribution",
       y = "Amount of words", x = "Frequency")

### Сейчас будем считать метрику tf-idf, которая является мерой того, насколько слово уникально для своей темы. 

#### Так и узнаем, какие слова являются маркерами темы

word_tf_idf <- word_freq %>%
  bind_tf_idf(word, theme, n) %>%
  select(-freq) 

head(word_tf_idf)

word_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word, tf_idf), tf_idf, fill = theme)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf", fill = "Papers", title = "The most important words in themes") +
  coord_flip()

### Что ж, пришло время подкинуть данные в наш корпус документов.

### Добавим остальные три темы:

new_full_df <- rbind(full_df
                         ,txt_from_folder("News Articles/tech/")
                         ,txt_from_folder("News Articles/entertainment/")
                         ,txt_from_folder("News Articles/sport/"))

### Ранее мы строили частотный график Word frequency distribution, и он неудовлетворительный, ### все равно распределение частот очень скошенное.

#### Отличное решение: надо сформировать свой кастомный список стоп-слов!: считается (как в тренинге), что туда #### попадают слова, которые присутствуют во всех темах, и с высокой частотой

words_tf_idf <- new_full_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  filter(!is.na(word)) %>%
  count(theme, word) %>%
  bind_tf_idf(word, theme, n) %>%
  group_by(theme) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  arrange(desc(tf_idf))

custom_stop_words <- words_tf_idf %>% 
  filter(idf==0) %>%
  group_by(word) %>%
  summarise(frequency = sum(n)/sum(total)) %>%
  filter(frequency > 0.00025) %>%
  arrange(desc(frequency))

print(custom_stop_words)

tidy_articles <- new_full_df %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% custom_stop_words$word) %>%
    filter(!word %in% stop_words$word) %>%
    filter(str_detect(word,"[a-z]"))

## Теперь сложный перформанс: построение карты признакового пространства
### Используем визуальную библиотечку serVis для построения тем чисто по лексике, без указания ### заголовков.
#### Необходимо сделать ряд преобразований, сначала - сформировать словарь всех-всех слов, без повторений. #### Это будут измерения для нашего признакового пространства

# compute the table of terms:
term.table <- table(unlist(tidy_articles$word))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

### Теперь каждое слово всех документов кодируем в соответствии со словарем - подписываем однёрки

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x$word, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(split.data.frame(tidy_articles, tidy_articles$doc_name), get.terms)

# Вычисляем некоторые статистики
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]
print(c(D, W, N))

### Строим модель LDA - Латентное размещение Дирихле.
### Это т.н. порождающая модель - она как бы преполагает причины сходства некоторых частей данных. Её основные параметры - корпус документов в векторном пространстве, и число топиков, которое мы пожелаем.
### Используем её прямо из коробки. Она некоторое время обсчитывает данные, поэтому я её создала и она у нас лежит забэкапленная в файле, чтобы лишний раз не пересчитывать
### Хочется проверить, сможет ли lda прямо по тексту определить исходные темы, если я ей закажу 5 топиков прям как у нас исходно было

K <- 5
G <- 1000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)

if (!file.exists("../working/son.json")) {
    print("GO")
    t1 <- Sys.time()
    fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                       num.iterations = G, alpha = alpha, 
                                       eta = eta, initial = NULL, burnin = 0,
                                       compute.log.likelihood = TRUE)


    t2 <- Sys.time()
    t2 - t1  # about 24 minutes on laptop

    theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
    phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

    lda_data <- list(phi = phi,
                         theta = theta,
                         doc.length = doc.length,
                         vocab = vocab,
                         term.frequency = term.frequency)

    # create the JSON object to feed the visualization:
    json2 <- createJSON(phi = lda_data$phi, 
                       theta = lda_data$theta, 
                       doc.length = lda_data$doc.length, 
                       vocab = lda_data$vocab, 
                       term.frequency = lda_data$term.frequency)

      write(json2, "../working/son.json")
} else {
    print("ne GO")
    json2 <- read_file("../working/son.json")
}

# И опубликуем модель
serVis(json2, out.dir = "./", open.browser = F)

system("mv index.html results.html")

### Короче она [смогла](https://www.kaggleusercontent.com/kf/8768071/eyJhbGciOiJkaXIiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0..A8v8qZ3kkJngaUXBaA5Law.m23pzZ7IfWd0Qao8XV72MDQPU4LOnFbG_4i8eshDkBGFkIhmx46tPqrqKuX5mBtKECRCHX0VgXMLVoFXP766kCuIqCrIUS6j75QGB7MXB0qNuU9hRxobtJBCgWiraU7_NyN-EUdcakhBbHtBEql8VUtvMeBEVfFbqjEr6ST7ZBk.0sJv1x79TJ1xBBtXthLk2g/results.html) 
#### Ссылка правда не работает отсюда - в папке output файл results.html

## В таких темах как бизнес и политика вообще непонятно что значат слова. Поэтому очень кстати будет разбиение на биграмы

doc_bigrams <- new_full_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 

head(doc_bigrams)

# Cleanse
bigrams_separated <- doc_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(str_detect(word1,"[a-z]") | str_detect(word2,"[a-z]"))

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

head(bigram_counts)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(theme, bigram) %>%
  bind_tf_idf(bigram, theme, n) %>%
  arrange(desc(tf_idf))

head(bigram_tf_idf)

### Урра, картинка!

bigram_tf_idf %>%
  group_by(theme) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(bigram, tf_idf), y = tf_idf, fill = theme)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~theme, scales = "free", ncol = 2) +
  labs(x = NULL, y = "tf-idf", title = "The most importatnt bigrams in papers") +
  coord_flip()

### Надо заметить, что сейчас это выглядит гораздо, гораздо более внятно чем с отдельными словами

