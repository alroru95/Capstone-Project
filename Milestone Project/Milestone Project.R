---
title: 'Coursera Capstone Project: Milestone Report'
author: "A. Rodríguez"
date: "3/1/2021"
output:
  html_document: default
  pdf_document: default
---

## Introduction

The goal of this project is to display what I learnt on this program and the steps to create my prediction algorithm. This report explains exploratory analysis and goals for the eventual app and algorithm. What is shown in this project is: 
1. Demonstrate I downloaded data and have successfully loaded it in.
2. Create a basic report of summary statistics about the data sets.
3. Report any interesting findings amassed so far.
4. Get feedback on your plans for creating a prediction algorithm and Shiny app. 

## Data load.

```{r download}
URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(URL, "./Capstone Dataset.zip")
unzip("./Capstone Dataset.zip", exdir = "./Capstone Project")
```

The download of data into our directory is pretty straightforward, like in any other project done so far.

```{r load}
Blog_file <- file("./Capstone Project/final/en_US/en_US.blogs.txt")
Twitter_file <- file("./Capstone Project/final/en_US/en_US.twitter.txt")
News_file <- file("./Capstone Project/final/en_US/en_US.news.txt")
Blog <- readLines(Blog_file)
Twitter <- readLines(Twitter_file)
News <- readLines(News_file) ###Use default settings
close(Blog_file)
close(Twitter_file)
close(News_file)
```

If you want to avoid warnings, you can set warn = FALSE, although shown warnings aren't a big deal.

## General statistics.

### Word and line counts, data tables.

The table shown below contains the most basic features of the 3 datasets: words, lines, size, etc.

```{r table}
library(stringi)
Names <- c("Blog", "Twitter", "News")
Length <- c(length(Blog), length(Twitter), length(News))
Chars <- c(sum(nchar(Blog)),sum(nchar(Twitter)),sum(nchar(News)))
Max_chars <- c(max(nchar(Blog)),max(nchar(Twitter)),max(nchar(News)))
Median_chars <- c(median(nchar(Blog)),median(nchar(Twitter)),median(nchar(News)))
Words <- c(sum(stri_count_words(Blog)),sum(stri_count_words(Twitter)),sum(stri_count_words(News)))
Size <- c(file.info("./Capstone Project/final/en_US/en_US.blogs.txt")$size /1024^2, file.info("./Capstone Project/final/en_US/en_US.twitter.txt")$size /1024^2, file.info("./Capstone Project/final/en_US/en_US.news.txt")$size /1024^2) ###Divide by 1024^2 to get MB
Data_table <- data.frame("Name" = Names, "Nº Lines" = Length, "Nº Characters" = Chars, "Max char in a line" = Max_chars, "Median chars per line" = Median_chars, "Nº words" = Words, "Size in MB" = Size)
Data_table
```

### Exploratory Data Analysis

As these datasets are very large, we'll only take a sample from them (for example a 3% of each one) and join them in a single corpus with the same encoding (latin alphabet).

```{r sample}
set.seed(12345)
Blog_sample <- sample(Blog, length(Blog)*0.03)
Twitter_sample <- sample(Twitter, length(Twitter)*0.03)
News_sample <- sample(News, length(News)*0.03)
Total_sample <- c(Blog_sample, Twitter_sample, News_sample)
Corpus <- iconv(Total_sample, from = "UTF-8", to = "ASCII", sub = "")
```

Over this corpus, we'll perform some cleaning. On the order written here, the steps taken to tidy the dataset is: convert to lowercase, remove punctuation marks, numbers, stopwords, "profanity" and extra whitespaces done by these removals.

```{r clean}
library(tm)
Corpus <- VCorpus(VectorSource(Corpus))
Corpus <- tm_map(Corpus, content_transformer(tolower))
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus, removeWords, stopwords("english"))
Corpus <- tm_map(Corpus, stripWhitespace)
Corpus <- tm_map(Corpus, PlainTextDocument)
```

After having cleaned the corpus, it's time now to perform a few plots to ilustrate the most frequent n-grams (Unigrams, Bigrams, Trigrams and Quadgrams).

```{r tokenize}
library(RWeka)
library(tm)
unigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
unigram_tdm <- TermDocumentMatrix(Corpus, control = list(tokenize = unigram))
unigram_freq <- findFreqTerms(unigram_tdm, lowfreq = 30)

bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigram_tdm <- TermDocumentMatrix(Corpus, control = list(tokenize = bigram))
bigram_freq <- findFreqTerms(bigram_tdm, lowfreq = 30)

trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigram_tdm <- TermDocumentMatrix(Corpus, control = list(tokenize = trigram))
trigram_freq <- findFreqTerms(trigram_tdm, lowfreq = 5)

quadgram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
quadgram_tdm <- TermDocumentMatrix(Corpus, control = list(tokenize = quadgram))
quadgram_freq <- findFreqTerms(quadgram_tdm, lowfreq = 5)
```

After having tokenized the corpus by these n-grams, we now features a barplot for each n-gram.

```{r 1gram}
library(ggplot2)
unigram_freq_sums <- rowSums(as.matrix(unigram_tdm[unigram_freq,]))
unigram_ord <- order(unigram_freq_sums, decreasing = T)
unigram_freq_sums <- data.frame(word=names(unigram_freq_sums[unigram_ord]), frequency=unigram_freq_sums[unigram_ord])

ggplot(unigram_freq_sums[1:25,], aes(factor(word, levels = unique(word)), frequency)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Unigram") + ylab("Frequency")
```

```{r 2gram}
library(ggplot2)
bigram_freq_sums <- rowSums(as.matrix(bigram_tdm[bigram_freq,]))
bigram_ord <- order(bigram_freq_sums, decreasing = T)
bigram_freq_sums <- data.frame(word=names(bigram_freq_sums[bigram_ord]), frequency=bigram_freq_sums[bigram_ord])

ggplot(bigram_freq_sums[1:25,], aes(factor(word, levels = unique(word)), frequency)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Bigram") + ylab("Frequency")
```

```{r 3gram}
library(ggplot2)
trigram_freq_sums <- rowSums(as.matrix(trigram_tdm[trigram_freq,]))
trigram_ord <- order(trigram_freq_sums, decreasing = T)
trigram_freq_sums <- data.frame(word=names(trigram_freq_sums[trigram_ord]), frequency=trigram_freq_sums[trigram_ord])

ggplot(trigram_freq_sums[1:25,], aes(factor(word, levels = unique(word)), frequency)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Trigram") + ylab("Frequency")
```

```{r 4gram}
quadgram_freq_sums <- rowSums(as.matrix(quadgram_tdm[quadgram_freq,]))
quadgram_ord <- order(quadgram_freq_sums, decreasing = T)
quadgram_freq_sums <- data.frame(word=names(quadgram_freq_sums[quadgram_ord]), frequency=quadgram_freq_sums[quadgram_ord])

ggplot(quadgram_freq_sums[1:25,], aes(factor(word, levels = unique(word)), frequency)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Quadgram") + ylab("Frequency")
```

## Interesting findins

Using quadgrams to see which expression is more repeated is quite unuseful, as the expressions shown don't make much sense. Aside from quadgrams, when comparing the other barplots, we see that in 1-grams the frencuency decreases relative constant. However, in 2-grams and especially 3-grams the frecuency distribution is more unequal, as the top n-grams cover a bigger portion of this distribution.

## Feedback on my plans

Apart from realizing that analyzing quadgrams return expressions that doesn't make sense, they also indicate that, when cleaning the data, there should be a further remove of words aside from the aforementioned.
