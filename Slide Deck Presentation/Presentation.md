Coursera: Data Science Capstone Project (Final Presentation)
========================================================
author: A. Rodriguez
date: February 5th, 2021
autosize: true

Introduction 
========================================================

The purpose of this project was to develop an application, in order to predict the next word after having entered a word or phrase (using an electronic device). The main demanded features of the app would be:

- Light, so it can be ran in any normal electronic device. 
- Based on Natural Language Processing rule.
- Being easy to use for anyone.


Data
========================================================

The Corpora (texts), granted by Swiftkey, was publicly available on a web page. There were four versions in different languages; although the application uses the English one only. The data was divided in news, blog, and twitter texts.  

To carry on this project, it was necessary to clean the data, removing strange punctuation, excessive whitespace, profanity, and other non-text elements (such as numbres. This purified texts were merged into a single corpus and then tokenized into n-grams.


Algorithm to build n-grams
========================================================

The algorithm used was The Katz Backoff Model. This model predicts the probability of a word being the next one after inputting a text, by comparing what has already been entered against a set of ngrams (quadgrams, trigrams, and bigrams).

If a match isn't found by comparing a four word phrase to a set of quadgrams, the last three words would then be used against a set of trigrams, and so on. Therefore, "backing off" until an appropriate next word is found.

The code used for this project, can be found in my github repository:
https://github.com/alroru95/Capstone-Project


User Interface
========================================================



The user interface (as aforementioned in the "Introduction" slide) was meant to be easy and intuitive. Any user not expert in the subjet can use it by entering a word or phrase in the text box. The suggested words will appear below it. In case of any doubt, there are instructions to use it.
