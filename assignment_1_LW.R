##################################
### Code Completion Assignment ###
### Solution by: Lena Wagner #####
### ID: 206246, Fall 2021/22 #####
##################################

# Your goal is to complete the following skeleton code so that the script can be executed
# the three *** indicate where you need to to insert the correct code 

# stringr & regular expressions -------------------------------------------
addresses <-c("221B Baker St., London", "1600 Pennsylvania Avenue, Washington D.C.", 
              "742 Evergreen Terrace, Springfield")

products <- c("TV  ", " laptop", "portable  charger", "Wireless Keybord",
              "   HeadPhones   ")

sentences <- stringr::sentences[1:20]

field_names <- c("order_number", "order_date", "customer_email", "product_title", "amount")

email <- c("tom@hogwarts.com",
           "tom.riddle@hogwarts.com",
           "tom@hogwarts.eu.com",
           "potter@hogwarts.com",
           "harry@hogwarts.com",
           "hermione+witch@hogwarts.com")

files <- c(".bash_profile",
           "workspace.doc",
           "img0912.jpg",
           "updated_img0912.png",
           "documentation.html",
           "favicon.gif",
           "img0912.jpg.tmp",
           "access.lock")

#convert addresses to lower-case.
library(stringr)
str_to_lower(string = addresses)

#extract digits from addresses
str_extract_all(string = addresses, pattern = "[:digit:]", simplify = T)

#split addresses into two parts: street & city
str_split(string = addresses, pattern = "\\,", simplify = T)

#split addresses into three parts: house number, street & city
str_split(string = addresses, pattern = "(?<=\\dB)|\\,|(?<=\\d{1,4})\\s", simplify = T)

#For sentences that end with the letter t, extract the last word
str_extract(string = sentences, pattern = "[:alnum:]+t.$")

#Extract the first 30 characters from each sentence
str_trunc(string = sentences, 30, ellipsis = "...")

#replace all underscores in field_names with spaces and capitalize the first letter of each word
str_replace_all(string = field_names, "_", " ") %>% 
  str_to_title()

#extract names appearing before @ from email 
str_extract(email, "([\\w\\.\\-_+]+)?\\w+[^@]")

#extract the three images (.jpg, .png, .gif) from files
str_extract(files, "\\w+.jpg|\\w+.png|\\w+.gif")


# web scraping & tidytext -------------------------------------------------
library(rvest)
library(tidytext)
library(tibble)
library(dplyr)

#extract the text of J.K. Rowling's commencement speech at Harvard University
url <- "https://news.harvard.edu/gazette/story/2008/06/text-of-j-k-rowling-speech/"
speech <- read_html(url) %>%
  html_elements(".article-body.basic-text.do-drop-cap p") %>%
  html_text2()

#convert the text to a tibble, remove the first line ("Text as Delivered") 
# and the 15th line ("Sign up for daily emails to get the latest Harvard news.")
# and add a column with a number for each paragraph
speech_df <- 
  tibble(text = speech) %>%
  slice(-1) %>%
  slice(-14) %>%
  mutate(id = row_number())

View(speech_df)

#tokenize the text into words and remove stop words
speech_df <- speech_df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

#list the 5 most frequent words
speech_df %>% 
  count(word, sort = TRUE) %>%
  top_n(5)

#Answer:life, failure, day, people, parents

# Reading in text ---------------------------------------------------------
library(readtext)
library(quanteda)

#read in the UK manifestos from the course's data folder, 
#create document names for party and year from file names
setwd("C:/Users/lena_/OneDrive/Desktop/MPP Hertie/21-22_Fall/TextAsData/Text_as_Data_21")

manifestos <- readtext("data/UK_manifestos/*.txt", 
                       docvarsfrom = "filenames",
                       dvsep = "-", 
                       docvarnames = c("party", "year")
                       )

# create a corpus out of the documents
manifesto_corpus <- corpus(manifestos)
summary(manifesto_corpus)

# overwrite the party column with these new labels: Conservatives, Labour, LibDems
party <- c("Conservatives", "Labour", "LibDems")
docvars(manifesto_corpus, "party") <- factor(manifesto_corpus$party, labels = party)
summary(manifesto_corpus)

#tokenize the text into words, remove numbers and punctuations, and convert it to lower case
#and find out in which context the parties mention words containing europe
manifesto_tokens <-
  tokens(manifesto_corpus,
         remove_punct = T,
         remove_numbers = T) %>%
  tokens_tolower()
  
kwic(manifesto_tokens, "europe", window = 5)

#now remove stop words from the text using the smart source 
#and tokenize the manifestos into bigrams 
manifesto_bigrams <- manifesto_tokens %>% 
  tokens_remove(pattern = stopwords("en", source = "smart")) %>% 
  tokens_ngrams(n=2)

#create a document feature matrix from the manifesto bigrams 
#and keep only the party manifestos from the Liberal Democrats
manifesto_dfm <- dfm(manifesto_bigrams) %>% 
  dfm_subset(party == "LibDems")

#find out how many bigrams appear only once
bigram_freq <- sum(featfreq(manifesto_dfm) == 1)
#Answer: 3453 bigrams appear only once
