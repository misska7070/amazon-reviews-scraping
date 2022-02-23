# Libraries ----
library(tidyverse)
library(tidytext)
library(rvest)
library(robotstxt)
library(xml2)
library(RSelenium)
library(jsonlite)
library(ggplot2)
library(igraph)
library(ggraph)
library(scales)
library(tidytext)
library(wordcloud2)
library(wordcloud)
library(udpipe)
library(textplot)
library(corrplot)
library(quanteda)
library(quanteda.textmodels)
library(caret)
library(cld2)
library(SnowballC)
library(reshape2)




# function ----
amazon_reviews <- function(id, page) {
  
  url <- paste0("https://www.amazon.co.uk/product-reviews/1847941435/ref=cm_cr_othr_d_show_all_btm?ie=UTF8&reviewerType=all_reviews",
                id, "/?pageNumber=", page)
  html <- read_html(url)
  
  # Review title (UK and not-UK)
  title = html %>%
    html_elements("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text2()
  
  title = title %>%
    c(html %>%
        html_elements("[class='a-size-base review-title a-color-base review-title-content a-text-bold']") %>%
        html_text2())
  
  # Review text (the same for UK and not-UK)
  text = html %>%
    html_elements("[class='a-size-base review-text review-text-content']") %>%
    html_text2()
  
  # Review stars (UK and not-UK)
  star = html %>%
    html_elements("[data-hook='review-star-rating']") %>%
    html_text2()
  
  star = star %>%
    c(html %>%
        html_elements("[data-hook='cmps-review-star-rating']") %>%
        html_text2())
  
  # Return a tibble
  tibble(title, text, star, page = page) %>%
    return()
}

# scraping ----
id = "0300188226"
page = 1:49
data = map_df(page, ~amazon_reviews(id = "0300188226", page = .))


data$doc_id = 1:nrow(data) 
save(data, file = "data.rda")

load("data.rda")
data

view(data)


# DATA CLEANING


#Check the language 




library(cld2)

data$title_lang=detect_language(data$title)
data$text_lang=detect_language(data$text)


table(Text=data$text_lang,Title=data$title_lang,useNA="always")

#Code for filtering only English reviews
data=data %>% 
  filter (text_lang=="en")
data

# Extract the star

data = data %>% 
  mutate(score=as.numeric(substring(star,1,1)))

# analyse the score

summary(data$score)

data %>% count(score) %>% 
  mutate(p=round(n/sum(n),2))




#Graph depicting the number of stars
data %>% 
  ggplot(aes(x=score)) + 
  geom_bar(aes(y = (..count..)),fill="steelblue")+
  labs(title="Amazon reviews' stars",
       subtitle = "The Theory That Would Not Die, by Mcgrayne",
       x ="Stars", 
       y = "Number of comments")+
  theme_bw()+
  theme(plot.title = element_text(color = "steelblue", 
                                  size = 12,
                                  face = "bold"),
        plot.subtitle = element_text(color = "steelblue2"))



#Classifying into positive and negative sentiment

data=data %>% 
  mutate(star_sent=ifelse(score>=4,"positive","negative"))
data %>% count(star_sent) %>%
  mutate(p=n/sum(n))

#Comparing the length of negative and positive element

data$nchar=str_length(data$text)
ggplot(data, aes(x=star_sent, y=nchar, fill=star_sent)) + 
  geom_boxplot() +
  theme_bw()+  
  scale_fill_manual(values=c("steelblue","skyblue"))





#### tokenization 

library(tidytext)
tidy_text <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

library(SnowballC)
tidy_stem <- tidy_text %>%
  mutate(word = wordStem(word))

tidy_lemma <- udpipe(data, "english-gum")


tidy_lemma = tidy_lemma %>%
  mutate(stem = wordStem(token)) %>%
  tibble()
tidy_lemma