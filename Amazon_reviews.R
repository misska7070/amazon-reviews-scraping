#Loading Packages
library(tidyverse)
library(robotstxt)
library(rvest)
library(RSelenium)
install.packages("RSelenium")
install.packages("rvest")
install.packages("tidytext")
library(tidytext)
install.packages('xml2')
library(xml2)

url <- "https://www.theguardian.com/international"
html <- read_html(url)


title <- html %>%
  html_elements(css = ".js-headline-text") %>%
  html_text()

view(title)

titlesOf=as_tibble(title)
View(titlesOf)

holder.tbl=titlesOf %>% distinct()

names(holder.tbl)= c("Text")

headlines <- holder.tbl %>% mutate(ID = seq_along(Text))

headlines <- filter(headlines,ID<7)
view(headlines)

#cleaning data

as_tibble(headlines)

tidy.headlines=headlines %>% unnest_tokens(word,Text)

View(stop_words)

tidy.headlines.2=tidy.headlines %>% anti_join(stop_words)

tidy.headlines.2

view(tidy.headlines.2)


freq.df=tidy.headlines.2 %>% count(word,sort=TRUE)

freq.df

View(freq.df)

#EXERCISE 4

# set up connection and start browser to navigate the page
rD <- rsDriver(verbose = TRUE, browser = "firefox", port = 4444L)

rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]

url <- "https://www.premierleague.com/stats/top/players/yellow_card"
remDr$navigate(url)


# identify and click the season list button
list <- remDr$findElement(using = "css", value = "div.dropDown:nth-child(2) > div:nth-child(2)")
list$clickElement()

# identify and click on 2019/2020
elem <- remDr$findElement(using = "css", value = "div.dropDown:nth-child(2) > ul:nth-child(3) > li:nth-child(4)")
elem$clickElement()

# identify and click the position list butto
#list <- remDr$findElement(using = "css", value = "div.dropDown:nth-child(3) > div:nth-child(2)")
#list$clickElement()


# identify and click the club button
list <- remDr$findElement(using = "css", value = "div.dropDown:nth-child(3) > div:nth-child(2)")
list$clickElement()

# identify and click on AstonVilla
elem <- remDr$findElement(using = "css", value = "div.dropDown:nth-child(3) > ul:nth-child(3) > li:nth-child(3)")
elem$clickElement()


# save the live DOM tree
output <- remDr$getPageSource(header = TRUE)
write(output[[1]], file = "players.html")



# parse data
players <- read_html("players.html", encoding = "utf-8")
filtered_table <- players %>%
  html_element("table") %>%
  html_table()


View(filtered_table)

# close the connection
remDr$close()
rD$server$stop()




