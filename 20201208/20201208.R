# install.packages("GGally")
# install.packages("ggtext")
# install.packages("ggridges")
# install.packages("hrbrthemes")
install.packages("rvest")
install.packages("tokenizers")
install.packages("tidytext")
install.packages("stopwords") 
library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(rvest)
library(tokenizers)
library(tidytext)
library(stopwords)
library(SnowballC) #for stemming

rm(list=ls(all=TRUE))

women <- tidytuesdayR::tt_load('2020-12-08')$women

#Exploration
  women$description[24]
  table(women$category)
  table(women$role)
  table(women$country)


# Getting tokens (by category), removing stopwords, stemming
descr_token_wostop<-women %>%
    select(category, description) %>%
    # replace co-founder by cofounder
    mutate(description = gsub("woman", "women", description)) %>%
    mutate(description = gsub("co-founder", "cofounder", description)) %>%
    mutate(description = gsub("’", "", description)) %>%
    mutate(description = gsub("'", "", description)) %>%
    unnest_tokens(word, description)%>% 	
    filter(!(word %in% stopwords(source = "snowball"))) %>%
    #also removing: year, name, dr, work and numbers (because not super informative)
    filter(!(word %in% c("year", "around", "name","dr", "work", "19", "22", "23", "2020"))) %>%
    mutate(stem = wordStem(word)) %>%
    group_by(category) %>%
    count(stem) %>%
    arrange(desc(n)) %>%
    filter(n>1) %>% #keeping only those stems mentioned more than once 
    slice(1:10)

#Plotting
plot<-ggplot(
  descr_token_wostop,
  aes(
    label = stem, size = n,
    x=category, color = category)
  ) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 7) +
  scale_color_carto_d(palette="Bold") +
#Layouting
  theme_minimal() +
  coord_flip() +
  theme(aspect.ratio = 1.5,
      text = element_text(family = "Andale Mono"), legend.position = "none", # change all text font and remove the legend
      panel.grid = element_line(color="white"),  # change the grid color and remove minor y axis lines
      plot.caption = element_text(hjust = 0, size = 9, color = "#11A579"),
      plot.title = element_text(size = 18), plot.subtitle = element_markdown(size=9, family = "Helvetica", color = "#11A579"),
      axis.text=element_text(size=14)) +
  # title
  labs(title = "Describing inspiring women",
       subtitle = "with the top 10 word stems by category from the BBC 2020 list",
       x = NULL, y=NULL,
       caption = "DataViz by @SylviRz for #TidyTuesday, data from BBC")
plot
ggsave("describing_influential_women.png", width=5.5, height=8)


