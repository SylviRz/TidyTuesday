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
library(GGally)
#library(ggtext)
#library(ggridges)
library(ggplot2)
#library(viridis)
#library(hrbrthemes)
library(tidyverse)
library(rvest)
library(tokenizers)
library(tidytext)
library(stopwords)
#library(hcandersenr)



rm(list=ls(all=TRUE))


women <- tidytuesdayR::tt_load('2020-12-08')$women

#Exploration
  women$description[24]
  table(women$category)
  table(women$role)
  table(women$country)
  tokenize_words(women$description) %>%
    
install.packages("SnowballC")
  library(SnowballC) #for stemming
  
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
    #also removing: year
    filter(!(word %in% c("year", "around", "name","dr", "work", "19", "22", "23", "2020"))) %>%
    mutate(stem = wordStem(word)) %>%
    group_by(category) %>%
    count(stem) %>%
    arrange(desc(n)) %>%
    filter(n>1) %>% #keeping only those stems mentioned more than once 
    slice(1:10)

#Plotting
plot4<-ggplot(
  descr_token_wostop,
  aes(
    label = stem, size = n,
    x=category, color = category
  )
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 6) +
  #scale_x_discrete(breaks = NULL) +
  #Layouting
  theme_minimal() +
  coord_flip() +
  theme(aspect.ratio = 1.5,
      text = element_text(family = "Andale Mono"), legend.position = "none", # change all text font and move the legend to the bottom
      panel.grid = element_line(color="white"),  # change the grid color and remove minor y axis lines
      plot.caption = element_text(hjust = 0, size = 8, color = "#29AF7FFF"), # remove x-axis text and edit the caption (centered and brown)
      plot.title = element_text(size = 24), plot.subtitle = element_markdown(size=9, family = "Helvetica", color = "#29AF7FFF")) +# make the title bigger and edit the subtitle (font)
  # title
  labs(title = "Describing infuential women",
       subtitle = "with the top 10 word stems by category",
       x = NULL, y=NULL,
       caption = "DataViz by @SylviRz for #TidyTuesday, data from BBC")
plot4
ggsave("describing_influential_women.png", width=5, height=6)

#To Do 
  # nice colors



install.packages("wordcloud")
library(wordcloud)
plot3<-wordcloud(descr_token_wostop$stem, descr_token_wostop$n,scale=c(2,.1))
plot3

plot4<-comparison.cloud()

install.packages("ggwordcloud")
library(ggwordcloud)


## Plotting
install.packages("VennDiagram")
library(VennDiagram)


plot2<-venn.diagram(
  x = list(descr_token_wostop$stem[1:5], descr_token_wostop$stem[6:10], descr_token_wostop$stem[11:15], descr_token_wostop$stem[16:20],descr_token_wostop$stem[21:25]),
  category.names = c("All" , "Creativity" , "Identity", "Knowledge", "Leadership"),
  filename = '#14_venn_diagramm.png',
  output=TRUE)
plot2 

# expect a warning about rows with missing values being removed
plot1<-ggplot(descr_token_wostop, aes(x = n, y = stem, 
                      color = abs(stem - n))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = stem), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~category, ncol = 2) +
  theme(legend.position="none") 
plot1
  
library(RColorBrewer)
myCol <- brewer.pal(5, "Pastel2")

venn.diagram(
  x = list(
    descr_token_wostop %>% filter(category=="All") %>% select(stem) %>% unlist() , 
    descr_token_wostop %>% filter(category=="Creativity") %>% select(stem) %>% unlist() , 
    descr_token_wostop %>% filter(category=="Identity") %>% select(stem) %>% unlist(),
    descr_token_wostop %>% filter(category=="Knowledge") %>% select(stem) %>% unlist() , 
    descr_token_wostop %>% filter(category=="Leadership") %>% select(stem) %>% unlist()
  ),
  category.names = c("All" , "Creativity" , "Identity", "Knowledge", "Leadership"),
  filename = 'venn.png',
  output = TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  
  # Circles
  compression = "lzw",
  lwd = 1,
  col=myCol,
  fill = myCol,
  
  # Numbers
  cex = 0.5,
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.3,
  cat.default.pos = "text",
  #cat.pos = c(-27, 27, 135),
  #cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  cat.col = myCol,
  #rotation = 1
)
  



# Removing stop words
  
# Graphing top 5 or top 10 words by category  
