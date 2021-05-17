install.packages("GGally")
install.packages("ggtext")
install.packages("ggridges")
install.packages("hrbrthemes")
library(tidytuesdayR)

library(GGally)
library(ggtext)
library(ggridges)
library(viridis)
library(tidytuesdayR)
library(ggplot2)
library(hrbrthemes)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2020-11-24')

hike_data <- tuesdata$hike_data

#hike_data <- hike_data %>% 
#  mutate(number_features = purrr::map_int(features, length))

hike_data_clean<- hike_data %>%
  tidyr::unnest(features) 


#This will be the final plot
plot4<-ggplot(hike_data_clean, aes(x=fct_rev(fct_infreq(features)))) + # ordered ascendingly
   geom_bar(stat="count", fill="bisque3", color="bisque4") +
  # Highlighting just a couple of features
   scale_x_discrete(labels=c("Coast", "", "", "", "", "", "Ridges/Passes", "", "","", "","","", "", "Mountain \n Views")) +
  # edit the theme
  theme(text = element_text(family = "Andale Mono"), legend.position = "none", # change all text font and move the legend to the bottom
        panel.grid = element_line(color="white"),  # change the grid color and remove minor y axis lines
        plot.caption = element_text(hjust = 0.5, size = 8, color = "bisque4"), # remove x-axis text and edit the caption (centered and brown)
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill="lightblue"),
        plot.title = element_text(size = 24), 
        plot.subtitle = element_markdown(size=8, family = "Helvetica", color = "bisque4")) + # make the title bigger and edit the subtitle (font)
  # title
  labs(title = "On a mountain high",
       subtitle = "Mountain Views, dogs on a leash allowed, and Wildflowers top 3 features of Washington Trails.", 
       x = "Most frequent features of hikes", y=NULL,
       caption = "DataViz by @SylviRz for #TidyTuesday, Data: Washington Trails Association")
plot4
ggsave("mountainhigh1.png")
