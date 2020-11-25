install.packages("GGally")
install.packages("ggtext")
install.packages("ggridges")
install.packages("hrbrthemes")
library(tidytuesdayR)
library(ggplot2)
library(GGally)
library(ggtext)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(purrr)

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
        #strip.background = element_rect(colour="white", fill="#FFFFFF"), # removing the gray facet_wrap box
        plot.title = element_text(size = 24), 
        plot.subtitle = element_markdown(size=8, family = "Helvetica", color = "bisque4")) + # make the title bigger and edit the subtitle (font)
  # title
  labs(title = "On a mountain high",
       subtitle = "Mountain Views, dogs on a leash allowed, and Wildflowers top 3 features of Washington Trails.", 
       x = "Most frequent features of hikes", y=NULL,
       caption = "DataViz by @SylviRz for #TidyTuesday, Data: Washington Trails Association")
plot4
ggsave("mountainhigh1.png")

# to do:
#   - subtitle line break
# - order the hike features a bit differently OK
# - include features as axis labels for the numeric var... differnt---

# 3 learnings
 #scale_x_discrete(guide = guide_axis(n.dodge = 3)) -- to have the xaxis labels spread out on 2 rows
 #tidyr::unnest(features, keep_empty=TRUE) -- to turn a list into a variable, 
    #option keep_empty has FALSE as default, make sure to put keep_empty=TRUE
 # within ggplot use fct_infreq() to order a factor from most common to least common, place fct_rev() to order ascendingly

#old:
hike_data_clean<- hike_data %>%
  tidyr::unnest(features, keep_empty=TRUE) 
%>%
  group_by(name, location) %>%
  mutate(number_features=n()) %>%
  ungroup() %>%
  group_by(as.factor(features)) %>%
  mutate(n=n()) %>%
  ungroup()

#Short data 2
hike_data_short2<-hike_data_clean%>%
  select(features, n) %>%
  pivot_wider()

#trying to shuffle the data a bit differently... !? TO Do!
set.seed(543)
roworder<-sample(nrow(hike_data_short2))
hike_data_short3 <- hike_data_short2[roworder,]


#Short data. 
hike_data_short<-hike_data_clean %>%
  select(name, location, rating, number_features) %>%
  mutate(rating=as.numeric(rating)) %>%
  filter(number_features<10) %>%
  pivot_wider()

summary(hike_data_clean)

table(hike_data_clean$features)



plot1<-ggplot(hike_data_clean, aes(x=fct_infreq(features))) + #learning: fct_infreq(features)
  geom_bar(stat="count") +
  coord_flip( )
plot1



plot3<-ggplot(hike_data_clean, aes(x=features)) + #learning: fct_infreq(features)
  geom_density() 
plot3

plot2<-ggplot(hike_data_short, aes(y=rating, x=number_features)) +
                geom_count() +
  geom_smooth()
+
  # edit the theme
  theme(text = element_text(family = "Andale Mono"), legend.position = "none", # change all text font and move the legend to the bottom
        panel.grid = element_line(color="white"),  # change the grid color and remove minor y axis lines
        plot.caption = element_text(hjust = 0.5, size = 8, color = "#29AF7FFF"), # remove x-axis text and edit the caption (centered and brown)
        strip.background = element_rect(colour="white", fill="#FFFFFF"), # removing the gray facet_wrap box
        plot.title = element_text(size = 24), plot.subtitle = element_markdown(size=9, family = "Helvetica", color = "#29AF7FFF")) +# make the title bigger and edit the subtitle (font)
  # title
  labs(title = "Call me mobile",
       subtitle = "In 2016, 30% of Africans and 90% of Europeans have at least one mobile phone subscription.  ",
       x = "Number of subscriptions per person", y=NULL,
       caption = "data from OurWorldInData.org")
plot2

#notion app 
#workflow 

scale_colour_gradientn()

Von Andreas an Alle: (9:59 PM)
https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
# the code for getting length in numeric, found it on the GitHub page of tidytuesday
>as.numeric(gsub("(\\d+[.]\\d+).*","\\1", hike_data$length))