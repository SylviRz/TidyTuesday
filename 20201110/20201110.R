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

tuesdata <- tidytuesdayR::tt_load('2020-11-10')

mobile <- tuesdata$mobile
landline<- tuesdata$landline


#browsing
summary(mobile)
summary(landline)

summary(mobile$mobile_subs)
summary(landline$landline_subs)


#Final Code
#Prep
mobile_afreuro<-mobile %>%
  filter(continent %in% c("Africa","Europe") & year %in% c(1990,1995,2000,2005, 2010, 2016)) %>%
  #orginal data: per 100 -- dividing by 100 to make a per person figure out of it.
  mutate(mobile_pp=mobile_subs/100) %>%
  select(code, continent, year, mobile_pp)%>%
  pivot_longer(cols=continent)

#Summary Statistics
mobile_afreuro%>% 
          group_by(value, year ) %>%
          summarise(
          q10=quantile(mobile_pp,probs=(.1), na.rm=TRUE),
          q25=quantile(mobile_pp,probs=(.25), na.rm=TRUE), 
          median=median(mobile_pp, na.rm=TRUE),
          q70=quantile(mobile_pp,probs=(.7), na.rm=TRUE),
          q75=quantile(mobile_pp,probs=(.75), na.rm=TRUE))  

#Final Plot
plot5<-ggplot(mobile_afreuro, aes(x =mobile_pp , y =  as.factor(year), fill = as.factor(year))) +
  geom_density_ridges(alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ridges() + 
  facet_wrap(~value) + # and country?
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
plot5
ggsave("callMeMobile3.png")

