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

#Plotting
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


# mobile_clean <- mobile %>%
#   mutate(entity=as.factor(entity),
#          code=as.factor(code),
#          continent=as.factor(continent))
# levels(mobile_clean$entity)
# 
# mobile_clean %>% select(-c(entity, code)) %>% ggpairs()
# 
# plot1=ggplot(mobile_clean, aes(x=year, y=mobile_subs)) +
#   geom_point()
# plot1
# 
# mobile_clean %>% select(-c(entity, code), continent=="Africa") %>% ggplot( aes(x=year, y=mobile_subs)) +
#   geom_point()
# 
# 
# mobile_africa<-mobile_clean %>%
#   filter(continent=="Africa" & year<2017) %>%
#   #group_by(code) %>%
#   #arrange(code, year) %>%
#   #mutate(mobile_subs100=mobile_subs/total_pop*100) %>% necessary?
#   #mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
#   #      Diff_growth = mobile_subs - lag(mobile_subs), # Difference in route between years
#   #      Rate_percent_mb = (Diff_growth / Diff_year)/mobile_subs * 100) %>% # growth rate in percent
#   mutate(mobile_pp=mobile_subs/100) %>%
#   select(code, year, mobile_pp)
# 
# 
# landline_africa<-landline %>%
#   filter(continent=="Africa" &  year<2017) %>%
#   # group_by(code) %>%
#   # arrange(code, year) %>%
#   # mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
#   #        Diff_growth = landline_subs - lag(landline_subs), # Difference in route between years
#   #        Rate_percent_ll = (Diff_growth / Diff_year)/landline_subs * 100) %>% # growth rate in percent
#   mutate(landline_pp=landline_subs/100) %>%
#   select(code, year, landline_pp) 
# 
# 
# 
# all_lines_africa<-full_join(mobile_africa, landline_africa, by=c("code", "year"))
# 
# #long format
# all_lines_africa_l<-pivot_longer(all_lines_africa, cols=mobile_pp:landline_pp) %>%
#   mutate(name=factor(name, labels=c("Landline Subscribers", "Mobile Subscribers")))
# 
# 
# # all_lines_africa_l<-all_lines_africa_l %>%
# #   group_by(year, name) %>%
# #   mutate(
# #          median=median(value, na.rm=TRUE), 
# #          q75=quantile(value,probs=seq(.75), na.rm=TRUE))  
# 
# # table(all_lines_africa_l$median, all_lines_africa_l$year)    
# 
# plot2=ggplot(all_lines_africa_l, aes(x=year, y=value, group=year)) +
#   geom_boxplot() +
#   facet_wrap(~name) +
#   #axis(1, at = seq(1990, 2017, by = 5)) +
#   #scale_x_discrete(breaks=seq(1990,2015,5))
#   xlim(1990,2017) +
#   # change the overall theme
#   theme_minimal() +
#   # edit the theme
#   theme(text = element_text(family = "Andale Mono"), legend.position = "bottom", # change all text font and move the legend to the bottom
#         panel.grid = element_line(color="white"), #panel.grid.minor.y = element_blank(), # change the grid color and remove minor y axis lines
#         plot.caption = element_text(hjust = 0.5, size = 8, color = "#56B4E9"), # remove x-axis text and edit the caption (centered and brown)
#         plot.title = element_text(size = 24), plot.subtitle = element_markdown(size=9, family = "Avenir-Black", color = "#56B4E9")) + #, # make the title bigger and edit the subtitle (font and brown)
#   #plot.tag.position = c(0.06, 0.115), plot.tag = element_text(hjust = 1, angle = 50, size = 11, lineheight=1.5)) + # position the tag under the x-axis of the first panel and rotate its position
#   # title
#   labs(title = "Call me mobile",
#        subtitle = "Africans are best reached on their mobile phone. Half of Africans have .8 phones. The top 25% have 1.6 phones.",
#        y = "Number of subscriptions per person", x=NULL,
#        caption = "data from OurWorldInData.org")
# plot2
# ggsave("callMeMobile.png")
# 
# years=c(1990,1995,2000,2005, 2010, 2016)
# 
# mobile_afreuro<-mobile %>%
#   filter(continent %in% c("Africa","Europe") & year %in% c(1990,1995,2000,2005, 2010, 2016)) %>%
#   #group_by(code) %>%
#   #arrange(code, year) %>%
#   #mutate(mobile_subs100=mobile_subs/total_pop*100) %>% necessary?
#   #mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
#   #      Diff_growth = mobile_subs - lag(mobile_subs), # Difference in route between years
#   #      Rate_percent_mb = (Diff_growth / Diff_year)/mobile_subs * 100) %>% # growth rate in percent
#   mutate(mobile_pp=mobile_subs/100) %>%
#   select(code, continent, year, mobile_pp)%>%
#   pivot_longer(cols=continent)
# 
# 
# landline_afreuro<-landline %>%
#   filter((continent=="Africa" | continent=="Europe") & year<2017) %>%
#   # group_by(code) %>%
#   # arrange(code, year) %>%
#   # mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
#   #        Diff_growth = landline_subs - lag(landline_subs), # Difference in route between years
#   #        Rate_percent_ll = (Diff_growth / Diff_year)/landline_subs * 100) %>% # growth rate in percent
#   mutate(landline_pp=landline_subs/100) %>%
#   select(code, continent, year, landline_pp) 
# 
# all_lines_afreuro<-full_join(mobile_afreuro, landline_afreuro, by=c("code", "year","continent"))
# 
# #long format
# all_lines_afreuro_l<-pivot_longer(all_lines_afreuro, cols=mobile_pp:landline_pp) %>%
#   mutate(name=factor(name, labels=c("Landline Subscribers", "Mobile Subscribers")))
# 
# 
# plot3=ggplot(all_lines_africa_l, aes(x=value, group=continent)) +
#   geom_density() +
#   facet_wrap(~name) 
# plot3
# +
#   #axis(1, at = seq(1990, 2017, by = 5)) +
#   #scale_x_discrete(breaks=seq(1990,2015,5))
#   xlim(1990,2017) +
#   # change the overall theme
#   theme_minimal() +
#   # edit the theme
#   theme(text = element_text(family = "Andale Mono"), legend.position = "bottom", # change all text font and move the legend to the bottom
#         panel.grid = element_line(color="white"), #panel.grid.minor.y = element_blank(), # change the grid color and remove minor y axis lines
#         plot.caption = element_text(hjust = 0.5, size = 8, color = "#56B4E9"), # remove x-axis text and edit the caption (centered and brown)
#         plot.title = element_text(size = 24), plot.subtitle = element_markdown(size=9, family = "Avenir-Black", color = "#56B4E9")) + #, # make the title bigger and edit the subtitle (font and brown)
#   #plot.tag.position = c(0.06, 0.115), plot.tag = element_text(hjust = 1, angle = 50, size = 11, lineheight=1.5)) + # position the tag under the x-axis of the first panel and rotate its position
#   # title
#   labs(title = "Call me mobile",
#        subtitle = "Africans are best reached on their mobile phone. Half of Africans have .8 phones. The top 25% have 1.6 phones.",
#        y = "Number of subscriptions per person", x=NULL,
#        caption = "data from OurWorldInData.org")
# plot3
# ggsave("callMeMobile2.png")
# 
# #todo: https://www.r-graph-gallery.com/294-basic-ridgeline-plot.html#color
# 
# 
# plot4<-ggplot(all_lines_afreuro_l, aes(x = value, y = `year`, fill = ..x..)) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   scale_fill_viridis(name = "Temp. [F]", option = "C") +
#   #labs(title = 'Temperatures in Lincoln NE in 2016') +
#   theme_ipsum() +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   )
# plot4