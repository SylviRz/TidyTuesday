library(tidytuesdayR)
library(tidyverse)
library(janitor)



drivers <- tidytuesdayR::tt_load('2021-05-25')$drivers

summary(drivers)
table(drivers$player)
drivers%>% tabyl(position)

records <- tidytuesdayR::tt_load('2021-05-25')$records

summary(records$record_duration)

table(records$track, records$shortcut)

table(records$type)
table(records$system_played)

# On which track does the shortcut save the most time?
records_prep<-records %>%
  select(-c("player", "system_played", "time_period")) %>%
  filter(type=="Three Lap") %>%
  group_by(first_record=min(date))

#when were the shortcuts discovered?
shortcut_discovery<-records %>%
  select(type, track, shortcut, date) %>%
  filter(type=="Three Lap", 
         shortcut=="Yes") %>%
  group_by(track) %>%
  summarise(post_shortcut=min(date)) 

records_prep2<-merge(records_prep, shortcut_discovery, by="track") %>%
  #How long did it take to discover short cuts?
  mutate(period_to_discovery=post_shortcut-first_record)
  

plot1<-ggplot(records_prep2, aes(x=date, time, color=track)) +
      geom_line() +
      facet_wrap(~shortcut) +
      theme_minimal()
plot1

plot2 <- ggplot(records_prep2, aes(y=period_to_discovery, x=track)) +
        geom_bar(stat='identity') +
        coord_flip() +
        theme_minimal()
plot2

#keeping pre-discovery non-shortcut and post-discovery short-cut data per track
pre_discovery<-records_prep2 %>%
  filter(date<post_shortcut,
         shortcut=="No") %>%
  mutate(time_to_discovery=date-post_shortcut)

post_discovery<-records_prep2 %>%
  filter(date>=post_shortcut,
         shortcut=="Yes") %>%
  mutate(time_to_discovery=date-post_shortcut)

pre_post<-rbind(pre_discovery, post_discovery)

plot3<-ggplot(pre_post, aes(y=time, x=as.numeric(time_to_discovery), color=track)) +
       geom_line() +     
       coord_cartesian(xlim=c(-50, 50)) +
       geom_vline(xintercept = 0) +
       theme_bw() +
       labs(y="Record Time",
            x="Days to Shortcut Discovery")
plot3



#to dos
  # turn those with no short cuts gray
  # make a variable current 10 record times before the short cut and 10 after
  # Or: last pre short cut time versus first post-shortcut time
  # Is there a convergence of shortcut - non-shortcut times?
  # if I want to show the difference in pre and post I could also directly show that 
  # check: how long does a record time last?