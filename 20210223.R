library(tidyverse)
library(tidytuesdayR)

rm(list=ls(all=TRUE))


# Read in as a dataframe
earn <- as.data.frame(tt_load('2021-02-23')$"earn")
employed <- as.data.frame(tt_load('2021-02-23')$"employed")


# Exploratory data analyis
###employed
table(employed$industry)
# why Asian? 

table(employed$major_occupation)
# why so many 1488 counts?

table(employed$year)
table(employed$race_gender)

###earn
table(earn$year) # from 2010 to 2020

# Graph ideas 
# - share of men or women / race in occupations from 2015 to 2020


# - Dubois style graph... 

data_prep<-employed %>%
  filter(year==2020) %>%
  filter(race_gender=="White" | race_gender== "Black or African American") %>%
  #mutate(major_occupation_fct=as.factor(major_occupation), 
  #       race_gender_fct=as.factor(race_gender)) %>%
  group_by(race_gender, major_occupation) %>%
  # sum all minor occupations into the major occupations
  summarise(employ_n_sum=sum(employ_n, na.rm=TRUE))  %>% # learning!
  group_by(race_gender) %>%
  mutate(occ_share=paste0(round(employ_n_sum/sum(employ_n_sum)*100, 2))) %>%
  select(occ_share, major_occupation, race_gender) %>%
  ungroup() %>%
  #add 2 lines for the white space in between whites and blacks
  add_row(race_gender = "White", major_occupation = "empty", occ_share="70") %>%
  add_row(race_gender = "Black or African American", major_occupation = "empty", occ_share="70") %>%
  arrange(race_gender)
  
  
# Code Tweaked from: https://rpubs.com/ejhollowood/du-bois

# Create factor to order occupationstab
data_prep$race_occ<-factor(paste(data_prep$race_gender, data_prep$major_occupation, sep = " "))
                           ,
                    levels = c("B management", "B resources", "B production", 
                     "B sales",  "B service", "B empty",
                     "W management","W resources", "W production", 
                     "W sales", "W service", "W empty", 
                     ordered = TRUE)
          )

# Create vector for colours
occupation_fill <- c("resources" = "#dc143c", ##EE3B3B",
                     "service" ="#ffd700", # "#FFD700",
                     " " = "white",
                     "production" = "#4682b4", #"#1C86EE",
                     "management" = "#654321", #"#CDAA7D",
                     "sales" = "#d2b48c") #"#EEEEE0")
                       
# colors from https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf
  # Gold #ffd700
  # Crimson 
  # Blue #4682b4
  # Tan #d2b48c
  # Brown #654321

# Create labels and breaks (items to appear in Key) for occupations
# occupation_labels <- c("agriculture" = "AGRICULTURE, FISHERIES AND MINING.",
#                        "manufacturing" = "MANUFACTURING AND MECHANICAL INDUSTRIES.",
#                        "domestic_service" = "DOMESTIC AND PERSONAL SERVICE.",
#                        "professions" = "PROFESSIONS.",
#                        "trade" = "TRADE AND TRANSPORTATION.")

#occupation_breaks <- c("agriculture", "manufacturing", "domestic_service", "professions", "trade")
occupation_breaks_new <- c("resources", "production", "service", "management", "sales")


duboispie<-ggplot(data_prep) +
      geom_bar(aes(x = "", y = occ_share, fill = major_occupation, group = race_occ), stat = "identity") +
      geom_text(aes(x = "", y = 285, vjust = -18, label = "BLACK."),  size = 5) +
      geom_text(aes(x = "", y = 120, vjust = 19, label = "WHITE."),  size = 5) +
      coord_polar("y", start = 5.3, direction = 1) +
      #scale_y_reverse() +
      scale_fill_manual(values = occupation_fill, 
                        #labels = occupation_labels,
                        breaks = occupation_breaks_new,
                        guide = guide_legend(title = NULL, ncol = 2)) +
      theme_minimal() +
      theme(text = element_text( size = 16, colour = "black"), #family = "Rajdhani",
            plot.title = element_text(size = 24, hjust = 0.5, face = "bold", margin = margin(0,0,10,0)),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom") +
            labs(title = "OCCUPATIONS OF BLACK AND WHITE IN THE US.",
            caption = "Data www.bls.gov | DataViz @SylviRz")


#to dos
- factor variables - give them good levels
- Get a nice font - Rajdhani?