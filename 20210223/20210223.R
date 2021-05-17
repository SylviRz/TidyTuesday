library(tidyverse)
library(tidytuesdayR)

rm(list=ls(all=TRUE))


# Note

  # didn't finish this one

# Read in as a dataframe
earn <- as.data.frame(tt_load('2021-02-23')$"earn")
employed <- as.data.frame(tt_load('2021-02-23')$"employed")


# Exploratory data analyis
###employed
table(employed$industry)
# why does Asian appear in this column? 

table(employed$major_occupation)
table(employed$year)
table(employed$race_gender)

###earn
table(earn$year) # from 2010 to 2020

# Graph ideas 
# - share of men or women / race in occupations from 2015 to 2020
# - Dubois style graph... -- going for this

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
  #add 2 lines for the white space in between whites and blacks in the pie chart
  add_row(race_gender = "White", major_occupation = "", occ_share="70") %>%
  add_row(race_gender = "Black or African American", major_occupation = "", occ_share="70") %>%
  arrange(race_gender)
  
  
# Code Tweaked from: https://rpubs.com/ejhollowood/du-bois 

# Create factor to order occupationstab
data_prep$race_occ<-factor(paste(data_prep$race_gender, data_prep$major_occupation, sep = " "))
levels(data_prep$race_occ) 
#data_prep$race_occ2<-reorder(data_prep$race_occ, new.order=c(3, 6, 4, 2, 5, 1, 9,12,10,8,11,7))
data_prep$race_occ2<-factor(data_prep$race_occ,
                            levels(data_prep$race_occ)[c(9,12,10,8,11,7,3, 6, 4, 2, 5, 1)],
                            ordered = TRUE) #learning!
levels(data_prep$race_occ2)                    

levels = c("B resources", "B service", "B production", "B management",
                     "B sales",   "B empty",
                     "W resources","W service", "W production", "W management",
                     "W sales",  "W empty", 
                     )

# Create vector for colours
occupation_fill <- c("#dc143c", ##EE3B3B",
                     "#ffd700", # "#FFD700",
                     "#4682b4", #"#1C86EE",
                     "#d2b48c",
                     "#654321", #"#CDAA7D",
                     "white") #"#EEEEE0")

# Crimson "#dc143c"
# Gold #ffd700
# Blue #4682b4
# Tan #d2b48c
# Brown #654321
  # c("resources" = "#dc143c", ##EE3B3B",
  #                    "service" ="#ffd700", # "#FFD700",
  #                    " " = "white",
  #                    "production" = "#4682b4", #"#1C86EE",
  #                    "management" = "#654321", #"#CDAA7D",
  #                    "sales" = "#d2b48c") #"#EEEEE0")
                       
# colors from https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf
  # Gold #ffd700
  # Crimson "#dc143c"
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

extra font: Nirmala UI SEmilight


#Load a font from Google Fonts
sysfonts::font_add_google("Nanum Pen Script", "Nanum Pen Script", regular.wt = 400)
#sysfonts::font_add_google("Ubuntu Mono", "Ubuntu Mono")
showtext::showtext_auto()

quartz()

duboispie<-ggplot(data_prep) +
      geom_bar(aes(x = "", y = occ_share, fill = major_occupation, group = race_occ2), stat = "identity") +
      #geom_text(aes(x = "", y = 285, vjust = -18, label = "BLACK."),  size = 5) +
      #geom_text(aes(x = "", y = 120, vjust = 19, label = "WHITE."),  size = 5) +
      coord_polar("y", start = 5.3, direction = 1) +
      scale_fill_manual(values = occupation_fill, 
                        #labels = occupation_labels,
                        #breaks = occupation_breaks_new,
                        guide = guide_legend(title = NULL, ncol = 2)) +
      theme_minimal() +
      theme(text = element_text(size = 12, colour = "black", family="Nanum Pen Script"), #family = "Rajdhani",
            plot.title = element_text(size = 24, hjust = 0.5, face = "bold", margin = margin(0,0,10,0)),
            panel.background =element_blank(), #element_rect(fill = "cornsilk"), #= "cornsilk", #,
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank() +
            legend.position = "bottom" ) +
      labs(title = "OCCUPATIONS OF BLACK AND WHITE IN THE US.",
            caption = "Data www.bls.gov | DataViz @SylviRz")
duboispie

ggsave("/Users/rzepka/Documents/Correlaid/TidyTuesday/TidyTuesday/20210223.pdf")

, height = 7, width = 9, dpi = 600, scale = 0.5)



#to dos
#- factor variables - give them good levels
#- Get a nice font - Rajdhani?
#- Color the background in a beige tone... 
