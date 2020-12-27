install.packages("tmap")
library(ggplot2)
library(tidytuesdayR)
library(tidyverse)
library(tmap)
library(sf) # for worldmap data
library(dplyr)
library(rcartocolor)

rm(list=ls(all=TRUE))
# Read in as a dataframe
bigmac <- as.data.frame(tt_load('2020-12-22')$"big-mac")

class(bigmac)
str(bigmac)
summary(bigmac$date)
table(bigmac$name)


#loading world map data
data("World") 

#####
#data prep

#Extracting the Eurozone
eurozone<- bigmac %>%
  filter(date=="2020-07-01") %>%
  filter(iso_a3=="EUZ") %>%
  dplyr::select(!iso_a3) %>%
  #duplicating 19x one line for each euro zone country
  slice(rep(1:n(), each=19)) 

#### iso_a3 for all eurozone countries  
eurozone$iso_a3<-c("AUT", "BEL", "CYP", "EST", "FIN", "FRA", "DEU", "GRC",
                   "IRL", "ITA", "LUX", "LVA", "LTU", "MLT", "NLD",
                   "PRT", "SVK", "SVN", "ESP")

# binding eurozone to bigmac data
bigmac2<-rbind(bigmac, eurozone)

bigmac_prep <- bigmac2 %>%
  #filtering for last date
  filter(date=="2020-07-01")%>%
  #dropping aggregated Euro zone line
  filter(iso_a3!="EUZ") %>%
  # keeping only a few vars, "..._adjusted" is the GPD adjusted over/undervaluation with respect to the currency
  dplyr::select(iso_a3, usd_raw, gdp_dollar, usd_adjusted, eur_adjusted, cny_adjusted, jpy_adjusted) 

#Joining the population to the shapefile data from world
bigmac_shapedata <- merge(World, bigmac_prep, by.x="iso_a3", by.y="iso_a3", all.x=TRUE)
head(bigmac_shapedata)

# Plotting

my_colors = carto_pal(7, "Fall")


map1<-tm_shape(bigmac_shapedata) +
  tm_fill(title="Undervalued (<0) \nOvervalued (>0) \nlocal currency", 
          c("usd_adjusted", "eur_adjusted", "cny_adjusted", "jpy_adjusted"),
          midpoint=0,
          breaks=c(-.5, -.3, -.15, 0, 0.001, .15, .3, .5, .9),
          palette=my_colors,
          colorNA="grey95",
          textNA = "No GDP data or McDonald's") + #c("usd_raw", "eur_raw", "cny_raw", "jpy_raw")
  tm_facets(sync = TRUE, ncol = 2, nrow=2,
            ) +
  tm_layout(main.title="Adjusted Big Mac Index for different base currencies",
            #main.subtitle="Shows how under- and overevaluation depends on the perspective",
            title="DataViz by @SylviRz for #TidyTuesday, \nData from The Economist",
            title.size=.5,
            title.position=c("left", "BOTTOM"),
            fontfamily="Andale Mono",
            legend.outside = TRUE,
            legend.text.size=0.5,
            panel.labels=c("USD base currency", "EUR base currency", "CNY base currency", "JPY base currency"),
            panel.label.bg.color = my_colors[4],
            frame=FALSE)
map1
tmap_save(
  tm = map1,
  filename = "bigmacindex_by_currency.jpg", width=8.5, height=5)

# Learnings
  # how to duplicate rows: slice(rep(1:n(), each=19))  -- copies the row 19 times
  # tm_facet - doesn't require a long data set
  # tmap's data(world) has the shapefiles as a world map
  # For the message a graph with bars lower or higher than zero for each country would have been a bit more informative
