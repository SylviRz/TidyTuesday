install.packages("tidytuesdayR")
install.packages("viridis")

library("tidytuesdayR")
library("ggplot2")
library("dplyr")
library("rcartocolor")
library("viridis")
library("forcats")

## loading the data locally
wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')


# exploring

str(wind_turbine)

table(wind_turbine$province_territory)
table(wind_turbine$manufacturer)

plot1<-ggplot(wind_turbine, aes(x=province_territory, y=commissioning_date)) +
  geom_point() +
  theme_minimal()
plot1

#problems with the dates: gsub or stringremove

#time: 2005/2006/2012
table(wind_turbine$province_territory, wind_turbine$manufacturer)

plot2<-ggplot(wind_turbine,aes(y=province_territory)) +
  geom_histogram(stat="count") +
  theme_minimal()
plot2

plot3<-ggplot(wind_turbine,aes(y=manufacturer)) +
  geom_bar(stat="count") +
  theme_minimal()
plot3

#filtering: just the provinces with the most wind_turbines

wind2<-wind_turbine %>%
  filter(province_territory=="Alberta" | 
           province_territory=="Ontario" | 
           province_territory=="Quebec") %>%
  mutate(manufacturer_fct=as.factor(manufacturer)) %>%
  mutate(manufacturer_fct_o=fct_lump_min(manufacturer_fct, 19)) %>%
  count(province_territory, manufacturer_fct_o) 

plot4<-ggplot(wind2, aes(fill=manufacturer_fct_o, x=n, y=province_territory)) +
  geom_bar(stat="identity") +
  labs(x = NULL, y = "Province",
       color = NULL, 
       title = "Manufacturers by Province") +
  scale_fill_viridis(discrete = T, direction =-1) +
  #scale_fill_carto_d("Manufacturer",
  #                    palette = "Prism") +
  theme_minimal() +
  theme(legend.title = element_blank(),
    legend.position = "bottom")
plot4
ggsave("/Users/rzepka/Documents/Correlaid/TidyTuesday/20201028/manufacturers_province.pdf")
#ggsave("/Users/rzepka/Documents/Correlaid/TidyTuesday/20201028/manufacturers_province_carto.pdf")

# make an other category

  
#tipps:
#rnaturalearth -- for maps
#ggforce -- voronoi tiles
#