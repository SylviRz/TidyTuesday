library(tidyverse)
remotes::install_github("friep/vaccc19de")
library(vaccc19de)



cumulative_ts <- rki_download_cumulative_ts()
diffs_ts <- rki_download_diffs_ts()


#Exploring

summary(cumulative_ts$indikation_nach_alter)
table(cumulative_ts$indikation_nach_alter)
summary(cumulative_ts$pflegeheim_bewohner_in)
summary(cumulative_ts$ts_datenstand)


cumulative_ts_aktuell<- cumulative_ts %>%
  # nur aktuellen Tag behalten
  filter(ts_datenstand==max(ts_datenstand)) %>%
  arrange(desc(pflegeheim_bewohner_in))

pflegeheim_share<-ggplot(cumulative_ts_aktuell, aes(x=reorder(bundesland_iso, pflegeheim_bewohner_in), y=pflegeheim_bewohner_in)) +
      geom_bar(stat="identity") +
      labs(y="Pflegeheimbewohner/in", x="Bundesland") +
      theme_bw()
pflegeheim_share

Bundesland Bevölkerung?