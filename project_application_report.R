library(tidyverse)

---
  params:
  applicant_id: 1
anon_path: "https://raw.githubusercontent.com/CorrelAid/projectsdb/main/0000-00-EXA/team_selection/data/applications_anonymized.csv"
output: pdf_document

---
  
  <!-- THIS FILE IS NOT SUPPOSED TO BE KNITTED BY HAND
INSTEAD IT IS KNIT AUTOMATICALLY 
BY THE APPLICATIONS_REPORT.RMD FILE FOR EACH APPLICANT.
YOU CAN EDIT THIS FILE IF YOU WANT TO CHANGE THE INFORMATION DISPLAYED FOR EACH APPLICANT
-->
  ```{r include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(message = FALSE)

library(tidyverse)
lvls <- tibble(x = c(0, 1, 2, 3), level = c("Beginner", "User", "Advanced", "Expert"))
theme_set(theme_bw())
```

```{r message=FALSE}
appl <- readr::read_csv(params$anon_path, col_types = cols())
a <- appl %>% 
  filter(applicant_id == params$applicant_id)
```

### ggridges Plot by Frie

library(ggridges)

theme_set(theme_bw())

# tmp_ggridge <- tmp_all %>% 
#   semi_join(tmp_a, by = c("value", "level")) %>% 
#   mutate(this_applicant = TRUE) %>% 
#   bind_rows(tmp_all %>% 
#               anti_join(tmp_a, by = c("value", "level")) %>% 
#               mutate(this_applicant = FALSE))
# 
# ggplot(tmp_ggridge, aes(x = x, y = value)) + 
#   geom_density_ridges(stat = "binline", bins =4, scale = 0.9, draw_baseline = FALSE,  linetype = 0)+  
#   #vline(data = tmp_ggridge %>% filter(this_applicant), color = "orange") +
#   geom_point(data = tmp_ggridge %>% filter(this_applicant), color = "orange", shape="l", size=4)+
#   scale_x_continuous(breaks = seq(0, 3, 1), labels = str_sub(lvls$level, 1, 1))+
#   labs(x = "experience level", caption = "B: Beginner, U: User, A: Advanced, E: Expert")+
#   facet_wrap(~type, scales = "free_y", nrow = 3) +
#   theme(strip.background =element_blank()) #facet_wrap(~type, ncol = 1, strip.position = "left") 
# 


###plot Cool Plot by Long tweaked by Sylvi

plot1<-tmp_all %>% 
  semi_join(tmp_a, by = c("value", "level")) %>% 
  mutate(this_applicant = TRUE) %>% 
  bind_rows(tmp_all %>% 
              anti_join(tmp_a, by = c("value", "level")) %>% 
              mutate(this_applicant = FALSE)) %>% 
  mutate(value = fct_inorder(value)) %>%
  ggplot(aes(x = fct_reorder(level, x),
             alpha = this_applicant,
             fill = type)) +
  geom_bar(width = 1, show.legend = FALSE) +
  scale_x_discrete(labels = c("Beginner", "", "", "Expert")) +
  scale_alpha_discrete(range = c(.25, 1), guide = "none") +
  facet_wrap(~value, ncol = 4) +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line("grey92"),
        axis.text.x=element_text(size=rel(1.25)),
        strip.background =element_blank()) +
  labs(x = NULL, y = NULL)
plot1

ggsave("application_plot1.jpg")