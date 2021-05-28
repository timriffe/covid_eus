library(tidyverse)
library(ggridges)
library(lubridate)
covid <- readRDS("Data/iscii_ccaa.rds")


covid_st <-
  covid %>% 
  group_by(CCAA_iso,year_iso, week_iso, fecha, variable) %>% 
  summarize(tasa_st_pv = sum(tasa * stand_pv), 
            tasa_st_nac = sum(tasa * stand_nac),
            .groups = "drop") 

# cumulative incidence by age groups and sex
covid %>% 
  dplyr::filter(variable == "casos") %>% 
  ggplot(aes(x = fecha, y = tasa_cumul, color = CCAA_iso)) + 
  geom_line() +
  facet_grid(sexo~edad) 

both_sex_subset <- 
  covid %>% 
  filter(edad %in% c(0,20,40,60,80)) %>% 
  group_by(CCAA_iso, variable, fecha, edad) %>% 
  summarize(value = sum(value),
            pob = sum(pob),
            .groups = "drop") %>% 
  mutate(tasa = value / pob) %>% 
  arrange(CCAA_iso,variable,edad, fecha) %>% 
  group_by(CCAA_iso,variable,edad) %>% 
  mutate(tasa_cumul = cumsum(tasa)) %>% 
  ungroup()

both_sex_subset %>% 
  dplyr::filter(variable == "casos") %>% 
  ggplot(aes(x = fecha, y = tasa_cumul * 100, color = CCAA_iso)) + 
  geom_line() +
  facet_grid(~edad, scale = "free_y") 



PV <- covid_st%>% dplyr::filter(CCAA_iso == "PV")

covid_st %>% 
  ggplot(aes(x = fecha, y = tasa_st_nac * 1e5, group = CCAA_iso)) + 
  geom_line(alpha = .5) +
  geom_line(data = PV,
            mapping = aes(x = fecha, y = tasa_st_nac * 1e5),
            color = "red",
            size = 2) +
  facet_wrap(~variable, scale = "free_y") 

PV %>% 
  dplyr::filter(variable == "def") %>% 
  ggplot(aes(x = fecha, y = tasa_st_pv)) +
  geom_line()



covid_st %>% 
  dplyr::filter(variable == "casos")  %>% 
  ggplot(aes(x = fecha, y = CCAA_iso, height = tasa_st_nac * 5e2)) + 
  geom_ridgeline() +
  geom_vline(xintercept = as_date(c("2020-01-05",
                                    "2020-06-20",
                                    "2020-11-30",
                                    "2021-03-01",
                                    "2021-05-18"))) +
  annotate("text",x = as_date(c("2020-03-01", "2020-09-01","2020-12-30","2021-03-15")),
           y = rep(19,4), 
           label = c("W1","W2","W3","W4"),
           size = 8)

# rank dataset
covid_ranks <-
  covid_st %>% 
  mutate(WaveBreaks = cut(fecha, breaks = as_date(c("2020-01-01",
                                           "2020-06-20",
                                           "2020-11-30",
                                           "2021-03-01",
                                           "2021-05-21")),
                          labels = c("W1","W2","W3","W4")) ) %>% 
  group_by(variable, CCAA_iso, WaveBreaks) %>% 
  summarize(tasa_st_nac = sum(tasa_st_nac),
            tasa_st_pv = sum(tasa_st_pv), 
            .groups = "drop") %>%
  group_by(variable, WaveBreaks) %>% 
  mutate(Rank = rank(tasa_st_nac)) %>% 
  ungroup() 
cr_pv <- covid_ranks %>% dplyr::filter(CCAA_iso == "PV")


this_variable <- "hosp"

CCAA_rank_plots <- list()
es_vars         <- c("casos","hosp","uci","def")
eng_vars        <- c("case","hospitalization","UCI","death")
names(eng_vars) <- es_vars
for (this_variable in c("casos","hosp","uci","def")){
  
  
  CCAA_rank_plots[[this_variable]] <-
    covid_ranks %>% 
    dplyr::filter(variable == this_variable) %>% 
    ggplot(aes(x = WaveBreaks, y = Rank, color = CCAA_iso)) + 
    geom_point(size = 4) +
    geom_line(aes(x = WaveBreaks, y = Rank, color = CCAA_iso, group = CCAA_iso)) +
    geom_line(data = dplyr::filter(covid_ranks, 
                                   variable == this_variable,
                                   CCAA_iso == "PV"),
              mapping = aes(x = WaveBreaks, y = Rank, color = CCAA_iso, group = CCAA_iso),
              size = 2) +
    geom_text(data = dplyr::filter(covid_ranks, 
                                   WaveBreaks == "W4",
                                   variable == this_variable),
              mapping = aes(x = 4.2, y = Rank, label = CCAA_iso),
              size = 5) +
    geom_text(data = dplyr::filter(covid_ranks, 
                                   WaveBreaks == "W1",
                                   variable == this_variable),
              mapping = aes(x = .8, y = Rank, label = CCAA_iso),
              size = 5) + 
    annotate("text",x = rep(.6,17), y = 1:17, label = 17:1) +
    annotate("text", x = .6, y= 17.9, label = "Rank", size = 6) +
    annotate("text", x = 4.2, y= 17.9, label = "CCAA", size = 6) +
    labs(title = paste("CCAA rank of age-sex-standardized",eng_vars[this_variable],"rates by wave"),
         subtitle = "based on following date breaks: 2020-06-20, 2020-11-30, 2021-03-01",
         x = "Wave") +
    theme(legend.position = "none",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}

CCAA_rank_plots[[4]]

  #facet_wrap(~variable) +
  # geom_point(data= dplyr::filter(covid_ranks, CCAA_iso == "PV", variable == "casos"), 
  #            mapping = aes(x = WaveBreaks, y = Rank),
  #            size = 4, color = "red")
  

# Again, but relative to max, where max is 17.
covid_relranks <-
  covid_ranks %>% 
  group_by(WaveBreaks, variable) %>% 
  mutate(RelRank = tasa_st_nac / max(tasa_st_nac) * 17) %>% 
  ungroup() 

CCAA_relrank_plots <- list()
for (this_variable in c("casos","hosp","uci","def")){
  
  CCAA_relrank_plots[[this_variable]] <-
  covid_relranks %>% 
  dplyr::filter(variable == this_variable) %>% 
  ggplot(aes(x = WaveBreaks, y = RelRank, color = CCAA_iso)) + 
  geom_point(size = 4) +
  geom_line(aes(x = WaveBreaks, y = RelRank, color = CCAA_iso, group = CCAA_iso)) +
  geom_line(data = dplyr::filter(covid_relranks, 
                                 variable == this_variable,
                                 CCAA_iso == "PV"),
            mapping = aes(x = WaveBreaks, y = RelRank, color = CCAA_iso, group = CCAA_iso),
            size = 2) +
  geom_text(data = dplyr::filter(covid_relranks, 
                                 WaveBreaks == "W4",
                                 variable == this_variable),
            mapping = aes(x = 4.2, y = RelRank, label = CCAA_iso),
            size = 5) +
  geom_text(data = dplyr::filter(covid_relranks, 
                                 WaveBreaks == "W1",
                                 variable == this_variable),
            mapping = aes(x = .8, y = RelRank, label = CCAA_iso),
            size = 5) + 
  #annotate("text",x = rep(.6,17), y = 1:17, label = 17:1) +
  #annotate("text", x = .6, y= 17.9, label = "Rel. Rank", size = 6) +
  annotate("text", x = 4.2, y= 17.9, label = "CCAA", size = 6) +
  labs(title = paste("CCAA age-sex-wave-standardized",
                     eng_vars[this_variable],"rates by wave"),
       subtitle = "based on following date breaks: 2020-06-20, 2020-11-30, 2021-03-01",
       x = "Wave") +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
}
CCAA_relrank_plots[[1]]


library(cowplot)
plot_grid(CCAA_relrank_plots[[1]],CCAA_relrank_plots[[2]],CCAA_relrank_plots[[3]],CCAA_relrank_plots[[4]])


covid_relranks2 <-
  covid_ranks %>% 
  group_by(CCAA_iso, variable) %>% 
  summarize(tasa_st_nac = sum(tasa_st_nac),.groups = "keep") %>% 
  mutate(RelRank = tasa_st_nac / max(tasa_st_nac) * 17) %>% 
  ungroup() 
  
  covid_relranks2 %>% 
  ggplot(aes(x = variable, y = RelRank, color = CCAA_iso)) + 
    geom_point(size = 4) +
    geom_line(aes(x = variable, y = RelRank, color = CCAA_iso, group = CCAA_iso)) +
    geom_text(data = dplyr::filter(covid_relranks, 
                             
                                   variable == "casos"),
              mapping = aes(x = 4.2, y = RelRank, label = CCAA_iso),
              size = 5) +
    geom_text(data = dplyr::filter(covid_relranks, 
                                   WaveBreaks == "W1",
                                   variable == this_variable),
              mapping = aes(x = .8, y = RelRank, label = CCAA_iso),
              size = 5) + 
    #annotate("text",x = rep(.6,17), y = 1:17, label = 17:1) +
    #annotate("text", x = .6, y= 17.9, label = "Rel. Rank", size = 6) +
    annotate("text", x = 4.2, y= 17.9, label = "CCAA", size = 6) +
    labs(title = paste("CCAA age-sex-wave-standardized",
                       eng_vars[this_variable],"rates by wave"),
         subtitle = "based on following date breaks: 2020-06-20, 2020-11-30, 2021-03-01",
         x = "Wave") +
    theme(legend.position = "none",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  