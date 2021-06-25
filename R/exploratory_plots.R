library(tidyverse)
library(ggridges)
library(lubridate)
library(ggrepel)
library(GGally)
library(ggforce)
library(colorspace)
library(cowplot)
library(patchwork)
library(ggridges)


covid <- readRDS("Data/data_ccaa.rds") %>% 
  arrange(CCAA_iso, variable, sexo, edad, year_iso, week_iso) %>% 
  group_by(CCAA_iso, variable, sexo, edad) %>% 
  mutate(tasa_cumul = cumsum(tasa)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(value))

# pseudo Lexis surfaces
covid %>% 
  dplyr::filter(CCAA_iso == "MD", variable == "casos") %>% 
  ggplot(aes(x =fecha, y = edad + 5, fill = tasa))+
  geom_tile()+
  scale_fill_continuous_sequential("PinkYl") + 
  theme_minimal() + 
  facet_grid(rows = "sexo")

# divergent
# covid %>% 
#   dplyr::filter(CCAA_iso == "PV", variable == "exceso") %>% 
#   select(fecha, sexo, tasa, edad) %>% 
#   pivot_wider(names_from = "sexo", values_from = "tasa") %>% 
#   mutate(sr = H / M,
#          sr = ifelse(sr > 2, 2, sr)) %>% 
#   ggplot(aes(x =fecha, y = edad + 5, fill = sr)) +
#   geom_tile()+
#   scale_fill_continuous_diverging() + 
#   theme_minimal()


covid_st <-
  covid %>% 
  group_by(CCAA_iso,year_iso, week_iso, fecha, variable) %>% 
  summarize(tasa_st_pv = sum(tasa * stand_pv), 
            tasa_st_nac = sum(tasa * stand_nac),
            .groups = "drop")

covid_st_by_sex <-
  covid %>% 
  group_by(CCAA_iso,year_iso, week_iso, fecha, variable, sexo) %>% 
  summarize(tasa_st_pv = sum(tasa * stand_pv) / sum(stand_pv), 
            tasa_st_nac = sum(tasa * stand_nac) / sum(stand_nac),
            .groups = "drop")


# Sex ratios by variable excluding excess
covid_st_by_sex %>% 
  select(CCAA_iso, fecha, sexo, variable, tasa_st_nac) %>% 
  pivot_wider(names_from = sexo, values_from = tasa_st_nac) %>% 
  dplyr::filter(variable != "exceso") %>% 
  mutate(sr = H / M,
         sr = ifelse(is.infinite(sr) | is.nan(sr),NA_real_,sr)) %>% 
  ggplot(aes(x = fecha, y = sr, group = CCAA_iso, color = CCAA_iso)) +
  geom_line()+
  facet_wrap(~variable, scale = "free_y") + 
  scale_y_log10()

# Sex ratios (cumulative) by measure. 
# what's going on here?
covid_st_by_sex %>% 
  mutate(variable = factor(variable, levels = c("casos","hosp","uci","def","exceso"))) %>% 
  group_by(CCAA_iso, variable, sexo) %>% 
  summarize(tasa_st_nac = sum(tasa_st_nac), .groups = "drop") %>% 
  pivot_wider(names_from = sexo, values_from = tasa_st_nac) %>% 
  dplyr::filter(variable != "exceso") %>% 
  mutate(sr = H / M,
         sr = ifelse(is.infinite(sr) | is.nan(sr),NA_real_,sr)) %>% 
  ggplot(aes(x = variable, y = sr, group = CCAA_iso, color = CCAA_iso)) +
  geom_line()+
  scale_y_log10()


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
PV %>% 
  dplyr::filter(variable == "exceso") %>% 
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
                                    "2021-05-31"))) +
  annotate("text",x = as_date(c("2020-03-01", "2020-09-01","2021-01-10","2021-04-15")),
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
                                           "2021-06-09")),
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

CCAA_rank_plots_waves <- list()
es_vars         <- c("casos","hosp","uci","def", "exceso")
eng_vars        <- c("case","hospitalization","UCI","death","excess death")
names(eng_vars) <- es_vars
for (this_variable in es_vars){
  
  
  CCAA_rank_plots_waves[[this_variable]] <-
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

save(CCAA_rank_plots_waves, file = "FigData/CCAA_rank_plots_waves.Rdata")

CCAA_rank_plots_waves
  #facet_wrap(~variable) +
  # geom_point(data= dplyr::filter(covid_ranks, CCAA_iso == "PV", variable == "casos"), 
  #            mapping = aes(x = WaveBreaks, y = Rank),
  #            size = 4, color = "red")
  

# Again, but relative to max, where max is 17.
covid_relranks <-
  covid_ranks %>% 
  group_by(WaveBreaks, variable) %>% 
  mutate(RelRank = tasa_st_nac / max(tasa_st_nac) * 17) %>% 
  ungroup() %>% 
  mutate(RelRank = ifelse(sign(tasa_st_nac) == -1, 0, RelRank))

CCAA_relrank_waves <- list()
for (this_variable in es_vars){
  
  CCAA_relrank_waves[[this_variable]] <-
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
CCAA_relrank_waves[[5]]
save(CCAA_relrank_waves, file = "FigData/CCAA_relrank_waves.Rdata")


plot_grid(CCAA_relrank_plots[[1]],CCAA_relrank_plots[[2]],CCAA_relrank_plots[[3]],CCAA_relrank_plots[[4]], CCAA_relrank_plots[[5]])


stands <- covid %>% 
  dplyr::filter(CCAA_iso == "PV", 
                year_iso == 2020, 
                week_iso == 20, 
                variable == "casos") %>% 
  select(edad,sexo, stand_nac, stand_pv)

covid_ranks2 <-
  covid %>% 
  group_by(CCAA_iso, variable,sexo,edad) %>%
  summarize(tasa = sum(tasa, na.rm = TRUE), .groups = "drop") %>% 
  left_join(stands, by = c("sexo","edad")) %>% 
  group_by(CCAA_iso, variable) %>% 
  summarize(tasa_st_pv = sum(tasa * stand_pv, na.rm=TRUE), 
            tasa_st_nac = sum(tasa * stand_nac, na.rm=TRUE),
            .groups = "drop") %>% 
  group_by(variable) %>% 
  mutate(Rank = rank(tasa_st_nac),
         RelRank = tasa_st_nac / max(tasa_st_nac, na.rm=TRUE) * 17) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable, levels = c("casos","hosp","uci","def","exceso")))

PV2 <- covid_ranks2 %>% 
  dplyr::filter(CCAA_iso == "PV")
  
cumulative_ranks <-
covid_ranks2 %>% 
  ggplot(aes(x = variable, y = Rank, color = CCAA_iso, group = CCAA_iso)) + 
    geom_point(size = 4) +
    geom_line() +
  geom_point(data = PV2, 
             mapping = aes(x = variable, y = Rank, color = CCAA_iso, group = CCAA_iso),
             size = 6) +
  geom_line(data = PV2, 
             mapping = aes(x = variable, y = Rank, color = CCAA_iso, group = CCAA_iso),
             size = 2) +
  geom_line() +
     geom_text_repel(data = dplyr::filter(covid_ranks2, 
                              
                                    variable == "exceso"),
               mapping = aes(x = 5.2, y = Rank, label = CCAA_iso),
               size = 5) +
     geom_text_repel(data = dplyr::filter(covid_ranks2, 
                                    variable == "casos"),
               mapping = aes(x = .8, y = Rank, label = CCAA_iso),
               size = 5) + 
    #annotate("text",x = rep(.6,17), y = 1:17, label = 17:1) +
    #annotate("text", x = .6, y= 17.9, label = "Rel. Rank", size = 6) +
    annotate("text", x = 5.2, y= 17.9, label = "CCAA", size = 6) +
    labs(title = paste("CCAA age-sex-standardized cumulative rates"),
         subtitle = "Ranks",
         x = "Measure") +
    theme(legend.position = "none",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
save(cumulative_ranks, file = "FigData/cumulative_ranks.Rdata")  


cumulative_relranks <-
  covid_ranks2 %>% 
  ggplot(aes(x = variable, y = RelRank, color = CCAA_iso, group = CCAA_iso)) + 
  geom_point(size = 4) +
  geom_line() +
  geom_point(data = PV2, 
             mapping = aes(x = variable, y = RelRank, color = CCAA_iso, group = CCAA_iso),
             size = 6) +
  geom_line(data = PV2, 
            mapping = aes(x = variable, y = RelRank, color = CCAA_iso, group = CCAA_iso),
            size = 2) +
  geom_line() +
  geom_text_repel(data = dplyr::filter(covid_ranks2, 
                                       
                                       variable == "exceso"),
                  mapping = aes(x = 5.2, y = RelRank, label = CCAA_iso),
                  size = 5) +
  geom_text_repel(data = dplyr::filter(covid_ranks2, 
                                       variable == "casos"),
                  mapping = aes(x = .8, y = RelRank, label = CCAA_iso),
                  size = 5) + 
  #annotate("text",x = rep(.6,17), y = 1:17, label = 17:1) +
  #annotate("text", x = .6, y= 17.9, label = "Rel. Rank", size = 6) +
  annotate("text", x = 5.2, y= 17.9, label = "CCAA", size = 6) +
  labs(title = paste("CCAA age-sex-standardized cumulative rates"),
       subtitle = "Relative Ranks",
       x = "Measure") +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
save(cumulative_relranks, file = "FigData/cumulative_relranks.Rdata")  
# TODO
# 1) ridgeplots for each variable
# 2) rank plots by wave.



variables <- c("casos","hosp","uci","def","exceso")
variable_names <- c("casos","hospitalizaciones","uci","defunciones COVID","exceso mortalidad")
names(variable_names) <- variables

heights <- c(casos = 300, hosp =3000 , uci = 50000, def = 10000, exceso = 10000  )
#heights <- c(casos= 1, hosp = 1, uci = 1, def = 1, exceso = 1)
ridge_plots <- list()

covid_st_ordered <-
covid_st %>% 
  mutate(CCAA_iso = reorder(CCAA_iso, tasa_st_nac, na.rm=TRUE, FUN = sum))
CCAA_isov <- covid_st_ordered %>% pull(CCAA_iso) %>% unique() %>% sort()
CCAA_cols <- rep(gray(.5),length(CCAA_isov))
CCAA_cols[CCAA_isov %in% c("PV","NC")] <- "#d4204d"
for (i in variables){
  ridge_plots[[i]] <-
    covid_st_ordered %>% 
  dplyr::filter(variable == i)  %>% 
  ggplot(aes(x = fecha, 
             #y = reorder(CCAA_iso, tasa_st_nac, na.rm=TRUE, FUN = var), 
             y = CCAA_iso,
             height = tasa_st_nac,
             fill = CCAA_iso)) + 
  geom_ridgeline(color  = gray(.2), alpha = .7,min_height = -2,scale = heights[i]) +
  geom_vline(xintercept = as_date(c("2020-01-05",
                                    "2020-06-20",
                                    "2020-11-30",
                                    "2021-03-01",
                                    as.character(today()))),
             color = "#AAAAAA",
             linetype = "82") +
  theme(legend.pos = "none", 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size = 14)) + 
  labs(x = "fecha", y = "CCAA", title = paste0("Tasas estandarizadas de ",variable_names[i])) + 
  scale_fill_manual(values = CCAA_cols, labels = CCAA_isov) + 
    coord_cartesian(clip = "off")

}

# covid_st_ordered %>% 
#   dplyr::filter(variable == "casos")  %>% 
#   ggplot(aes(x = fecha, 
#              y = factor(CCAA_iso), 
#              # y = CCAA_iso,
#              height = tasa_st_nac,
#              fill = CCAA_iso)) + 
#   geom_ridgeline(color  = gray(.2), alpha = .7,min_height = -2,scale = heights[1]) +
#   geom_vline(xintercept = as_date(c("2020-01-05",
#                                     "2020-06-20",
#                                     "2020-11-30",
#                                     "2021-03-01",
#                                     as.character(today()))),
#              color = "#AAAAAA",
#              linetype = "82") +
#   theme(legend.pos = "none", 
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=14),
#         title = element_text(size = 14)) + 
#   labs(x = "fecha", y = "CCAA", title = paste0("Tasas estandarizadas de ",variable_names[1])) + 
#   scale_fill_manual(values = CCAA_cols, labels = CCAA_isov) + 
#   coord_cartesian(clip = "off")

save(ridge_plots, file = "FigData/ridge_plots.Rdata")
# library(cowplot)
# plot_grid(plotlist = ridge_plots, ncol = 1)
# 
# ridge_plots[[1]]
# ridge_plots[[2]]
# ridge_plots[[3]]
# ridge_plots[[4]]
# ridge_plots[[5]]
plot_grid(plotlist = ridge_plots)

covid_st %>% 
  select(CCAA_iso, fecha, variable, tasa_st_nac) %>% 
  pivot_wider(names_from = variable, values_from = tasa_st_nac) %>% 
  ggplot(aes(x = casos, y = hosp)) +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10()

covid_st %>% 
  select(CCAA_iso, fecha, variable, tasa_st_nac) %>% 
  pivot_wider(names_from = variable, values_from = tasa_st_nac) %>% 
  ggplot(aes(x = casos, y = uci)) +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10()

covid_st %>% 
  select(CCAA_iso, fecha, variable, tasa_st_nac) %>% 
  pivot_wider(names_from = variable, values_from = tasa_st_nac) %>% 
  ggplot(aes(x = casos * 1e5, y = def * 1e5)) +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10()


ggpairs(covid_st)

 

# Check correlations (as scatterplots), distribution and print corrleation coefficient 
A <-
covid_st %>% 
  select(CCAA_iso, fecha, variable, tasa_st_nac) %>% 
  pivot_wider(names_from = variable, values_from = tasa_st_nac) %>% 
  dplyr::filter(casos > 0) %>% 
  group_by(CCAA_iso) %>% 
  summarize(casos_hosp = cor(casos, hosp),
            casos_uci = cor(casos, uci),
            casos_def = cor(casos, def),
            casos_exceso = cor(casos, exceso, 
                               use = "pairwise.complete.obs"),
            hosp_uci = cor(hosp, uci),
            hosp_def = cor(hosp, def),
            hosp_exceso = cor(hosp, exceso, 
                              use = "pairwise.complete.obs"),
            uci_def = cor(uci, def),
            uci_exceso = cor(uci, exceso, 
                             use = "pairwise.complete.obs"),
            def_exceso = cor(def, exceso, 
                             use = "pairwise.complete.obs"))

# Correlations:
Corrs <-
A %>% 
  pivot_longer(casos_hosp:def_exceso, names_to = "vars", values_to = "cor") %>% 
  separate(vars, sep = "_", into = c("var1","var2")) %>% 
 mutate(var1 = factor(var1,levels = c("casos","hosp","uci","def")),
        var2 = factor(var2,levels = c("hosp","uci","def","exceso"))) %>% 
  ggplot(aes(x=var1,y=var2, fill = `cor`)) +
  geom_tile() +
  scale_fill_continuous_diverging()+
  facet_wrap(.~CCAA_iso, ncol = 4)
save(Corrs, file = "FigData/MeasureCorrs.Rdata")  
# st <- 
#   covid %>% 
#   dplyr::filter(fecha == max(fecha), variable %in% c("casos"), CCAA_iso == "PV") %>%
#   group_by(edad) %>% 
#   summarize(pob = sum(pob),.groups = "drop") %>% 
#   mutate(stand = pob / sum(pob))
# 
# 
# 
# chunk <- 
# covid %>% 
#   dplyr::filter(fecha == max(fecha), variable %in% c("casos","def"), 
#                 CCAA_iso == "PV") %>% 
#   select(sexo, edad, variable, tasa_cumul) %>% 
#   pivot_wider(names_from = "variable", values_from = "tasa_cumul") %>% 
#   left_join(st) %>% 
#   mutate(ascfr = def / casos) 
# 
# MF <- 
# chunk %>% 
#   group_by(sexo) %>% 
#   summarize(SCFR = sum(ascfr) * 10,
#             SCFR2 = sum(ascfr * ifelse(edad == 80,20,10)),
#             sCFR = sum(ascfr * stand),
#             CFR = sum(casos) / sum(def))
# slice(MF[2:5],1) / slice(MF[2:5],2)
# 
# 
# chunk %>% 
#   select(sexo, edad, ascfr) %>% 
#   pivot_wider(names_from = sexo, values_from = ascfr) %>% 
#   mutate(sr = H / M)
# 
# M <- chunk %>% 
#   dplyr::filter(sexo == "M")
# 
# 
# chunk2 <- M %>% 
#   mutate(sexo = "H",
#          ascfr  = ascfr * 2,
#          def = ascfr * casos) %>% 
#   bind_rows(M)
# 
# MF2 <- 
#   chunk2 %>% 
#   group_by(sexo) %>% 
#   summarize(SCFR = sum(ascfr) * 10,
#             SCFR2 = sum(ascfr * ifelse(edad == 80,20,10)),
#             sCFR = sum(ascfr * stand),
#             CFR = sum(casos) / sum(def))
# slice(MF2[2:5],1) / slice(MF2[2:5],2)


