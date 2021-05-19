library(tidyverse)

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
  dplyr::filter(variable == "hosp") %>% 
  ggplot(aes(x = fecha, y = tasa_cumul * 100, color = CCAA_iso)) + 
  geom_line() +
  facet_grid(~edad, scale = "free_y") 

both_sex_subset %>% 
  ggplot(aes())

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
  ggplot(aes(x = fecha, y = tasa)) +
  geom_line()

library(ggridges)

covid_st %>% 
  dplyr::filter(variable == "casos")  %>% 
  ggplot(aes(x = fecha, y = CCAA_iso, height = tasa_st_nac * 4e2)) + 
  geom_ridgeline() +
  geom_vline(xintercept = as_date("2020-03-25"))



