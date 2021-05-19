
library(httr)
library(rvest)
library(tidyverse)
library(readr)
library(lubridate)


redistribute_NC_edad <- function(chunk){
  if (sum(chunk$value) == 0){
    chunk %>% dplyr::filter(edad != "NC") %>% return()
  }
  NC <- chunk %>% dplyr::filter(edad == "NC") %>% dplyr::pull(value)
  chunk <- chunk %>% 
    dplyr::filter(edad != "NC") %>% 
    mutate(p = value / sum(value),
           p = ifelse(is.nan(p), 0, p),
           value = value + p * NC) %>% 
    select(-p)
  chunk
}
redistribute_NC_prov <- function(chunk){
  if (sum(chunk$value) == 0){
    chunk %>% dplyr::filter(provincia_iso != "NC") %>% return()
  }
  NC <- chunk %>% dplyr::filter(provincia_iso == "NC") %>% dplyr::pull(value)
  chunk <- chunk %>% 
    dplyr::filter(provincia_iso != "NC") %>% 
    mutate(p = value / sum(value),
           p = ifelse(is.nan(p), 0, p),
           value = value + p * NC) %>% 
    select(-p)
  chunk
}

url <- "https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv"

download.file(url, destfile = "Data/isciii.csv", mode = "wb")

A <- read_csv("Data/isciii.csv") %>% 
  mutate(provincia_iso = case_when(is.na(provincia_iso)~"NA",
                                   TRUE ~ provincia_iso),
         year_iso = isoyear(fecha),
         week_iso = isoweek(fecha)) %>% 
  group_by(provincia_iso, year_iso, week_iso, sexo, grupo_edad) %>% 
  summarize(fecha = fecha[weekdays(fecha) == "Monday"],
            casos = sum(num_casos),
            hosp = sum(num_hosp),
            uci = sum(num_uci),
            def = sum(num_def),
            .groups = "drop") %>% 
  mutate(edad = 
           case_when(
             grupo_edad == "0-9"   ~ "0" ,
             grupo_edad == "10-19" ~ "10",
             grupo_edad == "20-29" ~ "20",
             grupo_edad == "30-39" ~ "30",
             grupo_edad == "40-49" ~ "40",
             grupo_edad == "50-59" ~ "50",
             grupo_edad == "60-69" ~ "60",
             grupo_edad == "70-79" ~ "70",
             grupo_edad == "80+"   ~ "80",
             grupo_edad == "NC"    ~ "NC" )) %>% 
  select(-grupo_edad) %>% 
  dplyr::filter(!provincia_iso %in% c("CE","ML")) %>% 
  mutate(provincia_iso = stringr::str_remove_all(provincia_iso, "[^A-z|0-9|[:punct:]|\\s]"))

# redistribute NC sexo, edad, prov
B <-
  A %>% 
  pivot_longer(casos:def, names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = "sexo", values_from = "value") %>% 
  mutate(pm = ifelse(H + M == 0, 0, H / (H + M)),
         pf = ifelse(H + M == 0, 0, M / (H + M)),
         H = H + pm * NC,
         M = M + pf * NC) %>% 
  select(-pm, -pf, -NC) %>% 
  pivot_longer(H:M, values_to = "value", names_to = "sexo") %>% 
  group_by(provincia_iso, year_iso, week_iso, fecha, sexo, variable) %>% 
  do(redistribute_NC_edad(chunk = .data)) %>% 
  ungroup() %>% 
  group_by(year_iso, week_iso, fecha, sexo, edad, variable) %>% 
  do(redistribute_NC_prov(chunk = .data)) %>% 
  ungroup()

  


# CCAA <- read_html("https://www.ine.es/daco/daco42/codmun/cod_ccaa_provincia.htm") %>% 
#   html_table() %>% 
#   '[['(1) 
# CCAA %>% 
#   mutate(CPRO = sprintf("%02d", CPRO)) %>% 
#   select(CPRO,
#          CCAA = `Comunidad Autónoma`)

#CCAA %>% View()
#A$provincia_iso


# this relates province iso2 to ccaa iso2
prov_codes <- 
  read_html("https://es.wikipedia.org/wiki/ISO_3166-2:ES", encoding = "latin") %>% 
  html_table() %>% 
  '[['(2) %>% 
  mutate(provincia_iso = `Código`,
         provincia_iso = gsub(provincia_iso, pattern = "ES-", replacement = ""),
         provincia_iso = gsub(provincia_iso, pattern = "\\[nota 3\\]", replacement = "")) %>%
  select(CCAA_iso = `Comunidad autónoma`, provincia_iso) %>% 
  mutate(provincia_iso = stringr::str_remove_all(provincia_iso, "[^A-z|0-9|[:punct:]|\\s]"))

# relates province name, iso2, and postal code
# need postal code, since this is how provinces are coded in population data...
CPRO <-
  read_html("https://es.wikipedia.org/wiki/Anexo:Provincias_de_Espa%C3%B1a_por_c%C3%B3digo_postal") %>% 
  html_table() %>% 
  '[['(1) %>% 
  mutate(provincia_iso = `Código Ministerio del Interior`,
         provincia_iso = case_when(is.na(provincia_iso)~"NA",
                                   provincia_iso == "OR / OU" ~ "OR",
                                   provincia_iso == "PM / IB" ~ "PM",
                                   provincia_iso == "GE / GI" ~ "GI",
                                   TRUE~provincia_iso)) %>% 
  select(Provincia, provincia_iso, CPRO = `Código postal`) %>% 
  mutate(CPRO = sprintf("%02d", CPRO),
         CPRO = stringi::stri_enc_toascii(CPRO),
         provincia_iso = stringr::str_remove_all(provincia_iso, "[^A-z|0-9|[:punct:]|\\s]"))

# add all codes to covid data by province
CC <-
  B %>% 
  left_join(prov_codes, by = "provincia_iso") %>% 
  left_join(CPRO, by = "provincia_iso")

#
# For population, get INE series 31304 (https://www.ine.es/jaxiT3/Tabla.htm?t=31304)
P <- read_delim("Data/31304.csv", delim = ";") %>% 
  mutate(Total = gsub(Total, pattern = "\\.", replacement = ""),
         Total = as.integer(Total)) %>% 
  dplyr::filter(Edad !="Total",
                Provincias != "Total Nacional") %>% 
  separate(Edad, sep = " ", into= c("Age",NA)) %>% 
  mutate(Age = as.integer(Age)) %>% 
  separate(Provincias, into = c("CPRO","Provincia")) %>% 
  dplyr::filter(Periodo == "1 de julio de 2020",
                Sexo %in% c("Hombres", "Mujeres")) %>% 
  mutate(sexo = case_when(Sexo == "Hombres" ~ "H",
                          Sexo == "Mujeres" ~ "M"),
         edad = Age - Age %% 10,
         edad = ifelse(edad > 80, 80, edad)) %>% 
  group_by(sexo, edad, CPRO) %>% 
  summarize(pob = sum(Total), .groups = "drop") %>% 
  mutate(edad = as.character(edad))

R <- left_join(CC, P, by = c("edad","sexo","CPRO"))

S <- 
  R %>% 
  group_by(CCAA_iso, year_iso, week_iso, fecha, sexo, edad, variable) %>% 
  summarise(value = sum(value),
            pob = sum(pob),
            .groups = "drop")
# Basque Country standard population (age * sex)
stand_pv <-
  S %>% 
  dplyr::filter(fecha == min(fecha),
                CCAA_iso == "PV",
                variable == "casos") %>% 
  mutate(stand_pv = pob / sum(pob)) %>% 
  select(sexo, edad, stand_pv)

# Spain nation standard population (age * sex)
stand_nac <-
  S %>% 
  group_by(sexo, edad) %>% 
  summarize(pob = sum(pob), .groups = "drop") %>% 
  mutate(stand_nac = pob / sum(pob)) %>% 
  select(sexo, edad, stand_nac)

out <-
  S %>% 
  left_join(stand_nac) %>% 
  left_join(stand_pv) %>% 
  arrange(CCAA_iso, variable, edad, fecha) %>% 
  group_by(CCAA_iso, variable, edad) %>% 
  mutate(tasa = value / pob,
         tasa_cumul = cumsum(tasa)) %>% 
  ungroup() 


saveRDS(out, file = "Data/iscii_ccaa.rds")


