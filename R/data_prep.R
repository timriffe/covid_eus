
library(httr)
library(rvest)
library(tidyverse)
library(readr)
library(lubridate)
library(eurostat)

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

# gets denominators
download.file("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/31304.csv?nocab=1", destfile = "Data/31304.csv")
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
            .groups = "drop") %>% 
  mutate(edad = as.integer(edad))
# Basque Country standard population (age * sex)
 stand_pv <-
   S %>% 
   dplyr::filter(fecha == min(fecha),
                 CCAA_iso == "PV",
                 variable == "casos") %>% 
   mutate(stand_pv = pob / sum(pob)) %>% 
   select(sexo, edad, stand_pv)
# 
# # Spain nation standard population (age * sex)
 stand_nac <-
   S %>% 
   group_by(sexo, edad) %>% 
   summarize(pob = sum(pob), .groups = "drop") %>% 
   mutate(stand_nac = pob / sum(pob)) %>% 
   select(sexo, edad, stand_nac)

U <-
  S %>% 
  arrange(CCAA_iso, variable, edad, fecha) %>% 
  group_by(CCAA_iso, variable, edad) %>% 
  mutate(tasa = value / pob) %>% 
  left_join(stand_pv, by = c("edad","sexo")) %>% 
  left_join(stand_nac, by = c("edad","sexo")) %>% 
  ungroup() 


# saveRDS(out, file = "Data/iscii_ccaa.rds")

#-----------------------------------------------------------------------#
# INE, just do a simple excess calc based on mean deaths in prior years #
# use original 5-year age groups                                        #
#-----------------------------------------------------------------------#

# https://www.ine.es/jaxiT3/Tabla.htm?t=35179

# helpers:
rescale_age <- function(chunk){
  TOT <- chunk %>% dplyr::filter(edad == "TOT") %>% dplyr::pull(deaths)
  chunk <- chunk %>% dplyr::filter(edad != "TOT")
  chunk %>% mutate(deaths = deaths / sum(deaths) * TOT,
                   edad = as.integer(edad))
}
rescale_sex <- function(chunk){
  TOT <- chunk %>% dplyr::filter(sexo == "T") %>% dplyr::pull(deaths)
  chunk <- chunk %>% dplyr::filter(edad != "T")
  chunk %>% mutate(deaths = deaths / sum(deaths) * TOT)
}

CCAA <- read_html("https://www.ine.es/daco/daco42/codmun/cod_ccaa_provincia.htm") %>% 
  html_table() %>% 
  '[['(1) %>% 
  mutate(CPRO = sprintf("%02d", CPRO)) %>% 
  select(CPRO,
         CCAA = `Comunidad Autónoma`,
         CODCCAA = CODAUTO) %>% 
  left_join(CPRO) %>% 
  left_join(prov_codes) %>% 
  select(CODCCAA, CCAA_iso) %>% 
  distinct() %>% 
  filter(!is.na(CCAA_iso))

download.file("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/35179.csv?nocab=1", destfile = "Data/35179.csv")
D <- read_delim("Data/35179.csv", delim = ";", col_types = "cccccc") %>% 
  mutate(Total = gsub(Total, pattern = "\\.", replacement = "") %>% as.numeric()) %>% 
  rename(edad = `Edad (grupos quinquenales)`) %>% 
  dplyr::filter(`Comunidades autónomas` != "Total Nacional",
                edad != "No consta",
                `Tipo de dato` == "Dato base") %>% 
  mutate(CODCCAA = substr(`Comunidades autónomas`,1,2)) %>% 
  left_join(CCAA) %>% 
  dplyr::filter(!is.na(CCAA_iso)) %>% 
  mutate(sexo = recode(Sexo,
                       "Total" = "T",
                       "Hombres" = "H",
                       "Mujeres" = "M"),
         edad = trimws(edad, "r"),
         edad = recode(edad,
                       "Todas las edades" = "TOT",
                       "De 0 a 4 años" = "0",
                       "De 5 a 9 años" = "5",
                       "De 10 a 14 años" = "10",
                       "De 15 a 19 años" = "15",
                       "De 20 a 24 años" = "20",
                       "De 25 a 29 años" = "25",
                       "De 30 a 34 años" = "30",
                       "De 35 a 39 años" = "35",
                       "De 40 a 44 años" = "40",
                       "De 45 a 49 años" = "45",
                       "De 50 a 54 años" = "50",
                       "De 55 a 59 años" = "55",
                       "De 60 a 64 años" = "60",
                       "De 65 a 69 años" = "65",
                       "De 70 a 74 años" = "70",
                       "De 75 a 79 años" = "75",
                       "De 80 a 84 años" = "80",
                       "De 85 a 89 años" = "85",
                       "90 y más años" = "90")) %>% 
  separate(Periodo, sep = "SM", into = c("year_iso", "week_iso"), convert = TRUE) %>% 
  select(CCAA_iso, year_iso, week_iso, sexo, edad, deaths = Total) %>% 
  group_by(CCAA_iso, year_iso, week_iso, sexo) %>% 
  do(rescale_age(chunk = .data)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "sexo", values_from = "deaths") %>% 
  mutate(HM = M + H,
         PM = M / HM,
         PH = H / HM,
         PM = ifelse(is.nan(PM),0,PM),
         PH = ifelse(is.nan(PH),0,PH),
         H = PH * `T`,
         M = PM * `T`) %>% 
  select(-PH, -PM, -HM) %>% 
  pivot_longer(H:`T`, values_to = "deaths", names_to = "sexo")

# get earlier years for baseline
DD <- get_eurostat("demo_r_mwk2_05") %>% 
  dplyr::filter(grepl(geo, pattern = "ES")) %>% 
  mutate(time = as.character(time),
         CCAA_code = gsub(geo,pattern = "ES", replacement = "")) %>% 
  dplyr::filter(nchar(CCAA_code) == 2) %>% 
  mutate(CCAA_iso = recode(CCAA_code,
                           "11" = "GA",
                           "12" = "AS",
                           "13" = "CB",
                           "21" = "PV",
                           "22" = "NC",
                           "23" = "RI",
                           "24" = "AR",
                           "30" = "MD",
                           "41" = "CL",
                           "42" = "CM",
                           "43" = "EX",
                           "51" = "CT",
                           "52" = "VC",
                           "53" = "IB",
                           "61" = "AN",
                           "62" = "MC",
                           "63" = "toss",
                           "64" = "toss",
                           "70" = "CN"
                           ),
         sexo = recode(sex,
                       "F" = "M",
                       "M" = "H",
                       "T" = "T")) %>% 
  dplyr::filter(CCAA_iso != "toss") %>% 
  separate(time, sep = "W", into = c("year_iso","week_iso"), convert = TRUE) %>% 
  dplyr::filter(year_iso >= 2016, 
                year_iso < 2020, 
                age != "UNK") %>% 
  mutate(edad = recode(age,
                      "TOTAL" = "TOT",
                      "Y_GE90" = "90",
                      "Y_LT5" = "0",
                      "Y5-9" = "5",
                      "Y10-14" = "10",
                      "Y15-19" = "15",
                      "Y20-24" = "20",
                      "Y25-29" = "25",
                      "Y30-34" = "30",
                      "Y35-39" = "35",
                      "Y40-44" = "40",
                      "Y45-49" = "45",
                      "Y50-54" = "50",
                      "Y55-59" = "55",
                      "Y60-64" = "60",
                      "Y65-69" = "65",
                      "Y70-74" = "70",
                      "Y75-79" = "75",
                      "Y80-84" = "80",
                      "Y85-89" = "85")) %>% 
  select(CCAA_iso, year_iso, week_iso, sexo, edad, deaths = values) %>% 
  group_by(CCAA_iso, year_iso, week_iso, sexo) %>% 
  do(rescale_age(chunk = .data)) %>% 
  ungroup() 
  
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
           edad = Age - Age %% 5,
           edad = ifelse(edad > 90, 90, edad)) %>% 
    group_by(sexo, edad, CPRO) %>% 
    summarize(pob = sum(Total), .groups = "drop") %>% 
    left_join(CPRO) %>% 
    left_join(prov_codes) %>% 
    dplyr::filter(!is.na(CCAA_iso)) %>% 
    group_by(CCAA_iso, sexo, edad) %>% 
    summarize(pob = sum(pob), .groups = "drop") %>% 
    arrange(CCAA_iso, sexo, edad)

  excess_deaths <- DD %>% 
    group_by(CCAA_iso, week_iso, sexo, edad) %>% 
    summarize(baseline = mean(deaths),.groups = "drop") %>% 
    right_join(D, by = c("CCAA_iso", "week_iso", "edad", "sexo")) %>% 
    dplyr::filter(year_iso > 2019,
                  sexo != "T") %>% 
    mutate(value = deaths - baseline) %>% 
    left_join(P, by = c("CCAA_iso","sexo","edad")) %>% 
    mutate(variable = "exceso",
           tasa = value / pob)
  
  
  stand_pv <-
    P %>% 
    dplyr::filter(CCAA_iso == "PV") %>% 
    mutate(stand_pv = pob / sum(pob)) %>% 
    select(sexo, edad, stand_pv)
  
  # Spain nation standard population (age * sex)
  stand_nac <-
    P %>% 
    group_by(sexo, edad) %>% 
    summarize(pob = sum(pob), .groups = "drop") %>% 
    mutate(stand_nac = pob / sum(pob)) %>% 
    select(sexo, edad, stand_nac)
  
  
# left off here ----------------
  # ---------------------------------------------
  excess <-
    U %>% 
    select(year_iso, week_iso, fecha) %>% 
    distinct() %>% 
    right_join(excess_deaths, by = c("year_iso", "week_iso")) %>% 
    dplyr::filter(!is.na(fecha)) %>% 
    select(-baseline, -deaths) 
  
  out <-
    excess %>% 
    left_join(stand_nac, by = c("edad", "sexo")) %>% 
    left_join(stand_pv, by = c("edad","sexo")) %>% 
    bind_rows(U)
  
  
  saveRDS(out, file = "Data/data_ccaa.rds") 
  
  # excess %>% 
  #   mutate(tasa = value / pob) %>% 
  #   group_by(CCAA_iso, year_iso, week_iso, fecha) %>% 
  #   summarize(deaths_st_pv = sum(tasa * stand_pv),
  #             deaths_st_nac = sum(tasa * stand_nac),
  #             .groups = "drop") %>% 
  #   ggplot(aes(x = fecha, y = deaths_st_nac, group = CCAA_iso)) + 
  #   geom_line()
  # 
  
  
# CCAA2 <- read_html("https://www.ine.es/daco/daco42/codmun/cod_ccaa_provincia.htm") %>% 
  #   html_table() %>% 
  #   '[['(1) %>% 
  #   mutate(CPRO = sprintf("%02d", CPRO)) %>% 
  #   select(CPRO,
  #          CCAA = `Comunidad Autónoma`,
  #          CODCCAA = CODAUTO) %>% 
  #   left_join(CPRO) %>% 
  #   left_join(prov_codes) %>% 
  #   select(CODCCAA, CCAA_iso, CCAA) %>% 
  #   distinct() %>% 
  #   filter(!is.na(CCAA_iso))
    